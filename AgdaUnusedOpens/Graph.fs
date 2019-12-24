namespace AgdaUnusedOpens

open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open AgdaUnusedOpens.Types

type Arrow =
    {
        From : Path
        To : Path
    }

type Graph = Graph of Arrow list

type ParseResult =
    | NotDotFile of string
    | Success of Graph

[<RequireQualifiedAccess>]
module Graph =

    /// Given the contents of a dotfile, parse it.
    let parse (dot : string) : ParseResult =
        let dot = dot.Trim ()
        if not <| dot.StartsWith "digraph dependencies {" then
            NotDotFile <| sprintf "Unexpected start of string: %s" dot
        else
        if not <| dot.EndsWith "}" then
            NotDotFile (sprintf "File did not end with a }: %s." dot)
        else
        let dot = dot.Substring(String.length "digraph dependencies {").TrimStart ()
        let dot = dot.Remove(String.length dot - 1).TrimEnd ()
        let lines = dot.Split ';' |> Array.map (fun i -> i.Trim ())
        let nodeDef, arrowDef =
            lines
            |> List.ofArray
            |> List.fold (fun (nD : string list, aD : string list) i ->
                if i.Contains "]" then (i :: nD, aD) elif i.Contains " -> " then (nD, i :: aD) else (nD, aD)) ([], [])
        let nodeRegex = Regex @"(.+)\[label=""(.+)""\]"
        let arrowRegex = Regex @"(.+) -> (.+)"
        let nodes : Map<string, Path> =
            nodeDef
            |> List.fold (fun m i ->
                let matches = nodeRegex.Match i
                let label = matches.Groups.[1].Value
                let name =
                    matches.Groups.[2].Value.Split('.')
                    |> List.ofArray
                    |> Path.make
                Map.add label name m) Map.empty
        let arrows : List<string * string> =
            arrowDef
            |> List.map (fun i ->
                let matches = arrowRegex.Match i
                matches.Groups.[1].Value, matches.Groups.[2].Value)

        arrows
        |> List.map (fun (from, to') ->
            {
                From = Map.find from nodes
                To = Map.find to' nodes
            })
        |> Graph
        |> Success

    let load (agda : AgdaCompiler) (f : AgdaFile) : ParseResult =
        let tmp = Path.GetTempFileName ()
        use proc = new Process ()
        proc.StartInfo.FileName <- agda.Compiler.FullName
        proc.StartInfo.Arguments <- sprintf "%s --dependency-graph=%s" (Path.combine agda.AgdaRoot f.Path).FullName tmp
        let res = proc.Start ()
        assert (res = true)
        proc.WaitForExit ()
        tmp
        |> File.ReadAllText
        |> parse
