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

type ParseError =
    | NotDotFile of Path * string
    | FailedToCompile of Path

[<RequireQualifiedAccess>]
type ParseResult =
    | Error of ParseError list
    | Success of Graph

[<RequireQualifiedAccess>]
module Graph =

    let merge (Graph g1) (Graph g2) : Graph =
        g1 @ g2
        |> List.distinct
        |> Graph

    [<RequireQualifiedAccess>]
    module internal ParseResult =
        let merge (res1 : ParseResult) (res2 : ParseResult) =
            match res1, res2 with
            | ParseResult.Error e, ParseResult.Error f -> ParseResult.Error (e @ f)
            | ParseResult.Error e, _ -> ParseResult.Error e
            | _, ParseResult.Error f -> ParseResult.Error f
            | ParseResult.Success g, ParseResult.Success h -> merge g h |> ParseResult.Success

    /// Given the contents of a dotfile, parse it.
    let parse (dot : string) : Result<Graph, string> =
        let dot = dot.Trim ()
        if not <| dot.StartsWith "digraph dependencies {" then
            Error <| sprintf "Unexpected start of string: %s" dot
        else
        if not <| dot.EndsWith "}" then
            Error (sprintf "File did not end with a }: %s." dot)
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
        |> Ok

    let load (agda : AgdaCompiler) (f : AgdaFile) : ParseResult =
        let tmp = Path.GetTempFileName ()
        use proc = new Process ()
        proc.StartInfo.FileName <- agda.Compiler.FullName
        proc.StartInfo.WorkingDirectory <- agda.AgdaRoot.FullName
        proc.StartInfo.Arguments <- sprintf "%s --dependency-graph=%s" (Path.combine agda.AgdaRoot f.Path).FullName tmp
        let res = proc.Start ()
        assert (res = true)
        proc.WaitForExit ()
        if proc.ExitCode = 0 then
            match tmp |> File.ReadAllText |> parse with
            | Ok g -> ParseResult.Success g
            | Error err -> ParseResult.Error (ParseError.NotDotFile (f.Path, err) |> List.singleton)
        else
            ParseResult.Error (ParseError.FailedToCompile f.Path |> List.singleton)
