namespace AgdaUnusedOpens.Internals

open System.Text.RegularExpressions
open AgdaUnusedOpens
open AgdaUnusedOpens.Types

type Arrow =
    {
        From : Path
        To : Path
    }

type internal AdjacencyList = AdjacencyList of Arrow list

[<RequireQualifiedAccess>]
module AdjacencyList =

    /// Given the contents of a dotfile, parse it.
    let internal parse (dot : string) : Result<AdjacencyList, string> =
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
        let arrows : (string * string) list =
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
        |> AdjacencyList
        |> Ok

