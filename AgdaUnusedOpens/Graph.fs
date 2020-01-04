namespace AgdaUnusedOpens

open System.Collections.Generic
open System.Diagnostics
open System.IO
open AgdaUnusedOpens.Types
open AgdaUnusedOpens.Internals

type Graph =
    {
        Node : Path
        Unblocks : Graph Set
    }

type ParseError =
    | NotDotFile of Path * string
    | FailedToCompile of Path

[<RequireQualifiedAccess>]
type ParseResult =
    | Error of ParseError list
    | Success of Graph

[<RequireQualifiedAccess>]
module Graph =

    /// A graph that is under construction.
    type private Construct =
        {
            /// The label of this node.
            Node : Path
            /// The outgoing edges, together with the nodes at their ends
            To : Map<Path, Construct>

            HasIncoming : bool ref
        }

    /// A map of path to which constructed graph that path is the root of.
    type private Constructing = Constructing of Map<Path, Construct>

    let private addEdge (from : Path) (toNode : Path) (Constructing constructing) =
        let imageNode, constructing =
            match Map.tryFind toNode constructing with
            | None ->
                let node =
                    { Node = toNode ; To = Map.empty ; HasIncoming = ref true }
                node, constructing |> Map.add toNode node
            | Some node ->
                node.HasIncoming := true
                node, constructing
        match Map.tryFind from constructing with
        | None ->
            constructing
            |> Map.add from { Node = from ; To = Map.empty |> Map.add toNode imageNode ; HasIncoming = ref false }
        | Some existingGraph ->
            let newFrom = { existingGraph with To = existingGraph.To |> Map.add toNode imageNode }

            constructing
            |> Map.add from newFrom
        |> Constructing

    let private addEdges (from : Path) (toNode : Path Set) (constructing : Constructing) =
        if Path.toString from = "Setoids.Setoids" then
            ()
        match Set.toList toNode with
        | [] ->
            let (Constructing c) = constructing
            match Map.tryFind from c with
            | None ->
                c
                |> Map.add from { Node = from ; To = Map.empty ; HasIncoming = ref false }
            | Some _ -> c
            |> Constructing
        | l ->
            l
            |> List.fold (fun state s -> addEdge from s state) constructing

    let private concretise (Constructing overallConstruction) : Graph =
        let memo = Dictionary<Path, Graph> ()
        let rec go (c : Construct) : Graph =
            match memo.TryGetValue c.Node with
            | true, v -> v
            | false, _ ->
                let u =
                    {
                        Node = c.Node
                        Unblocks =
                            c.To
                            |> Seq.map (fun kvp -> Map.find kvp.Key overallConstruction)
                            |> Seq.map go
                            |> Set.ofSeq
                    }
                memo.Add (c.Node, u)
                u

        let topNode =
            overallConstruction
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun v -> v.HasIncoming.Value = false)
            |> Seq.toList
        match topNode with
        | [] -> failwith "Circular dependency detected"
        | x :: y :: _ -> failwithf "Multiple nodes are at the root! %i many, at least '%s', '%s' (%+A)" (List.length topNode) (Path.toString x.Node) (Path.toString y.Node) (topNode |> List.map (fun i -> Path.toString i.Node))
        | [v] ->
            go v

    let internal toGraph (AdjacencyList g) : Graph =
        // If there are no outgoing nodes, the node will not appear as a key in this map.
        let reduced : Map<Path, Path Set> =
            g
            |> List.fold (fun m { From = from ; To = toPath } ->
                match Map.tryFind from m with
                | Some xs -> Map.add from (Set.add toPath xs) m
                | None -> Map.add from (Set.singleton toPath) m
                ) Map.empty

        let constructed =
            reduced
            |> Map.fold (fun inConstruction from toPaths ->
                addEdges from toPaths inConstruction
                ) (Constructing Map.empty)

        constructed
        |> concretise

    let parse (dot : string) : Result<Graph, string> =
        AdjacencyList.parse dot
        |> Result.map toGraph

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

    let cata<'a> (f : Path -> 'a list -> 'a) (g : Graph) : 'a =
        let store = Dictionary<Path, 'a> ()

        let rec go (g : Graph) =
            match store.TryGetValue g.Node with
            | false, _ ->
                let u =
                    g.Unblocks
                    |> Set.toList
                    |> List.map go
                    |> f g.Node
                store.Add (g.Node, u)
                u
            | true, v -> v

        go g
