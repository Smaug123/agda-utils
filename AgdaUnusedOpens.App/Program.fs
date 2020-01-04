open AgdaUnusedOpens
open System.IO

[<EntryPoint>]
let main argv =
    // TODO - get the complete dependency graph by getting all the agda files
    // Then from the leaves inwards, get all their open statements;
    // try compiling without each one.
    // Finally write out the file with unnecessary open statements removed.

    let agdaRoot = argv.[0]
    let compiler = argv.[1]

    let config =
        {
            AgdaRoot = DirectoryInfo agdaRoot
            Compiler = FileInfo compiler
        }

    let everything = FileInfo (Path.Join(agdaRoot, "Everything", "Safe.agda"))

    let graph =
        match Graph.load config (AgdaFile.makeFromFile everything) with
        | ParseResult.Success g -> g
        | ParseResult.Error e -> failwithf "%+A" e

    printfn "Loaded dependency graph."

    graph
    |> Graph.cata (fun node _ ->
        if node <> Path.make ["Agda"; "Primitive"] then
            Path.combine config.AgdaRoot node
            |> AgdaFile.makeFromFile
            |> OpenStatement.getAll
            |> OpenStatement.selectUnused config
            |> Seq.toList
            |> OpenStatement.removeAll
            |> Option.map (AgdaFile.flush config.AgdaRoot)
            |> ignore<unit option>)

    0
