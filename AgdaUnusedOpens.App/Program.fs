open AgdaUnusedOpens
open System.IO

let rec unusedOpens (config : AgdaCompiler) (opens : OpenStatement list) (file : AgdaFile) : OpenStatement seq =
    seq {
        for o in opens do
            let f = OpenStatement.remove file o
            if AgdaCompiler.compiles config f then
                yield o
    }

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

    let graph = Graph.load config (AgdaFile.makeFromFile everything)
    printfn "%+A" graph

    let f =
        File.ReadAllLines "/Users/Patrick/Desktop/Agda/Numbers/Naturals/EuclideanAlgorithm.agda"
        |> Array.toList
        |> AgdaFile.make
    let opens = OpenStatement.get f

    printfn "%+A" (unusedOpens config opens f |> Seq.map (fun i -> i.LineNumber) |> Seq.toList)
    0
