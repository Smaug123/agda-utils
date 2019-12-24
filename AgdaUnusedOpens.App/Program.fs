open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code

    // TODO - get the complete dependency graph by getting all the agda files
    // Then from the leaves inwards, get all their open statements;
    // try compiling without each one.
    // Finally write out the file with unnecessary open statements removed.