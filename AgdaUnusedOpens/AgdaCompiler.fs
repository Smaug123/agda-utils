namespace AgdaUnusedOpens

open System.IO

type AgdaCompiler =
    {
        Compiler : FileInfo
        /// The directory which your Agda modules are defined relative to
        AgdaRoot : DirectoryInfo
    }

[<RequireQualifiedAccess>]
module AgdaCompiler =

    let compiles (config : AgdaCompiler) (f : AgdaFile) : bool =
        // Write it out with a different, temporary name, and then run the compiler.
        // Delete the temporary file afterwards.
        let tempName = failwith ""
        let newFile = AgdaFile.rename tempName f
        AgdaFile.flush config.AgdaRoot newFile
        // run the compiler
        (Path.combine config.AgdaRoot newFile.Path).Delete()
        true
