namespace AgdaUnusedOpens

open System.Diagnostics
open System.IO

type AgdaCompiler =
    {
        Compiler : FileInfo
        /// The directory which your Agda modules are defined relative to
        AgdaRoot : DirectoryInfo
    }

[<RequireQualifiedAccess>]
module AgdaCompiler =

    let private compile (config : AgdaCompiler) (f : AgdaFile) : int =
        use proc = new Process ()
        proc.StartInfo.FileName <- config.Compiler.FullName
        proc.StartInfo.Arguments <- sprintf "%s" (Path.combine config.AgdaRoot f.Path).FullName
        proc.StartInfo.WorkingDirectory <- config.AgdaRoot.FullName
        proc.StartInfo.UseShellExecute <- false
        proc.StartInfo.RedirectStandardOutput <- true
        proc.StartInfo.RedirectStandardError <- true
        if proc.Start () then
            proc.StandardOutput.ReadToEnd () |> ignore
            proc.StandardError.ReadToEnd () |> ignore
            proc.WaitForExit ()
        proc.ExitCode

    let compiles (config : AgdaCompiler) (f : AgdaFile) : bool =
        // Write it out with a temporary name
        let tempName = "temporaryModuleName"
        let newFile = AgdaFile.rename tempName f
        AgdaFile.flush config.AgdaRoot newFile

        let exitCode = compile config newFile

        // Delete the temporary file
        (Path.combine config.AgdaRoot newFile.Path).Delete()
        exitCode = 0
