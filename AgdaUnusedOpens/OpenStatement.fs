namespace AgdaUnusedOpens

open AgdaUnusedOpens.Internals

type OpenStatement =
    {
        File : AgdaFile
        LineNumber : int
    }

[<RequireQualifiedAccess>]
module OpenStatement =
    let getAll (file : AgdaFile) : OpenStatement list =
        file.Contents
        |> List.indexed
        |> List.choose (fun (i, line) -> if line.TrimStart().StartsWith("open import ") then Some i else None)
        |> List.map (fun i -> { File = file ; LineNumber = i })

    /// Private version allowing us to remove multiple open statements from a file.
    /// This is unsafe in general - make sure removing lines from the right files,
    /// and you're not doing it in a bad order (e.g. removing the first line and thereby
    /// causing all the other lines to bump up one before you remove a later line)!
    let private remove' (f : AgdaFile) (s : OpenStatement) : AgdaFile =
        {
            Path = f.Path
            Contents = f.Contents |> Seq.omit s.LineNumber |> Seq.toList
            ModuleLine =
                if f.ModuleLine < s.LineNumber then f.ModuleLine
                elif f.ModuleLine = s.LineNumber then failwith "This is unexpected"
                else f.ModuleLine - 1
        }

    let remove (s : OpenStatement) : AgdaFile =
        remove' s.File s

    /// Returns None iff there were no elements in the list.
    /// Throws if the open statements are not all from the same file.
    let removeAll (s : OpenStatement list) : AgdaFile option =
        match s with
        | [] -> None
        | x :: xs ->
            xs
            |> List.sortBy (fun i -> -i.LineNumber)
            |> List.fold (fun (currFile : AgdaFile) newToRemove ->
                if newToRemove.File.Path <> currFile.Path then
                    failwithf
                        "Attempted to remove open statement in %s from file %s"
                        (Path.toString newToRemove.File.Path)
                        (Path.toString currFile.Path)
                remove' currFile newToRemove) (remove x)
            |> Some

    let rec selectUnused (config : AgdaCompiler) (opens : OpenStatement list) : OpenStatement seq =
        seq {
            for o in opens do
                let f = remove o
                if AgdaCompiler.compiles config f then
                    yield o
        }

