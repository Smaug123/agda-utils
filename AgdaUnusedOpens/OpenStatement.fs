namespace AgdaUnusedOpens

open AgdaUnusedOpens.Internals

type OpenStatement =
    {
        File : AgdaFile
        LineNumber : int
    }

[<RequireQualifiedAccess>]
module OpenStatement =
    let get (file : AgdaFile) : OpenStatement list =
        file.Contents
        |> List.indexed
        |> List.choose (fun (i, line) -> if line.TrimStart().StartsWith("open import ") then Some i else None)
        |> List.map (fun i -> { File = file ; LineNumber = i })

    let remove (f : AgdaFile) (s : OpenStatement) : AgdaFile =
        {
            Path = f.Path
            Contents = f.Contents |> Seq.omit s.LineNumber |> Seq.toList
            ModuleLine =
                if f.ModuleLine < s.LineNumber then f.ModuleLine
                elif f.ModuleLine = s.LineNumber then failwith "This is unexpected"
                else f.ModuleLine - 1
        }

