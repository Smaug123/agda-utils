namespace AgdaUnusedOpens

open AgdaUnusedOpens.Internals
open System.IO
open System.Text.RegularExpressions
open AgdaUnusedOpens.Types

[<RequireQualifiedAccess>]
module Path =
    let fold<'s> (f : 's -> string -> 's) (initial : 's) (p : Path) : 's =
        p.Tail
        |> List.fold f (f initial p.Head)

    let toString (p : Path) : string =
        match p.Tail with
        | [] ->
            p.Head
        | _ ->
            sprintf "%s.%s" p.Head (String.concat "." p.Tail)

    let combine (agdaRoot : DirectoryInfo) (p : Path) : FileInfo =
        p
        |> fold (fun soFar next -> Path.Combine (soFar, next)) agdaRoot.FullName
        |> sprintf "%s.agda"
        |> FileInfo

    let rename (newModule : string) (p : Path) =
        match p.Tail with
        | [] -> { Head = newModule ; Tail = [] }
        | t -> { Head = p.Head ; Tail = t |> List.rev |> List.tail |> (fun i -> newModule :: i) |> List.rev }

    let make (p : string list) =
        match p with
        | [] -> failwith "You fool"
        | x :: xs ->
            {
                Head = x
                Tail = xs
            }

/// An in-memory representation of an Agda file.
type AgdaFile =
    internal
        {
            Path : Path
            Contents : string list
            /// Line number of the line which contains the module statement
            ModuleLine : int
        }

[<RequireQualifiedAccess>]
module AgdaFile =
    let make (lines : string list) : AgdaFile =
        let moduleLineNo, moduleLine =
            lines
            |> List.indexed
            |> List.filter (fun (_, line) -> line.TrimStart().StartsWith "module ")
            |> List.exactlyOne
        let pathRegex = Regex ".*module ([^ ]+) .*where"
        let path =
            (pathRegex.Match moduleLine).Groups.[1].Value.Split('.')
            |> Array.toList
            |> Path.make

        {
            Path = path
            Contents = lines
            ModuleLine = moduleLineNo
        }

    let flush (agdaRoot : DirectoryInfo) (f : AgdaFile) : unit =
        let location = Path.combine agdaRoot f.Path
        File.WriteAllLines (location.FullName, f.Contents)

    let rename (newName : string) (f : AgdaFile) : AgdaFile =
        let newPath = Path.rename newName f.Path
        {
            Path = newPath
            Contents =
                let currentLine = f.Contents.[f.ModuleLine]
                let newLine =
                    let i = currentLine.IndexOf "module " + String.length "module "
                    sprintf "%s%s%s" (currentLine.Substring(0, i)) (Path.toString newPath) (currentLine.Substring(currentLine.IndexOf(' ', i)))
                f.Contents
                |> List.replaceAt f.ModuleLine newLine
            ModuleLine = f.ModuleLine
        }

