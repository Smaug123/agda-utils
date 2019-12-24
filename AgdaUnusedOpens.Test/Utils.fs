namespace AgdaUnusedOpens.Test

open System.IO
open System.Text.RegularExpressions

type Dummy = Dummy

[<RequireQualifiedAccess>]
module Utils =

    /// E.g. getResource "Example.dot" for the .dot embedded resource
    let getResource (filename : string) : string =
        let assembly = typeof<Dummy>.Assembly
        let resource = assembly.GetManifestResourceStream(sprintf "AgdaUnusedOpens.Test.%s" filename)
        use tr = new StreamReader(resource)
        tr.ReadToEnd ()

    let getResource' (filename : string) : string list =
        let assembly = typeof<Dummy>.Assembly
        let resource = assembly.GetManifestResourceStream(sprintf "AgdaUnusedOpens.Test.%s" filename)
        use tr = new StreamReader(resource)
        tr.ReadToEnd ()
        |> fun i -> Regex.Split(i, "\r\n|\r|\n")
        |> Array.toList

