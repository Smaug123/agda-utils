namespace AgdaUnusedOpens.Test

open NUnit.Framework
open FsUnitTyped

open AgdaUnusedOpens

[<TestFixture>]
module TestGraph =
    [<Test>]
    let ``Hard-coded example`` () =
        let g =
            "Example.dot"
            |> Utils.getResource
            |> Graph.parse
        match g with
        | Success (Graph g) ->
            let m0 = Path.make ["Sequences"]
            let m1 = Path.make ["Setoids" ; "Setoids"]
            let m2 = Path.make ["LogicalFormulae"]
            let m3 = Path.make ["Agda" ; "Primitive"]
            let m4 = Path.make ["Functions"]
            let m5 = Path.make ["Sets" ; "EquivalenceRelations"]
            let m6 = Path.make ["Numbers" ; "Naturals" ; "Definition"]
            let actual = g |> Set.ofList
            let expected =
                [
                    (m0, m1)
                    (m0, m2)
                    (m0, m6)
                    (m1, m2)
                    (m1, m3)
                    (m1, m4)
                    (m1, m5)
                    (m2, m3)
                    (m4, m2)
                    (m4, m3)
                    (m5, m2)
                    (m5, m3)
                    (m5, m4)
                    (m6, m2)
                ]
                |> List.map (fun (a, b) -> { From = a ; To = b })
                |> Set.ofList
            actual |> shouldEqual expected
        | x -> failwithf "oh no: %O" x
