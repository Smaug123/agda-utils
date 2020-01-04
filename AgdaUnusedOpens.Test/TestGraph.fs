namespace AgdaUnusedOpens.Test

open NUnit.Framework
open FsUnitTyped

open AgdaUnusedOpens
open AgdaUnusedOpens.Types
open AgdaUnusedOpens.Internals

[<TestFixture>]
module TestGraph =
    [<Test>]
    let ``Hard-coded example`` () =
        let g =
            "Example.dot"
            |> Utils.getResource
            |> AdjacencyList.parse
        match g with
        | Ok (AdjacencyList g) ->
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

    [<Test>]
    let ``Convert to a graph`` () =
        let g = "Example.dot" |> Utils.getResource |> AdjacencyList.parse
        let g = match g with | Error _ -> failwith "oh no" | Ok g -> g
        let i = Graph.toGraph g
        let m0 = Path.make ["Sequences"]
        let m1 = Path.make ["Setoids" ; "Setoids"]
        let m2 = Path.make ["LogicalFormulae"]
        let m3 = Path.make ["Agda" ; "Primitive"]
        let m4 = Path.make ["Functions"]
        let m5 = Path.make ["Sets" ; "EquivalenceRelations"]
        let m6 = Path.make ["Numbers" ; "Naturals" ; "Definition"]

        let primitive =
            {
                Node = m3
                Unblocks = Set.empty
            }
        let logical =
            {
                Node = m2
                Unblocks = Set.singleton primitive
            }
        let functions =
            {
                Node = m4
                Unblocks =
                    [
                        logical
                        primitive
                    ]
                    |> Set.ofList
            }
        let numbers =
            {
                Node = m6
                Unblocks = Set.singleton logical
            }
        let equivRel =
            {
                Node = m5
                Unblocks =
                    [
                        functions
                        primitive
                        logical
                    ]
                    |> Set.ofList
            }
        let setoids =
            {
                Node = m1
                Unblocks =
                    [
                        equivRel
                        logical
                        functions
                        primitive
                    ]
                    |> Set.ofList
            }
        let sequences =
            {
                Node = m0
                Unblocks =
                    [
                        numbers
                        setoids
                        logical
                    ]
                    |> Set.ofList
            }

        i
        |> shouldEqual sequences

    [<Test>]
    let ``Cata traversal order`` () =
        let g = "Example.dot" |> Utils.getResource |> AdjacencyList.parse
        let g = match g with | Error _ -> failwith "oh no" | Ok g -> g
        let i = Graph.toGraph g

        let nodes = ResizeArray<Path> ()

        i
        |> Graph.cata (fun p _ -> nodes.Add p)

        nodes
        |> Seq.toList
        |> List.map Path.toString
        |> shouldEqual
            [
                "Agda.Primitive"
                "LogicalFormulae"
                "Numbers.Naturals.Definition"
                "Functions"
                "Sets.EquivalenceRelations"
                "Setoids.Setoids"
                "Sequences"
            ]
