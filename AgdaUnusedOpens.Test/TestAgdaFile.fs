namespace AgdaUnusedOpens.Test

open AgdaUnusedOpens
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestAgdaFile =

    [<Test>]
    let ``Make example`` () =
        let agdaFile =
            "Example.agda"
            |> Utils.getResource'
            |> AgdaFile.make
        agdaFile.ModuleLine
        |> shouldEqual 20
        agdaFile.Path.Head
        |> shouldEqual "Rings"
        agdaFile.Path.Tail
        |> shouldEqual ["EuclideanDomains" ; "Definition"]

    [<Test>]
    let ``Rename example`` () =
        let agdaFile =
            "Example.agda"
            |> Utils.getResource'
            |> AgdaFile.make
        let newFile = AgdaFile.rename "NewModule" agdaFile
        newFile.ModuleLine |> shouldEqual 20
        newFile.Path.Head |> shouldEqual "Rings"
        newFile.Path.Tail |> shouldEqual ["EuclideanDomains" ; "NewModule"]

        let expected = "module Rings.EuclideanDomains.NewModule {a b : _} {A : Set a} {S : Setoid {a} {b} A} {_+_ _*_ : A → A → A} (R : Ring S _+_ _*_) where"
        newFile.Contents.[20] |> shouldEqual expected
