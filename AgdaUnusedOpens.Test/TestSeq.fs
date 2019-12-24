namespace AgdaUnusedOpens.Test

open AgdaUnusedOpens.Internals
open NUnit.Framework
open FsUnitTyped
open FsCheck

[<TestFixture>]
module TestSeq =
    [<Test>]
    let ``PBT for Seq.omit`` () =
        let property (l1 : byte list) (x : byte) (l2 : byte list) =
            Seq.omit (List.length l1) (l1 @ (x :: l2))
            |> List.ofSeq
            |> shouldEqual (l1 @ l2)

        Check.QuickThrowOnFailure property
