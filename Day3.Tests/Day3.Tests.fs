module Day3.Tests

// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote

[<TestCase('^', 0, 1)>]
[<TestCase('>', 1, 0)>]
[<TestCase('V', 0, -1)>]
[<TestCase('<', -1, 0)>]
let Mover_Test_Single_Direction directionChar expectedX expectedY =
    let expectedListLength = 2;     
    let positionList = Mover.Move (Mover.initPosition()) directionChar        
    Check.QuickThrowOnFailure (positionList.Length = expectedListLength |@ sprintf "move %c" directionChar)
    Check.QuickThrowOnFailure (positionList.Head.X = expectedX |@ sprintf "move once %c should be %i" directionChar expectedX)
    Check.QuickThrowOnFailure (positionList.Head.Y = expectedY |@ sprintf "move once %c should be %i" directionChar expectedY)

[<TestCase("^>V<", 0, 0)>]
[<TestCase("^>v<", 0, 0)>]
[<TestCase("^v^v^v^v^v", 0, 0)>]
let Mover_Test_Multiple_Direction directionString expectedX expectedY =
    let expectedListLength = (Seq.length directionString) |> (+) 1;     
    let positionList = Mover.MoveMany directionString
    Check.QuickThrowOnFailure (positionList.Length = expectedListLength |@ sprintf "move %s" directionString)
    Check.QuickThrowOnFailure (positionList.Head.X = expectedX |@ sprintf "move many should be %i" expectedX)
    Check.QuickThrowOnFailure (positionList.Head.Y = expectedY |@ sprintf "move many should be %i" expectedY)

[<TestCase("^>V<", 4)>]
let Mover_Test_Distinct directionString expectedDistinct =
    let positionList = Mover.MoveMany directionString
    let distinctList = Seq.distinct positionList
    let actualDistinct = Seq.length distinctList
    Check.QuickThrowOnFailure (actualDistinct = expectedDistinct |@ sprintf "there should be %i distinct cells" expectedDistinct)
    
[<Test>]
let Mover_Test_Distinct_Question_1 () =
    let directionString = System.IO.File.ReadAllText("data.txt")
    let expectedDistinct = 2565
    let positionList = Mover.MoveMany directionString
    let distinctList = Seq.distinct positionList
    let actualDistinct = Seq.length distinctList
    Check.QuickThrowOnFailure (actualDistinct = expectedDistinct |@ sprintf "there should be %i distinct cells got %i" expectedDistinct actualDistinct)

[<Test>]
let WorkLoadBalancer_Test_Split() =
    let sampleString = "abcdefghij"
    let expectedLength = sampleString.Length / 2

    let directionStringA =  WorkLoadBalancer.everyNth 2 0 sampleString |> System.String.Concat // |> Seq.concat
    let directionStringB =  WorkLoadBalancer.everyNth 2 1 sampleString |> System.String.Concat

    Check.QuickThrowOnFailure (directionStringA.Length = expectedLength |@ sprintf "a: length should be %i got %i" directionStringA.Length expectedLength)
    Check.QuickThrowOnFailure (directionStringB.Length = expectedLength |@ sprintf "b: length should be %i got %i" directionStringB.Length expectedLength)

[<Test>]
let Mover_Test_Distinct_Question_2 () =
    let expectedDistinct = 2639
    
    let directionStringAll = System.IO.File.ReadAllText("data.txt")
    let directionStringA =  WorkLoadBalancer.everyNth 2 0 directionStringAll |> System.String.Concat
    let directionStringB =  WorkLoadBalancer.everyNth 2 1 directionStringAll |> System.String.Concat

    let positionListA = Mover.MoveMany directionStringA
    let positionListB = Mover.MoveMany directionStringB

    let Combined = List.concat[positionListA; positionListB]

    let distinctList = Seq.distinct Combined
    let actualDistinct = Seq.length distinctList

    Check.QuickThrowOnFailure (actualDistinct = expectedDistinct |@ sprintf "there should be %i distinct cells got %i" expectedDistinct actualDistinct)

