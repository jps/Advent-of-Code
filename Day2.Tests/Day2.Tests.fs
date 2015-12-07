module Day2.Tests
// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote


let testDimension1 = {Length = 2; Width = 3; Height = 4 }
let testDimension2 = {Length = 1; Width = 1; Height = 10 }

let filePath = "data.txt"

[<Test>]
let CaculateSurfaceAreaWithSlack_1() =
    let expected = 58    
    let actual = Day2.CaculateSurfaceAreaWithSlack(testDimension1)
    Check.QuickThrowOnFailure (expected = actual |@ sprintf "%i = %i" expected actual)

[<Test>]
let CaculateSurfaceAreaWithSlack_2() =
    let expected = 43    
    let actual = Day2.CaculateSurfaceAreaWithSlack(testDimension2)
    Check.QuickThrowOnFailure (expected = actual |@ sprintf "%i = %i" expected actual)

[<Test>]
let Can_LoadDimensionsFromFile() =    
    let actual = Day2.LoadDimensionsFromFile(filePath)
    Check.QuickThrowOnFailure (actual.Length > 0 |@ sprintf "List contains %i items" actual.Length)

[<Test>]
let Can_CalculateTotalSurfaceAreaRequired() =
    let expected = 1606483
    let actual = Day2.CalculateTotalSurfaceAreaRequired (Day2.LoadDimensionsFromFile(filePath))
    Check.QuickThrowOnFailure (expected = actual |@ sprintf "expected %i actual %i" expected actual)

[<Test>]
let Can_CaculateSmallestFaceWithSlack_1() =
    let expected = 34
    let actual = Day2.CaculateSmallestFaceWithSlack(testDimension1)
    Check.QuickThrowOnFailure (expected = actual |@ sprintf "%i = %i" expected actual)

[<Test>]
let Can_CaculateSmallestFaceWithSlack_2() =
    let expected = 14
    let actual = Day2.CaculateSmallestFaceWithSlack(testDimension2)
    Check.QuickThrowOnFailure (expected = actual |@ sprintf "%i = %i" expected actual)

[<Test>]
let Can_CalculateTotalLengthRequired() =
    let expected = 3842356
    let actual = Day2.CalculateTotalLengthRequired (Day2.LoadDimensionsFromFile(filePath))
    Check.QuickThrowOnFailure (expected = actual |@ sprintf "expected %i actual %i" expected actual)