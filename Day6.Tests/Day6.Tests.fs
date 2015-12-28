module Day6.Tests

open System
open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote
open Day6

let filePath = "data.txt"


[<Test>]
let GridUpdater_DefaultGrid_Should_Contain_One_Million_Cells() =
    let expectedLength = 1000000
    let grid = GridUpdater.DefaultGrid()
    let actualLength = grid |> Array.length
    let actualMaxXCell = grid |> Array.maxBy (fun x -> x.X)
    let actualMaxYCell = grid |> Array.maxBy (fun x -> x.Y)
    Check.QuickThrowOnFailure (expectedLength = actualLength |@ sprintf "%i != %i" expectedLength actualLength)
    Check.QuickThrowOnFailure (GridUpdater.maxX = actualMaxXCell.X  |@ sprintf "%i != %i" GridUpdater.maxX actualMaxXCell.X)
    Check.QuickThrowOnFailure (GridUpdater.maxY = actualMaxYCell.Y  |@ sprintf "%i != %i" GridUpdater.maxY actualMaxYCell.Y)

// turn on 0,0 through 999,999 would turn on (or leave on) every light.
// toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning
// on the ones that were off.
// turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.



[<TestCase("turn on 0,0 through 999,999", 0,0,999,999)>]
[<TestCase("toggle 0,0 through 999,0", 0,0,999,0)>]
[<TestCase("turn off 499,499 through 500,500", 499,499,500,500)>]
let GridUpdater_GetCords(testString, fromX,fromY, toX, toY) =
    let (fromCord, toCord) = GridUpdater.GetCords(testString)
    Check.QuickThrowOnFailure (fromCord.X = fromX |@ sprintf "%i != %i" fromCord.X fromX)
    Check.QuickThrowOnFailure (fromCord.Y = fromY |@ sprintf "%i != %i" fromCord.Y fromY)
    Check.QuickThrowOnFailure (toCord.X = toX |@ sprintf "%i != %i" toCord.X toX)
    Check.QuickThrowOnFailure (toCord.Y = toY |@ sprintf "%i != %i" toCord.Y toY)
(* part 2 breaks this test roll back to 42f819ac 
[<TestCase("turn on 0,0 through 999,999", 1000000)>]
[<TestCase("toggle 0,0 through 999,0", 1000)>]
[<TestCase("turn on 499,499 through 500,500", 4)>]
let GridUpdater_UpdateGrid_Should_Have_The_Correct_Amount_Of_Cells_Active (testString:string) expectedCellsActive =
    let updatedGrid = GridUpdater.UpdateGrid testString (GridUpdater.DefaultGrid())
    let actualActive = updatedGrid |> List.filter (fun x -> x.IsOn) |> List.length
    Check.QuickThrowOnFailure (actualActive = expectedCellsActive |@ sprintf "%i != %i" actualActive expectedCellsActive)    

[<Ignore>] //is correct just a little slow... 
[<Test>]
let GridUpdater_Question_1() =
    let expectedActive = 377891
    let commands = (System.IO.File.ReadLines(filePath)) 
    let grid = Seq.fold (fun acc x -> GridUpdater.UpdateGrid x acc) (GridUpdater.DefaultGrid()) commands
    let actualActive = grid |> List.filter (fun x -> x.IsOn) |> List.length
    Check.QuickThrowOnFailure (actualActive = expectedActive |@ sprintf "%i != %i" actualActive expectedActive)    
*)

[<Test>]
let GridUpdater_Question_2() =
    let expectedTotal = 14110788
    let commands = (System.IO.File.ReadLines(filePath)) 
    let grid = Seq.fold (fun acc x -> GridUpdater.UpdateGrid x acc) (GridUpdater.DefaultGrid()) commands
    let actualTotal = grid |> Array.sumBy (fun x -> x.IsOn)
    Check.QuickThrowOnFailure (actualTotal = expectedTotal |@ sprintf "%i != %i" actualTotal expectedTotal)    