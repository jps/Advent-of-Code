module Day5.Tests

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote
open Day5

let filePath = "data.txt"
    
[<TestCase("ugknbfddgicrmopn", true)>]
[<TestCase("aaa", true)>]
[<TestCase("jchzalrnumimnmhp", false)>]
[<TestCase("haegwjzuvuyypxyu", false)>]
[<TestCase("dvszwmarrgswjxmb", false)>]
let StringValidator_IsStringValid(testString:string, expectedValidity:bool) =
    let validator = StringValidator()
    let actualValidity = validator.isStringValid(testString)
    Check.QuickThrowOnFailure (expectedValidity = actualValidity |@ sprintf "actual:%b = expected:%b" actualValidity expectedValidity)

[<TestCase(256)>]
let StringValidator_Question1(expectedCount) =
    let validator = StringValidator()

    let test = (System.IO.File.ReadLines(filePath)) 
                    |> Seq.where validator.isStringValid
                    |> Seq.toArray

    let actualCount = (System.IO.File.ReadLines(filePath)) 
                    |> Seq.where validator.isStringValid
                    |> Seq.length
    Check.QuickThrowOnFailure (expectedCount = actualCount |@ sprintf "actual:%i = expected:%i" actualCount expectedCount)