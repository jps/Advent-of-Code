module Day7.Tests

open Day7
open System
open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote

let testDataPath = "testData.txt"
let filePath = "data.txt"
let filePath2 = "data2.txt"

[<Test>]
let Parser_BuildCommandMap_Should_Build_Correct_Commands_For_Data() =
    let parser = new Parser()    
    let commands = (System.IO.File.ReadLines(filePath)) |> parser.BuildCommandMap  |> Map.toList    
    
    let TotalCount = commands |> List.length
    let ValCount = commands |> List.filter(fun (str,cmd) -> match cmd with  | VAL(x) -> true | _ -> false) |> List.length
    let NotCount = commands |> List.filter(fun (str,cmd) -> match cmd with  | NOT(x) -> true | _ -> false) |> List.length
    let AndCount = commands |> List.filter(fun (str,cmd) -> match cmd with  | AND(x,y) -> true | _ -> false) |> List.length
    let VandCount = commands |> List.filter(fun (str,cmd) -> match cmd with  | VAND(x,y) -> true | _ -> false) |> List.length
    let OrCount = commands |> List.filter(fun (str,cmd) -> match cmd with  | OR(x,y) -> true | _ -> false) |> List.length
    let PointerCount = commands |> List.filter(fun (str,cmd) -> match cmd with  | POINTER(x) -> true | _ -> false) |> List.length
    let LShift = commands |> List.filter(fun (str,cmd) -> match cmd with  | LSHIFT(x,y) -> true | _ -> false) |> List.length
    let RShift = commands |> List.filter(fun (str,cmd)-> match cmd with  | RSHIFT(x,y) -> true | _ -> false) |> List.length

    let expectedTotalCount = 339
    let expectedValCount = 2
    let expectedNotCount = 48
    let expectedAndCount = 96
    let expectedVAndCount = 16
    let expectedOrCount = 80
    let expectedPointerCount = 1
    let expectedLShiftCount = 32
    let expectedRShiftCount = 64   

    Check.QuickThrowOnFailure (TotalCount = expectedTotalCount |@ sprintf "%i != %i" TotalCount expectedTotalCount)
    Check.QuickThrowOnFailure (ValCount = expectedValCount |@ sprintf "%i != %i" ValCount expectedValCount)
    Check.QuickThrowOnFailure (NotCount = expectedNotCount |@ sprintf "%i != %i" NotCount expectedNotCount)
    Check.QuickThrowOnFailure (AndCount = expectedAndCount |@ sprintf "%i != %i" AndCount expectedAndCount)
    Check.QuickThrowOnFailure (VandCount = expectedVAndCount |@ sprintf "%i != %i" VandCount expectedVAndCount)
    Check.QuickThrowOnFailure (OrCount = expectedOrCount |@ sprintf "%i != %i" OrCount expectedOrCount)
    Check.QuickThrowOnFailure (PointerCount = expectedPointerCount |@ sprintf "%i != %i" PointerCount expectedPointerCount)
    Check.QuickThrowOnFailure (LShift = expectedLShiftCount |@ sprintf "%i != %i" LShift expectedLShiftCount)
    Check.QuickThrowOnFailure (RShift = expectedRShiftCount |@ sprintf "%i != %i" RShift expectedRShiftCount)
    ()

[<TestCase("d",72us)>]
[<TestCase("e",507us)>]
[<TestCase("f",492us)>]
[<TestCase("g",114us)>]
[<TestCase("h",65412us)>]
[<TestCase("i",65079us)>]
[<TestCase("x",123us)>]
[<TestCase("y",456us)>]
[<TestCase("q",8us)>]
[<TestCase("z",8us)>]
let Parser_EvalCommands_Should_Build_Correct_Commands_For_Test_Data( key, expected  )=
    let parser = new Parser()    
    let commands = (System.IO.File.ReadLines(testDataPath)) |> parser.BuildCommandMap
    let evaluatedCommands = parser.EvalCommands(commands)
    let (_,actual) = evaluatedCommands.[key]
    Check.QuickThrowOnFailure (actual = expected|@ sprintf "%i != %i" actual expected)


//        | POINTER(pointTo) -> (EvalCommand map.[pointTo] map) 
//        | NOT(applyTo) -> ~~~(EvalCommand map.[applyTo] map) //~~~16us
//        | VAND(vala, b) -> vala &&& (EvalCommand map.[b] map)
//        | AND(a, b) -> (EvalCommand map.[a] map) &&& (EvalCommand map.[b] map)//eval(map.[a],map) &&& eval(map.[b],map) &&&
//        | OR(a, b) -> (EvalCommand map.[a] map) ||| (EvalCommand map.[b] map)
//        | LSHIFT(applyTo, nBits) -> (EvalCommand map.[applyTo] map) <<< nBits//failwith "Not implemented yet" <<<
//        | RSHIFT(applyTo, nBits) ->

    
    
    ()
    //Check.QuickThrowOnFailure (commands.["d"] = 72) // |@ sprintf "%i != %i" expectedDval  dval)


//[<Test>] -- do not run will never complete... 
//let Parser_Question_1() =
//    let parser = new Parser()    
//    let commands = (System.IO.File.ReadLines(filePath2)) |> parser.BuildCommandMap    
//    let value = parser.EvalSingle "a" commands 
//    let actualValue = value
//    let expected = 45454us
//    Check.QuickThrowOnFailure (actualValue = expected|@ sprintf "%i != %i" actualValue expected)
//    ()    

[<Test>]
let Parser_Question_1_Simple() =
    let expected = 956us    
    let parser = new Parser()    
    let commands = (System.IO.File.ReadLines(filePath)) |> parser.BuildCommandMap    
    let value = parser.SimpleEvalCommands commands 
    let actual = value.["a"]    
    Check.QuickThrowOnFailure (expected = actual|@ sprintf "%i != %i" expected actual)
    ()    

[<Test>]
let Parser_Question_2_Simple() =
    let expected = 40149us
    let parser = new Parser()    
    let commands = (System.IO.File.ReadLines(filePath2)) |> parser.BuildCommandMap    
    let value = parser.SimpleEvalCommands commands 
    let actual = value.["a"]    
    Check.QuickThrowOnFailure (expected = actual|@ sprintf "%i != %i" expected actual)
    ()    


//    let expectedDval = 72
//    
//
//    Check.QuickThrowOnFailure (expectedDval = dval  |@ sprintf "%i != %i" expectedDval  dval)
    