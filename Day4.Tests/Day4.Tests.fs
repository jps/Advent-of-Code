module Day4.Tests

// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote
open Day4
// all tests are failing

// Note on FsCheck tests: The NUnit test runner will still green-light failing tests with Check.Quick 
// even though it reports them as failing. Use Check.QuickThrowOnFailure instead.

[<TestCase("abcdef", 609043)>]
[<TestCase("pqrstuv", 1048970)>]
let Miner_findFistMatchForKey_Should_Provide_Correct_Value key expected =
    let actual = Miner.findFistMatchForKey(key, expected)    
    Check.QuickThrowOnFailure (actual = expected |@ sprintf "actual %i - expected %i" actual expected)

[<TestCase("abcdef", 609043)>]
[<TestCase("pqrstuv", 1048970)>]
[<TestCase("yzbqklnj", 282749)>]
let Miner_findFistMatchForKey_Should_Provide_Correct_Value_From_0 key expected =
    let actual = Miner.findFistMatchForKey(key, 0)    
    Check.QuickThrowOnFailure (actual = expected |@ sprintf "actual %i - expected %i" actual expected)
