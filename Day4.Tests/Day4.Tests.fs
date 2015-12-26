module Day4.Tests

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote
open Day4


[<Literal>] 
let expectedStart5 = "00000"
[<Literal>] 
let expectedStart6 = "000000"


[<TestCase("abcdef", expectedStart5,609043)>]
[<TestCase("pqrstuv", expectedStart5 ,1048970)>]
let Miner_findFistMatchForKey_Should_Provide_Correct_Value key startsWith expected =
    let actual = Miner.findFistMatchForKey(key, startsWith, expected)    
    Check.QuickThrowOnFailure (actual = expected |@ sprintf "actual %i - expected %i" actual expected)

[<TestCase("abcdef", expectedStart5, 609043)>]
[<TestCase("pqrstuv", expectedStart5, 1048970)>]
[<TestCase("yzbqklnj", expectedStart5, 282749)>]
[<TestCase("yzbqklnj", expectedStart6, 9962624)>]
let Miner_findFistMatchForKey_Should_Provide_Correct_Value_From_0 key startsWith expected =
    let actual = Miner.findFistMatchForKey(key, startsWith, 0)    
    Check.QuickThrowOnFailure (actual = expected |@ sprintf "actual %i - expected %i" actual expected)
