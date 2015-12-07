module Day3.Tests

// https://github.com/fsharp/FsCheck/blob/master/Docs/Documentation.md
// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote

// all tests are failing

// Note on FsCheck tests: The NUnit test runner will still green-light failing tests with Check.Quick 
// even though it reports them as failing. Use Check.QuickThrowOnFailure instead.

[<Test>]
let ``FsCheck test 1``() =
    Check.QuickThrowOnFailure (true = false |@ sprintf "true = false")