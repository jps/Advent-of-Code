open System
open System.Collections.ObjectModel

let DifferenceBetweenMinAndMax (inputValues) =
    let max = inputValues |> Array.max
    let min = inputValues |> Array.min
    max - min

let testCases = [
    ("5 1 9 5", 8);
    ("7 5 3", 4);
    ("2 4 6 8", 6)]

for (input, expected) in testCases do
            let inputValues = input.Split() |> Array.map int;
            let result = DifferenceBetweenMinAndMax(inputValues)
            printfn "input:%s expected:%i actual:%i match:%b" input expected result (result = expected)


let UseProvidedInputData =
    let spreadsheetRows = System.IO.File.ReadLines("2017\\Day2.input.txt")
    let result = spreadsheetRows |> Seq.map (fun x -> x.Split() |> Array.map int) |> Seq.toArray |> Array.sumBy DifferenceBetweenMinAndMax
    printfn "result:%i"
    ()
