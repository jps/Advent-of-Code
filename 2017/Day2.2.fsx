let rec FindDivisablePair (inputValues: int list) =
    match inputValues with
                | head::tail when tail |> List.exists (fun x -> x % head = 0) 
                    -> (head, tail |> List.find(fun x -> x % head = 0))
                | _ -> FindDivisablePair inputValues.Tail
let ResultOfNumbersWhichEvenlyDivide (inputValues) =
    let orderMinToMax = inputValues |> List.sort
    let (divisor, multiple) = FindDivisablePair orderMinToMax
    multiple / divisor

let testCases = [
    ("5 9 2 8", 4);
    ("9 4 7 3", 3);
    ("3 8 6 5", 2)]

for (input, expected) in testCases do
            let inputValues = input.Split() |> Array.map int |> Array.toList;
            let result = ResultOfNumbersWhichEvenlyDivide(inputValues)
            printfn "input:%s expected:%i actual:%i match:%b" input expected result (result = expected)


let UseProvidedInputData =
    let spreadsheetRows = System.IO.File.ReadLines("2017\\Day2.input.txt")
    let result = spreadsheetRows 
                |> Seq.map (fun x -> x.Split() 
                                    |> Array.map int 
                                    |> Array.toList) 
                |> Seq.toList 
                |> List.sumBy ResultOfNumbersWhichEvenlyDivide
    printfn "result:%i" result
    ()
