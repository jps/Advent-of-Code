let rec realocateTillNoMovesLeft (input:int[]) position moves =
    match moves with
        | m when m > 0 -> 
            input.[position] <- (input.[position]+1)
            let nextPosition = match (position, input.Length) with
                                    | (p,l) when p = l -> 0
                                    | (p,_) -> (p+1)
            realocateTillNoMovesLeft input nextPosition (moves-1)
        | _ -> input

let rec memoryReallocation input (previousStates: string list)= 
    let largestValue = input |> Array.max
    let indexOfLargestValue = input |> Array.findIndex (fun x -> x = (largestValue))    
    input.[indexOfLargestValue] <- 0

    let finalState = realocateTillNoMovesLeft input indexOfLargestValue largestValue
    let newStateList = finalState.ToString() :: previousStates
    match (previousStates |> List.length, previousStates |> List.distinct |> List.length) with
        | (actual, distinct) when actual = distinct -> actual
        | _ -> memoryReallocation input newStateList
    


let testCase = 
    let input = "0 2 7 0".Split() |> Array.map int
    let expected = 5
    let actual = memoryReallocation input []
    printfn "result (%i) as expected(%i): %b" actual expected (expected = actual)
    ()

let inputData = "14	0	15	12	11	11	3	5	1	6	8	4	9	1	8	4"