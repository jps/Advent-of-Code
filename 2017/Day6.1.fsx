let getNextPosition currentPosition (inputArray:int[]) = 
    match (currentPosition, inputArray.Length) with
        | (p,l) when (p+1) = l -> 0
        | (p,_) -> (p+1)

let rec realocateTillNoMovesLeft (input:int[]) position moves =
    //printfn "%A postition:%i moves%i" input position moves
    match moves with
        | m when m > 0 -> 
            input.[position] <- (input.[position]+1)
            let nextPosition = getNextPosition position input
            realocateTillNoMovesLeft input nextPosition (moves-1)
        | _ -> input

let rec memoryReallocation input (previousStates: string list)= 
    //printfn "memoryReallocation %A" previousStates
    let largestValue = input |> Array.max
    let indexOfLargestValue = input |> Array.findIndex (fun x -> x = (largestValue))    
    input.[indexOfLargestValue] <- 0
    let startPosition = getNextPosition indexOfLargestValue input 
    let finalState = realocateTillNoMovesLeft input  startPosition largestValue
    let currentStateAsString = finalState 
                            |> Array.map (fun x -> x.ToString()) 
                            |> Array.fold (fun r s -> r + ";" + s) ""
    let newStateList = currentStateAsString :: previousStates
    let distinctLength = previousStates |> List.distinct |> List.length;
    let actualLength = previousStates |> List.length
    match (actualLength, distinctLength) with
        | (actual, distinct) when actual > distinct -> actual
        | _ -> memoryReallocation input newStateList
    


let test (inputString:string) expected= 
    let input = inputString.Split() |> Array.map int
    let actual = memoryReallocation input []
    printfn "result (%i) as expected(%i): %b" actual expected (expected = actual)
    ()

let inputString = test "0 2 7 0" 5
let inputData = test "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4" 1234