//  let ArrayToString array = array |> Array.map (fun x -> x.ToString()) 
//                                 |> Array.fold (fun r s -> r + ";" + s) ""
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
        | (actual, distinct) when actual > distinct -> 
            //let lastOccurance = newStateList.Length-2
            let dupeString = newStateList.[newStateList.Length-2]
            let firstOccurance = previousStates |> List.findIndex (fun x -> x = dupeString)
            let lastOccurance = previousStates |> List.findIndexBack (fun x -> x = dupeString)
            lastOccurance - firstOccurance
            //let distinct = (previousStates |> List.distinct)
            //let firstNonDistinct = previousStates |> List.findIndex (fun x -> previousStates |> List.(x))
            //FIND DUPLICATE

            
            // let lastOccurance = previousStates |> List.findIndexBack (fun x -> x = currentStateAsString)
            // printfn "length:(%i) firstOccurance:(%i) lastOccurance:(%i) %s" actual firstOccurance lastOccurance currentStateAsString
            // printfn "NON DISTINCT:(%A) DISTINCT:(%A)" (previousStates |> List.distinct) previousStates
        | _ -> memoryReallocation input newStateList
    


let test (inputString:string) expected= 
    let input = inputString.Split() |> Array.map int
    let actual = memoryReallocation input [input 
                            |> Array.map (fun x -> x.ToString()) 
                            |> Array.fold (fun r s -> r + ";" + s) ""]
    printfn "result (%i) as expected(%i): %b" actual expected (expected = actual)
    ()

let inputString = test "0 2 7 0" 4

let inputData = test "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4" 1234
// let getNextPosition currentPosition (inputArray:int[]) = 
//     match (currentPosition, inputArray.Length) with
//         | (p,l) when (p+1) = l -> 0
//         | (p,_) -> (p+1)

// let ArrayToString array = array 
//                             |> Array.map (fun x -> x.ToString()) 
//                             |> Array.fold (fun r s -> r + ";" + s) ""

// let rec realocateTillMatch (input:int[]) position moves (previousStates: string list) =
//     // printfn "%A postition:%i moves%i" input position moves
//     match (moves) with
//         | (m) when m > 0 -> 
//             input.[position] <- (input.[position]+1)
//             let nextPosition = getNextPosition position input
//             match (input |> ArrayToString, previousStates) with
//                     //| (i,ps) when ps |> List.contains i -> (i::ps)
//                     | (i,ps) -> realocateTillMatch input nextPosition (moves-1) (i::ps)
//         | _ -> previousStates

// let rec memoryReallocation input (previousStates: string list)= 
//     printfn "memoryReallocation %A" previousStates
//     let largestValue = input |> Array.max
//     let indexOfLargestValue = input |> Array.findIndex (fun x -> x = (largestValue))    
//     input.[indexOfLargestValue] <- 0
//     let startPosition = getNextPosition indexOfLargestValue input 
//     let newPreviousStateList = realocateTillMatch  input  startPosition largestValue ((input |> ArrayToString)::previousStates)
//     let distinctLength = newPreviousStateList |> List.distinct |> List.length;
//     let actualLength = newPreviousStateList |> List.length
//     printfn "%i" previousStates.Length
//     match (actualLength, distinctLength) with
//         | (al,dl) when al > dl -> previousStates
//         | _ -> memoryReallocation input newPreviousStateList
// // match (currentStateAsString, expected) with
// //     | (fs, e) when fs = e ->
// //                  printfn "currentStateAsString:%s expected%s" currentStateAsString expected
// //                  newStateList |> List.length
// //     | _ -> 
    
        
    


// let test (inputString:string) expected = 
//     let input = inputString.Split() |> Array.map int
//     printfn "input (%A)" input
//     let states = memoryReallocation input [input|> ArrayToString]
//     printfn "result (%A) as count()expected(%i): %b" states (states |> List.length) (expected = (states |> List.length))
//     ()

// //let inputString = test "0 2 7 0" 4
// let inputData = test "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4" 1234