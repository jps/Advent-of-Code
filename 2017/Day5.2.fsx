let getNextBoardState (input:int[]) position = 
    let nextState = Array.copy input
    nextState.[position] <- nextState.[position]+1
    nextState

let getNextPosition (input:int[]) position =
    input.[position] + position 

let rec runInstructionSet (input:int[]) position moveCount =
        // printfn "RUN INPUT:%A - POSITION:%i - MOVE: %i" input position moveCount
        match (position, input.Length) with
            | (position, arrayLength) 
                when position < 0 || position > arrayLength-1 -> moveCount
            | _ -> runInstructionSet (getNextBoardState input position) (getNextPosition input position) (moveCount+1)

let testCases = [([|0; 3;  0;  1;  -3;|], 5); ]

for (input, expected) in testCases do
            let result = runInstructionSet input 0 0
            printfn "input:%A expected:%i actual:%i match:%b" input expected result (result = expected)
            
let useProvidedInputData =
    let inputFile = System.IO.File.ReadLines("Day5.input.txt")
    let instructionSet = inputFile |> Seq.map int |> Seq.toArray
    let moveCount = runInstructionSet instructionSet 0 0
    printfn "result:%i" moveCount
    ()