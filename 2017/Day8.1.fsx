type LogicalOperator = 
    | LessThan 
    | LessThanOrEqual 
    | GreaterThan 
    | GreaterThanOrEqual 
    | Equal 
    | NotEqual

type Condition = {
    left: string;
    value: int;
    logicalOperator : LogicalOperator;
}

type Instruction = {
    valueToUpdate: string;
    updateValueBy: int;
    condition : Condition
}

let stringToInstruction (input:string) =
    let parts = input.Split()
    let name = parts.[0]
    let instruction = {
        valueToUpdate = name;
        updateValueBy = match (parts.[1], parts.[2] |> int) with
                            | (addOrSubtract, changeBy) 
                                when addOrSubtract = "dec" -> changeBy *(-1)
                            | (_,changeBy) -> changeBy
        condition = { left = parts.[4]; 
                      value = parts.[6] |> int;
                      logicalOperator = match parts.[5] with
                                            | ">" -> GreaterThan
                                            | ">=" -> GreaterThanOrEqual
                                            | "==" -> Equal
                                            | "<" -> LessThan
                                            | "<=" -> LessThanOrEqual 
                                            | "!=" -> NotEqual
                                            | _ -> Equal
                      }                                                                     
    }
    instruction

let isInstructionValid (registers: Map<string,int>) (condition:Condition) =
    let leftValue = registers.[condition.left]
    let rightValue = condition.value 
    let isValid = match condition.logicalOperator with 
                    | GreaterThan ->  leftValue > rightValue
                    | GreaterThanOrEqual ->  leftValue >= rightValue
                    | Equal ->  leftValue = rightValue
                    | LessThan -> leftValue < rightValue
                    | LessThanOrEqual -> leftValue <= rightValue
                    | NotEqual -> leftValue <> rightValue
    printfn "%i %s %i actual: %b" leftValue (condition.logicalOperator |> string) rightValue isValid
    isValid
    
let executeInstructions instructions =
    let instructions = instructions |> Array.map stringToInstruction
    let mapOfRegisters = instructions |> Array.map (fun i -> (i.valueToUpdate,0)) |> Map.ofArray
    let updatedRegisters = instructions |> Array.fold (fun (state:Map<string,int>) currentInstruction -> 
                                                                        // for (key, value) in state |> Map.toList do 
                                                                        //     printfn "%s %i" key value
                                                                        match (isInstructionValid state currentInstruction.condition) with
                                                                            | true ->   let currentValue = state.[currentInstruction.valueToUpdate]
                                                                                        let nextValue = currentValue + currentInstruction.updateValueBy
                                                                                        printfn "adding %i to %i - is %i" currentInstruction.updateValueBy currentValue nextValue
                                                                                        state.Add(currentInstruction.valueToUpdate, nextValue)
                                                                            | false -> state
                                                                         ) mapOfRegisters
    let maxValue = updatedRegisters 
                    |> Map.toSeq 
                    |> Seq.map (fun (_,value) -> value) 
                    |> Seq.max                  

    for (key, value) in updatedRegisters |> Map.toList do
        printfn "%s %i" key value

    maxValue


let runWithTestData = 
    let instructions = [|
        "b inc 5 if a > 1";
        "a inc 1 if b < 5";
        "c dec -10 if a >= 1";
        "c inc -20 if c == 10" |]
    let maxValue = executeInstructions instructions

    printfn "maxValue %i" maxValue
    0

let useProvidedInputData =
    let inputFile = System.IO.File.ReadLines("Day8.input.txt")
    let maxValue = executeInstructions (inputFile |> Seq.toArray)
    //let moveCount = runInstructionSet instructionSet 0 0
    printfn "result:%i" maxValue
    ()

// useProvidedInputData