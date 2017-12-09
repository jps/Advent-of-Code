let puzzleInput = 361527

type Direction = Right | Up | Left | Down
type SquareExpandsInto = DownRight | UpLeft

let rec findSmallestPowerOfTwoNumerExistsWithin input current = 
    match input with 
        | i when i <= pown current 2 -> current
        | _ -> findSmallestPowerOfTwoNumerExistsWithin input (current+1)

let findMidPoint startPoint endPoint =     
    (System.Math.Ceiling(((startPoint - endPoint) |> float) / (2 |> float))) 
                |> int 
                |> (+)endPoint

let findDistance input = 
    let containingSquare = findSmallestPowerOfTwoNumerExistsWithin input 2
    let expandingInto = match containingSquare % 2 = 0 with
                        | true -> SquareExpandsInto.UpLeft
                        | false -> SquareExpandsInto.DownRight
    let lastPower = pown (containingSquare-1) 2
    let containgPower = pown containingSquare 2
    
    printfn "Last Power:%i Containing Power:%i" lastPower containgPower

    let corner = findMidPoint containgPower lastPower

    let yMidPoint = (match expandingInto with
                        | DownRight -> findMidPoint (lastPower+1) corner
                        | UpLeft -> findMidPoint (lastPower+1) corner |> (-)1)
                     |> System.Math.Abs
    
    let xMidPoint = (match expandingInto with
                        | DownRight -> findMidPoint corner containgPower
                        | UpLeft -> findMidPoint  corner containgPower)
                     |> System.Math.Abs

    printfn "Corner:%i XMidPoint:%i YMidPoint:%i" corner xMidPoint yMidPoint

    let direction = match (input < corner, expandingInto) with
                                    | (true, UpLeft) -> Up
                                    | (false, UpLeft) -> Left
                                    | (true, DownRight) -> Down
                                    | (false, DownRight) -> Right
    
    printfn "Direction: %s" (direction |> string)

    let distanceFromYMidPoint = match direction with
                                        | Up | Down -> ([input;yMidPoint] |> List.max) - ([input;yMidPoint] |> List.min)
                                        | Right | Left -> ([corner;yMidPoint] |> List.max) - ([corner;yMidPoint] |> List.min)
    let distanceFromXMidPoint = match direction with
                                        | Up | Down -> ([corner;xMidPoint] |> List.max) - ([corner;xMidPoint] |> List.min)
                                        | Right | Left -> ([input;xMidPoint] |> List.max) - ([input;xMidPoint] |> List.min)
    
    printfn "dist from center X:%i Y:%i" distanceFromXMidPoint distanceFromYMidPoint

    distanceFromXMidPoint + distanceFromYMidPoint
    

findDistance puzzleInput
