
let puzzleInput = 361527
let createGrid size = [| for _ in 1 .. size ->
                            [|  for _ in 1 .. size -> -1 |] |]  

type Direction = Right | Up | Left | Down

type Agent = {
    XPosition : int32 ;
    YPosition : int32 
    Direction : Direction }

let setCurrentGridValue (grid : int [][])  agent value =
    grid.[agent.XPosition].[agent.YPosition] <- value

let moveAgent agent direction = 
    match direction with
        | Right -> { XPosition = agent.XPosition+1; YPosition = agent.YPosition; Direction = direction }
        | Up -> { XPosition = agent.XPosition; YPosition = agent.YPosition+1; Direction = direction }
        | Left -> { XPosition = agent.XPosition-1; YPosition = agent.YPosition; Direction = direction }
        | Down -> { XPosition = agent.XPosition; YPosition = agent.YPosition-1; Direction = direction }
    
let calculateCellValue (grid : int [][])  agent =
    let cellsToCheck =[
        (agent.XPosition-1, agent.YPosition-1);
        (agent.XPosition, agent.YPosition-1);
        (agent.XPosition+1, agent.YPosition-1);

        (agent.XPosition-1, agent.YPosition);
        (agent.XPosition, agent.YPosition);
        (agent.XPosition+1, agent.YPosition);

        (agent.XPosition-1, agent.YPosition+1);
        (agent.XPosition, agent.YPosition+1);
        (agent.XPosition+1, agent.YPosition+1);
    ]
    let sum = cellsToCheck |> List.map (fun (x,y) -> grid.[x].[y] )
                 |> List.filter (fun x -> x>(-1))
                 |> List.sum
    match sum with
        | 0 -> 1
        | _ -> sum

let isCellEmpty (grid : int [][]) x y =
    match grid.[x].[y] with
        | -1 -> true
        | _ -> false

let decideNextCell grid agent =
    match agent.Direction with
        | Down when isCellEmpty grid (agent.XPosition+1) agent.YPosition -> Right
        | Right when isCellEmpty grid agent.XPosition (agent.YPosition+1) -> Up
        | Up when isCellEmpty grid (agent.XPosition-1) agent.YPosition -> Left
        | Left when isCellEmpty grid (agent.XPosition) (agent.YPosition-1) -> Down
        | _ -> agent.Direction

let rec buildGridTo (grid : int [][]) agent stopIn =
    printfn "%i" stopIn
    let cellValue = calculateCellValue grid agent 
    setCurrentGridValue grid agent cellValue
    let nextDirection = decideNextCell grid agent
    let nextAgent = moveAgent agent nextDirection

    match cellValue with
        | x when x > puzzleInput -> printfn "OVER %i" cellValue
        | _ -> ()

    match stopIn-1 with
        | i when i >= 0 -> buildGridTo grid nextAgent i
        | _ -> ()
   
let gridSize = 25

let testGrid = createGrid gridSize

let initalAgent = { 
    XPosition = gridSize/2;
    YPosition = gridSize/2;
    Direction = Right}

buildGridTo testGrid initalAgent 125
