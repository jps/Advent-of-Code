open System
open System.Runtime.InteropServices
open System.Threading

type Tower = {  Name: string; Weight: int; Towers: string List }


let rec isRoot (tower: Tower) (otherTowers: Tower list) = 
    
    let towerRefs = otherTowers |> List.collect (fun x -> x.Towers)
    let containsTower = towerRefs |> List.exists (fun x -> 
        //printfn "%s = %s ? %b" x tower.Name (x=tower.Name)
        x =tower.Name)
    // printfn "CURRNET:%s OTHERS: %A REFS:%A - result:%b" 
    //     tower.Name 
    //     (otherTowers |> List.map (fun x -> x.Name))
    //     towerRefs 
    //     containsTower
    containsTower |> not


let findTowerByName (allTowers: Tower list) (name: string) =
    allTowers |> List.find (fun x -> x.Name = name)

let rec calculateWheight (allTowers: Tower list) (node: Tower) =
    let towerLookup = findTowerByName allTowers
    let weightCalculator = calculateWheight allTowers
    let childWeight = List.sumBy weightCalculator (node.Towers |> List.map towerLookup) 
    childWeight + node.Weight

let rec isBlanced (node: Tower) (allTowers: Tower list) (depth: int) =
    let towerLookup = findTowerByName allTowers
    let weightCalculator = calculateWheight allTowers
    
    match node.Towers.IsEmpty with
            | true -> true
            | false ->  let currentNodeTowers = node.Towers |> List.map towerLookup
                        let towerWeights = currentNodeTowers |> List.map(fun t -> (t, weightCalculator t))
                        let areBalanced = (towerWeights |> List.distinctBy(fun (_,w) -> w) |> List.length) = 1
                        match areBalanced with 
                                | true -> true
                                | false ->  printfn "the tower is not balanced at depth:%i node:%s node weight(%i) sum(%i) %A" 
                                                    depth
                                                    node.Name 
                                                    node.Weight 
                                                    ((towerWeights |> List.sumBy(fun(_,w) -> w)) + (node.Weight))
                                                    (towerWeights |> List.map (fun (t,w) -> (t.Name,w)))
                                            currentNodeTowers |> List.forall (fun x-> isBlanced x allTowers (depth+1))
                
let findRootElement (instructions:string list)=
    let towers = instructions |> List.sortBy (fun x -> x.Length)
                                  |> List.fold (fun acc elm ->
                                        //printfn "element %A" instructions
                                        let towerParts = elm.Split()
                                        let name = towerParts.[0]
                                        let weight = (towerParts.[1]).Replace("(", "").Replace(")", "") |> int
                                        let references = match towerParts.Length with
                                                            | l when l > 2 -> 
                                                                            let (_,second) = Array.splitAt 3 towerParts
                                                                            second |> Array.map (fun x -> x.Replace(",", "")) |> Array.toList
                                                            | _ -> []
                                        {Tower.Name = name; Weight = weight; Towers = references} :: acc
                                    ) []    
    let root = towers  
                    |> List.find (fun x -> isRoot x (towers |> List.filter(fun y ->  x.Name <> y.Name )))


    let blanced = isBlanced root towers 0
    //find inbalance
    //from root iterate each child taking it's 

    printfn "ROOT ELEMENT:%s" root.Name

    printfn "list length %i" towers.Length
    
    printfn "is balanced %b" blanced

    root.Name
let useTestCase = 
    let inputText = ["pbga (66)";
                        "xhth (57)";
                        "ebii (61)";
                        "havc (66)";
                        "ktlj (57)";
                        "fwft (72) -> ktlj, cntj, xhth";
                        "qoyq (66)";
                        "padx (45) -> pbga, havc, qoyq";
                        "tknk (41) -> ugml, padx, fwft";
                        "jptl (61)";
                        "ugml (68) -> gyxo, ebii, jptl";
                        "gyxo (61)";
                        "cntj (57)"]

    printfn "elements:%i" (inputText |> List.length)
    let rootElement = findRootElement inputText
    printfn "root element:%s" rootElement    
    ()                

let useProvidedInputData =
    let inputFile = System.IO.File.ReadLines("Day7.input.txt")
    let instructionSet = inputFile |> Seq.toList
    let rootElement = findRootElement instructionSet
    printfn "root element:%s" rootElement
    ()