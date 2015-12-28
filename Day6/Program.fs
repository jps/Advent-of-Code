// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace Day6

open System
(*
--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to 
deploy one million lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the 
ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999,
and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as 
coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair 
like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you 
in order.

For example:

    turn on 0,0 through 999,999 would turn on (or leave on) every light.
    toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning
     on the ones that were off.
    turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

After following the instructions, how many lights are lit?


*)
type Cord(x,y) = 
    member this.X = x;
    member this.Y = y
type Cell = 
    struct
        val X: int
        val Y: int
        val IsOn: bool
        new (x,y,isOn) = { X = x; Y = y; IsOn = isOn}
    end
 
module GridUpdater =
    let maxX = 999
    let maxY = 999
    let cells = 1000000
    let DefaultGrid () = 
        (List.init cells (fun index -> new Cell(index % 1000, index / 1000, false)))
//      this is sooo much slower
//        [|for x in 0 .. maxX do
//          for y in 0 .. maxY do 
//                yield Cell( x, y, false) |]
    let (|TurnOn|TurnOff|Toggle|) (input:string) = 
        match input with
        | i when i.Contains("turn on") -> TurnOn
        | i when i.Contains("turn off") -> TurnOff
        | _ -> Toggle
    let getCordFromString (cordString:string) =
        let cords = cordString.Split(',') |> Array.map(Int32.Parse)
        Cord(cords.[0], cords.[1])
    let GetCords (input:string) =
        let redundantStrings = ["turn on";"turn off";"through";"toggle"]        
        let cleaned = List.fold (fun (acc:string) (x:string) -> acc.Replace(x,"")) input redundantStrings 
        let cordStrings = cleaned.Replace("  "," ").Trim().Split(' ')
        (getCordFromString(cordStrings.[0]), getCordFromString(cordStrings.[1]))
    let UpdateGrid (command:string) (grid: Cell list) = 
        let (fromCords, toCords) = GetCords(command)
        let action(command:string) (current:bool) = 
            match command with
            | TurnOn -> true
            | TurnOff -> false
            | Toggle -> not current       
        let partiallyAppliedAction = action command
        let updateCell (cell:Cell) = 
            Cell(cell.X, cell.Y, partiallyAppliedAction(cell.IsOn))
        let shouldUpdate (cell:Cell) = 
            match cell with
            | c when c.X >= fromCords.X && c.X <= toCords.X && c.Y >= fromCords.Y && c.Y <= toCords.Y -> updateCell(cell)
            | _ -> cell
        grid |> List.map shouldUpdate        