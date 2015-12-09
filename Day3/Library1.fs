namespace Day3
open System
(*--- Day 3: Perfectly Spherical Houses in a Vacuum ---

Santa is delivering presents to an infinite two-dimensional grid of houses.

He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him 
via radio and tells him where to move next. Moves are always exactly one house to the north (^), south (v), east (>), 
or west (<). After each move, he delivers another present to the house at his new location.

However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and 
Santa ends up visiting some houses more than once. How many houses receive at least one present?

For example:

    > delivers presents to 2 houses: one at the starting location, and one to the east.
    ^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
    ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.
*)

type Movement =
    | Up = '^'
    | Right = 'V'
    | Down = '>'
    | Left = '<' 

type Mover() = 
    let (|Up|Down|Left|Right|) direction =
            match direction with
                | '^' -> Up
                | '>' -> Right
                | 'V' -> Down
                | '<' -> Left
    let mutable x = 0
    let mutable y = 0
    static let initVisited = [(0, 1)] 
    member this.Move (currentMoves: (int * int) list) movement = 
        let (lastx,lasty) = currentMoves.Head
        let current = match movement with
                        | Up ->     (lastx, lasty + 1)
                        | Right ->  (lastx + 1, lasty)
                        | Down ->   (lastx, lasty - 1)
                        | Left ->   (lastx - 1,lasty)
        current :: currentMoves
    member this.blah () =
        let posix = this.Move initVisited '>'
        ()

    member this.X = "F#"
    
