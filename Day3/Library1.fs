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


module Mover =
    type Position(x, y) = 
        member this.X = x
        member this.Y = y
        override this.Equals(pos) =
            match pos with
            | :? Position as p -> (this.X = p.X && this.Y = p.Y)
            | _ -> false
        override this.GetHashCode() = hash (sprintf "x%iy%i"this.X this.Y)

    let initPosition() = [Position(0, 0)] 

    type Movement =
        | Up = '^'
        | Right = 'V'
        | Down = '>'
        | Left = '<' 

    let (|Up|Down|Left|Right|) direction =
                match direction with
                    | '^' -> Up
                    | '>' -> Right
                    | 'V' | 'v' -> Down
                    | '<' -> Left
                    | _ -> Up
        
    let Move (currentMoves: Position list) movement = 
        let lastPosition = currentMoves.Head
        let current = match movement with
                        | Up ->     Position(lastPosition.X,     lastPosition.Y + 1)
                        | Right ->  Position(lastPosition.X + 1, lastPosition.Y    )
                        | Down ->   Position(lastPosition.X,     lastPosition.Y - 1)
                        | Left ->   Position(lastPosition.X - 1, lastPosition.Y    )
        current :: currentMoves
    let MoveMany directionString =
        Seq.fold (fun acc dirChar -> Move acc dirChar) (initPosition()) directionString



//    member this.blah () =
//        let posix = this.Move initVisited '>'
//        ()    
