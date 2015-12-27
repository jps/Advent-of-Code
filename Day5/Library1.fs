namespace Day5

(*

--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or nice.

A nice string is one with all of the following properties:

    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
    It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

For example:

    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
    aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
    jchzalrnumimnmhp is naughty because it has no double letter.
    haegwjzuvuyypxyu is naughty because it contains the string xy.
    dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?


*)

type StringValidator() = 
    let minVowelCount = 3
    let vowels = ['a';'e';'i';'o';'u']
    let badPairs = ["ab";"cd";"pd";"xy"]
    let ContainsRequiredVowels (str:string) = 
        str |> Seq.where(fun char -> vowels |> List.contains(char))
            |> Seq.length >= minVowelCount
    let ContainsAPair (str:string) = 
        let chars = str.ToCharArray() |> Array.toList
        let rec anyPair (arr:char List) =
            match arr with
            | a when a.Length = 1 -> false
            | a when a.Head = a.Tail.Head -> true
            | _ -> anyPair(arr.Tail)
        anyPair chars
    let ContainsBadPair (str:string) =
       badPairs  |> List.exists(fun x -> str.Contains(x.ToString()))
    member this.isStringValid (str:string) = 
        match str with
        | s when ContainsRequiredVowels s && ContainsAPair s && not(ContainsBadPair s) -> true
        | _ -> false    


type StringValidator2() = 
    let containsRepeatedPair (str:string) = 
        let chars = str.ToCharArray() |> Array.toList
        let rec repeatedPair (arr:char List) =
            match arr with
            | a when a.Length <= 3 -> false
            | a when System.String.Concat(Array.ofList(a.Tail.Tail)).Contains(new string([|a.Head; a.Tail.Head|])) -> true
            | _ -> repeatedPair(arr.Tail)
        repeatedPair chars 
    let anyTriples(str:string) = 
            let chars = str.ToCharArray() |> Array.toList
            let rec anyTrips (arr:char List) =
                match arr with
                | a when a.Length <= 2 -> false
                | a when a.Head = a.Tail.Head && a.Head = a.Tail.Tail.Head -> true
                | _ -> anyTrips arr.Tail
            anyTrips chars
    let containsSandwichedChar(str:string) = 
            let chars = str.ToCharArray() |> Array.toList
            let rec sandwitch (arr:char List) =
                match arr with
                | a when a.Tail.Length < 2 -> false
                | a when a.Head = a.Tail.Tail.Head -> true
                | _ -> sandwitch arr.Tail
            sandwitch chars
    member this.isStringValid (str:string) = 
        match str with
        | s when containsRepeatedPair s && not(anyTriples s) && containsSandwichedChar s -> true
        | _ -> false    
