
let stringToCharList input = 
    [for c in input -> c]

let charsToString listOfChars =
    new System.String(listOfChars |> Array.ofList) 

let IsPassPhraseValid (passphrase:string) = 
    let words = passphrase.Split() 
    let wordsAsOrderedCharList = words |> Array.map (stringToCharList >> List.sort)
    let originalCount = words.Length
    let distinctCount = wordsAsOrderedCharList |> Array.toSeq |> Seq.distinct |> Seq.length
    originalCount = distinctCount

let testCases = [
    ("abcde fghij", true);
    ("abcde xyz ecdab", false);
    ("a ab abc abd abf abj", true)
    ("iiii oiii ooii oooi oooo", true)
    ("oiii ioii iioi iiio", false)]

for (input, expected) in testCases do
            let result = IsPassPhraseValid(input)
            printfn "input:%s expected:%b actual:%b match:%b" input expected result (result = expected)

let UseProvidedInputData =
    let passPhrases = System.IO.File.ReadLines("Day4.input.txt")
    let validPhrases = passPhrases |> Seq.filter IsPassPhraseValid |> Seq.length                
    printfn "result:%i" validPhrases
    ()