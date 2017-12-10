let IsPassPhraseValid (passphrase:string) = 
    let words = passphrase.Split()
    let originalCount = words.Length
    let distinctCount = words |> Array.toSeq |> Seq.distinct |> Seq.length
    originalCount = distinctCount

let testCases = [
    ("aa bb cc dd ee", true);
    ("aa bb cc dd aa", false);
    ("aa bb cc dd aaa", true)]

for (input, expected) in testCases do
            let result = IsPassPhraseValid(input)
            printfn "input:%s expected:%b actual:%b match:%b" input expected result (result = expected)

let UseProvidedInputData =
    let passPhrases = System.IO.File.ReadLines("Day4.input.txt")
    let validPhrases = passPhrases |> Seq.filter IsPassPhraseValid |> Seq.length                
    printfn "result:%i" validPhrases
    ()