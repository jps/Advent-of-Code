namespace Day7
open System
open System.Collections.Generic 
open Microsoft.FSharp.Core

(*
--- Day 7: Some Assembly Required ---

This year, Santa brought little Bobby Tables a set of wires and bitwise logic gates! Unfortunately, little Bobby is a 
little under the recommended age range, and he needs help assembling the circuit.

Each wire has an identifier (some lowercase letters) and can carry a 16-bit signal (a number from 0 to 65535). A signal
is provided to each wire by a gate, another wire, or some specific value. Each wire can only get a signal from one
source, but can provide its signal to multiple destinations. A gate provides no signal until all of its inputs have a signal.

The included instructions booklet describes how to connect the parts together: x AND y -> z means to connect wires x 
and y to an AND gate, and then connect its output to wire z.

For example:

    123 -> x means that the signal 123 is provided to wire x.
    x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
    p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
    NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.

Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for some reason, you'd like to emulate the
circuit instead, almost all programming languages (for example, C, JavaScript, or Python) provide operators for these
gates.

For example, here is a simple circuit:

123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i

After it is run, these are the signals on the wires:

d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456

In little Bobby's kit's instructions booklet (provided as your puzzle input), what signal is ultimately provided to
wire a?
*)




type Command =
    |VAL of UInt16
    |NOT of string
    |AND of string * string
    |VAND of UInt16 * string
    |OR of string * string
    |POINTER of string
    |LSHIFT of string * int
    |RSHIFT of string * int
type Parser() =         
    let isValue (cmdString:string) = 
        let potentialVal = cmdString.Split([|"->"|], StringSplitOptions.None).[0].Trim().Split(' ').[0]            
        let (isValue ,_) = UInt16.TryParse(potentialVal)
        isValue
    let CreateCommand (cmdString:string) : (string * Command) = 
        let parts = cmdString.Split([|"->"|], StringSplitOptions.None)
        let cmd = parts.[0]
        let assignTo = parts.[1].ToString().Trim()
        let cmdParts = cmd.Split(' ')
        let command = match cmdString with
                        | c when c.Contains("NOT") -> NOT(cmdParts.[1])
                        | c when c.Contains("AND") && isValue(c) -> VAND(UInt16.Parse(cmdParts.[0]),cmdParts.[2])
                        | c when c.Contains("AND") -> AND(cmdParts.[0], cmdParts.[2])
                        | c when c.Contains("OR") -> OR(cmdParts.[0], cmdParts.[2])
                        | c when c.Contains("LSHIFT") -> LSHIFT(cmdParts.[0], Int32.Parse(cmdParts.[2]))
                        | c when c.Contains("RSHIFT") -> RSHIFT(cmdParts.[0], Int32.Parse(cmdParts.[2]))
                        | c when isValue(cmdString) ->  VAL(UInt16.Parse(cmd))
                        | _ -> POINTER(cmdParts.[0])
        (assignTo, command)
    let memorise f =
        let cache = Dictionary()
        fun x ->
        match cache.TryGetValue(x) with
        | true, v -> v
        | _ -> 
            let v = f x
            cache.Add(x, v)
            v
    let rec EvalCommand (command:Command) (map:Map<string,Command>) (n:int) =
        match command with
        | VAL(v) -> v 
        | POINTER(pointTo) -> let pointer = (EvalCommand map.[pointTo] map (n+1))
                              pointer
        | NOT(applyTo) ->   let evauated = (EvalCommand map.[applyTo] map (n+1))
                            (~~~)evauated //~~~16us
        | VAND(vala, b) ->  let vand = (vala &&& (EvalCommand map.[b] map (n+1)))
                            vand
        | AND(a, b) ->      let annd = ((EvalCommand map.[a] map (n+1)) &&& (EvalCommand map.[b] map (n+1)))//eval(map.[a],map) &&& eval(map.[b],map) &&&
                            annd
        | OR(a, b) -> ((EvalCommand map.[a] map (n+1)) ||| (EvalCommand map.[b] map (n+1)))
        | LSHIFT(applyTo, nBits) -> ((EvalCommand map.[applyTo] map (n+1)) <<< nBits)//failwith "Not implemented yet" <<<
        | RSHIFT(applyTo, nBits) -> ((EvalCommand map.[applyTo] map (n+1)) >>> nBits)//failwith "Not implemented yet" >>>
    let EvalSimple (command:Command) (cache:Dictionary<string,UInt16>) =
        match command with
        | VAL(v) -> Some(v)
        | POINTER(pointTo) when cache.ContainsKey(pointTo) -> Some(cache.[pointTo])
        | NOT(applyTo) when cache.ContainsKey(applyTo) -> Some(~~~cache.[applyTo])
        | VAND(vala, b) when cache.ContainsKey(b) -> Some(vala &&& cache.[b])
        | AND(a, b) when cache.ContainsKey(a) && cache.ContainsKey(b) -> Some(cache.[a] &&& cache.[b])
        | OR(a, b) when cache.ContainsKey(a) && cache.ContainsKey(b) -> Some(cache.[a] ||| cache.[b])
        | LSHIFT(applyTo, nBits) when cache.ContainsKey(applyTo) -> Some(cache.[applyTo] <<< nBits)
        | RSHIFT(applyTo, nBits) when cache.ContainsKey(applyTo) -> Some(cache.[applyTo] >>> nBits)
        | _ -> None
    member this.BuildCommandMap (commands:string seq) =
        Seq.fold(fun (acc:Map<string,Command>) x -> (acc.Add(CreateCommand x))) Map.empty<string,Command> commands
    member this.EvalCommands (commandList:Map<string,Command> ) = 
        commandList |> Map.map(fun key value -> (key, EvalCommand value commandList 0))
    member this.EvalSingle (key:string) (commandList:Map<string,Command>) = 
        let command = commandList.[key]//(fun key value -> (key, EvalCommand value commandList))
        EvalCommand command commandList 0    
    member this.SimpleEvalCommands (commandList:Map<string,Command>) = 
        let cache = Dictionary<string,UInt16>()
        while(cache.Count < commandList.Count) do
            for c in commandList do
                if(not (cache.ContainsKey(c.Key))) then
                    let evaluatedValue = (EvalSimple c.Value cache)
                    match evaluatedValue with
                    | Some(v) -> cache.Add(c.Key, v)
                    | _ -> ()
        cache



//    let rec evaluateCommandMap (command:command)= 
//        
//
//
//    member this.X = "F#"
