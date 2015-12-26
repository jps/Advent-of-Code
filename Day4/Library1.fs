﻿namespace Day4
open System
open System.Text
open System.Security.Cryptography
(*
--- Day 4: The Ideal Stocking Stuffer ---

Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically 
forward-thinking little girls and boys.

To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5
 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you
  must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.

For example:

    If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes
     (000001dbbfa...), and it is the lowest such number to do so.
    If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five 
    zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
*)

module Miner = 
    let searchLowerBound = 0
    let searchUpperBound = Int32.MaxValue

    let GetMd5HashOfString (input:string) = 
        let hasher = MD5.Create() //should be passed so it can be reused
        let inputAsBytes = Encoding.UTF8.GetBytes(input)
        hasher.ComputeHash(inputAsBytes)

    let toHexString (hash: byte[]) =        
        BitConverter.ToString(hash).Replace("-","");

    let rec findFistMatchForKey (key:string, startsWith:string, n:int) : int =
        match n with
        | x when x > searchUpperBound -> -1
        | _ -> let checkString = sprintf "%s%i"  key n
               let hashAsHex = (GetMd5HashOfString checkString) |> toHexString 
               let isMatch = hashAsHex.StartsWith(startsWith)        
               match isMatch with 
                | true -> n
                | false -> findFistMatchForKey(key, startsWith, n+1)