open System
open System.IO
open Node
open Utilities
open Instruction
open System.Collections.Generic

let firstChallenge() = 
    let input = readChallengeInput 1
    let mutable previousChar = input.Chars(input.Length - 1)
    let mapper c = 
        let retVal = if c = previousChar then Char.GetNumericValue c else 0.0
        previousChar <- c
        retVal
    input |> Seq.map mapper|> Seq.sum |> int

let secondChallenge() =
    let input = readChallengeInput 2
    let mutable total = 0
    let mapper i c = 
        let retVal = int (if c = input.[(i + (input.Length / 2)) % input.Length] then Char.GetNumericValue c else 0.0)
        total <- total + retVal
    String.iteri mapper input
    total

let thirdChallenge() =
    let input = spreadsheet (readChallengeInput 3)
    let rowChecksums = input |> Seq.map (fun(r: seq<int>) -> Seq.max r - Seq.min r)
    Seq.sum rowChecksums

let fourthChallenge() =
    let input = spreadsheet (readChallengeInput 3)
    let rowChecksums = 
        input 
        |> Seq.collect (fun (row: seq<int>) -> Seq.mapi (fun i n -> firstEvenlyDivisable n (Seq.skip (i + 1) row)) row) 
        |> Seq.map (fun a -> match a with Some r -> r | None -> 0.0) 
        |> Seq.map int
    Seq.sum rowChecksums

let fifthChallenge() =
    let input = Int32.Parse (readChallengeInput 5)
    let position = spiralPosition input
    abs position.[0] + abs position.[1]

let sixthChallenge() =
    let input = Int32.Parse (readChallengeInput 5)
    Seq.init 10 id 
    |> Seq.map (fun i -> spiralSum i) 
    |> Seq.collect id 
    |> Seq.sortBy id
    |> Seq.find (fun v -> v > input)

let seventhChallenge() =
    let input = splitLines (readChallengeInput 7)
    input
    |> Seq.where (fun (p: string) -> 
        let password = p.Split [|' '|]  
        Seq.length (Seq.distinct password) = Seq.length password)
    |> Seq.length

let eighthChallenge() =
    let input = splitLines (readChallengeInput 7)
    input
    |> Seq.where (fun (p: string) -> 
        let password = p.Split [|' '|]  
        let anagrams = password |> Seq.map (fun a -> Seq.sort [for c in a -> c] |> Seq.toList) |> Seq.toList
        let distinct = Seq.distinct anagrams
        Seq.length distinct = Seq.length password)
    |> Seq.length

let ninthChallenge() = 
    let mutable input = 
        readChallengeInput 9
        |> splitLines
        |> Seq.map Int32.Parse 
        |> Seq.toArray
    let exit = input.Length
    let mutable pointer = 0
    let mutable moves = 0
    while pointer < exit do
        let move = input.[pointer]
        input.[pointer] <- move + 1
        pointer <- pointer + move
        moves <- moves + 1
    moves

let tenthChallenge() =
    let mutable input = 
        readChallengeInput 9
        |> splitLines
        |> Seq.map Int32.Parse 
        |> Seq.toArray
    let exit = input.Length
    let mutable pointer = 0
    let mutable moves = 0
    while pointer < exit do
        let move = input.[pointer]
        input.[pointer] <- move + (if move > 2 then -1 else 1)
        pointer <- pointer + move
        moves <- moves + 1
    moves

let eleventhChallenge() = 
    let input = 
        readChallengeInput 11
        |> (fun s -> s.Split [|' ';'\t'|])
        |> Seq.map Int32.Parse
        |> Seq.toArray
    let count, _ = firstSeenPosition input
    count

let twelfthChallenge() = 
    let input = 
        readChallengeInput 11
        |> (fun s -> s.Split [|' ';'\t'|])
        |> Seq.map Int32.Parse
        |> Seq.toArray
    let _, finalConfiguration = firstSeenPosition input
    let count, _ = firstSeenPosition finalConfiguration
    count

let thirteenthChallenge() = 
    let input = Seq.map Node.fromLine (splitLines (readChallengeInput 13)) 
    let node = Seq.head (input |> Seq.map (fun (n: Node) -> (Node.getOrCreate n.Name)))
    match node.Top with
        | Some(v) -> v.Name
        | None -> raise (InvalidDataException())

let fifteenthChallenge() =
    let input = splitLines (readChallengeInput 15)
    let mutable registers = new Dictionary<string, int>()
    Seq.iter (fun instruction -> registers <- (new Instruction(instruction)).Evaluate(registers)) input
    Seq.max registers.Values    

let sixteenthChallenge() =
    let input = splitLines (readChallengeInput 15)
    let mutable registers = new Dictionary<string, int>()
    let mutable highest = 0
    let evalWithHighest instruction =
        let instruction = new Instruction(instruction)
        let register = instruction.Evaluate(registers)
        highest <- Seq.max [(Seq.max register.Values); highest]
    Seq.iter evalWithHighest input  
    highest
        

[<EntryPoint>]
let main argv = 
    let challenge = readInt()
    let (result: obj) = match challenge with
                 | 1 -> upcast firstChallenge()
                 | 2 -> upcast secondChallenge()
                 | 3 -> upcast thirdChallenge()
                 | 4 -> upcast fourthChallenge()
                 | 5 -> upcast fifthChallenge()
                 | 6 -> upcast sixthChallenge()
                 | 7 -> upcast seventhChallenge()
                 | 8 -> upcast eighthChallenge()
                 | 9 -> upcast ninthChallenge()
                 | 10 -> upcast tenthChallenge()
                 | 11 -> upcast eleventhChallenge()
                 | 12 -> upcast twelfthChallenge()
                 | 13 -> upcast thirteenthChallenge()
                 | 15 -> upcast fifteenthChallenge()
                 | 16 -> upcast sixteenthChallenge()
                 | _ -> raise (NotSupportedException())
    printfn "%A" result
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code