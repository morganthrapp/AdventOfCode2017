open System
open System.IO

let inputFiles = "C:\FSharpWorkspace\AdventOfCode\AdventOfCode\InputFiles"
let read() = stdin.ReadLine()
let readInt() = Int32.Parse(read())
let (+/) path1 path2 = Path.Combine(path1, path2)
let readChallengeInput challengeNumber =
    let challenge = challengeNumber.ToString()
    if File.Exists(inputFiles +/ challenge) then File.ReadAllText(inputFiles +/ challenge) else stdin.ReadLine()

let (|EmptySeq|_|) a = if Seq.isEmpty a then Some() else None

let firstEvenlyDivisable number (data: seq<int>) : option<float> = 
    let n = float number
    match data with
        | EmptySeq -> None
        | _ -> data 
                |> Seq.map float
                |> Seq.map (fun i -> if i > n then i / n else n / i ) 
                |> Seq.tryFind (fun x -> Math.Ceiling x = x)

let spreadsheet (s: string) = 
    s.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries) 
    |> Seq.map (fun(d: string) -> d.Split([|' ';'\t'|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse)

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

[<EntryPoint>]
let main argv = 
    let challenge = readInt()
    let result = match challenge with
                 | 1 -> firstChallenge()
                 | 2 -> secondChallenge()
                 | 3 -> thirdChallenge()
                 | 4 -> fourthChallenge()
                 | _ -> raise (NotSupportedException())
    printfn "%A" result
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code