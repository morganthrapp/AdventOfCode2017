open System
open System.IO

let inputFiles = "C:\FSharpWorkspace\AdventOfCode\AdventOfCode\InputFiles"
let read() = stdin.ReadLine()
let readInt() = Int32.Parse(read())
let (+/) path1 path2 = Path.Combine(path1, path2)
let readChallengeInput challengeNumber = if File.Exists(inputFiles +/ challengeNumber) then File.ReadAllText(inputFiles +/ challengeNumber) else stdin.ReadLine()

let firstChallenge() = 
    let input = readChallengeInput "1"
    let mutable previousChar = input.Chars(input.Length - 1)
    let mapper c = 
        let retVal = if c = previousChar then Char.GetNumericValue c else 0.0
        previousChar <- c
        retVal
    let total = input |> Seq.map mapper|> Seq.sum |> int
    printfn "%A" total

let secondChallenge() =
    let input = readChallengeInput "2"
    let mutable total = 0
    let mapper i c = 
        let retVal = int (if c = input.[(i + (input.Length / 2)) % input.Length] then Char.GetNumericValue c else 0.0)
        total <- total + retVal
    String.iteri mapper input
    printfn "%A" total



[<EntryPoint>]
let main argv = 
    let challenge = readInt()
    match challenge with
        | 1 -> firstChallenge()
        | 2 -> secondChallenge()
        | _ -> raise (NotSupportedException())
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code