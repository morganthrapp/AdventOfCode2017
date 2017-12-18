module Utilities

open System
open System.IO
open System.Text.RegularExpressions



let inputFiles = "C:\FSharpWorkspace\AdventOfCode\AdventOfCode\InputFiles"
let read() = stdin.ReadLine()
let readInt() = Int32.Parse(read())
let (+/) path1 path2 = Path.Combine(path1, path2)
let readChallengeInput challengeNumber =
    let challenge = challengeNumber.ToString()
    if File.Exists(inputFiles +/ challenge) then File.ReadAllText(inputFiles +/ challenge) else stdin.ReadLine()

let splitLines (lines: string )= lines.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)

let (|EmptySeq|_|) a = if Seq.isEmpty a then Some() else None
            
type Direction = | Up | Left | Down | Right

let firstEvenlyDivisable number (data: seq<int>) : option<float> = 
    let n = float number
    match data with
        | EmptySeq -> None
        | _ -> data 
                |> Seq.map float
                |> Seq.map (fun i -> if i > n then i / n else n / i ) 
                |> Seq.tryFind (fun x -> Math.Ceiling x = x)

let spreadsheet (s: string) = 
    splitLines s
    |> Seq.map (fun(d: string) -> d.Split([|' ';'\t'|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse)

let spiralPosition input : int * int =
    match (input - 1) with
        | 0 -> (0, 0)
        | _ ->
            let r = int (floor ((sqrt (float (input - 1) + float 1)) - float 1) / float 2) + 1
            let p = 8 * r * (r - 1) / 2
            let en = r * 2
            let a = (1 + (input - 1) - p) % (r * 8)
            match int (floor (float (a / (r * 2)))) with
                | 0 -> (a - r, -r)
                | 1 -> (r, (a % en) - r)
                | 2 -> (r - (a % en), r)
                | 3 -> (-r, r - (a % en))
                | _ -> raise (IndexOutOfRangeException())


let generateSpiralSum position = 
    let spiral = Array2D.create position position 0

    let generateInitialCoord() = match position with | 2 -> 0 | _ -> position / 2
    let y = generateInitialCoord()
    let x = generateInitialCoord()
    spiral.[y, x] <- 1
    let neighbors = [|
        [|1; 1|]
        [|1; 0|]
        [|1; -1|]
        [|0; 1|]
        [|0; -1|]
        [|-1; 1|]
        [|-1; 0|]
        [|-1; -1|]
    |]
    let isValidCoord c = 0 <= c && c < position
    let isValidCoordPair y x = isValidCoord y && isValidCoord x
    let leftTurn = function | Up -> Left | Left -> Down | Down -> Right | Right -> Up
    let dirOffset direction = 
        match direction with
        | Up -> (-1, 0)
        | Left -> (0, -1)
        | Down -> (1, 0)
        | Right -> (0, 1)

    let rec nextSpiral y x direction counter =
        if isValidCoordPair x y && counter <= (position * position)
        then
            let r = neighbors 
                    |> Seq.where (fun n -> isValidCoordPair (y + n.[0]) (x + n.[1]))
                    |> Seq.sumBy (fun n -> spiral.[y + n.[0], x + n.[1]]) 

            spiral.[y, x] <- match r with | 0 -> 1 | _ -> r 
            let turnLeft = direction |> leftTurn
            let (newDy, newDx) = turnLeft |> dirOffset
            let newY = y + newDy
            let newX = x + newDx
            let shouldTurn = isValidCoordPair newY newX && spiral.[newY, newX] = 0
            let nextCount = counter + 1
            if shouldTurn
            then
                nextSpiral newY newX turnLeft nextCount
            else
                let (dy, dx) = direction |> dirOffset
                nextSpiral (y + dy) (x + dx) direction nextCount
        else
            spiral

    nextSpiral y x Right 0

let spiralSum position =
    match position with
    | 0 -> Seq.cast<int> [|0|]
    | 1 -> Seq.cast<int> [|1|]
    | _ -> generateSpiralSum position   


let firstSeenPosition input  =     
    let mutable cycleCount = 0
    let mutable seenConfigurations = [Array.copy input]
    let mutable currentConfiguration = Array.copy input
    while (seenConfigurations.Length - 1) = cycleCount  || cycleCount = 0 do
        let blocks = Seq.max currentConfiguration
        let maxIndex = Seq.findIndex (fun v -> v = blocks) currentConfiguration
        currentConfiguration.[maxIndex] <- 0
        Seq.init blocks (fun _ -> 1)
        |> Seq.mapi (fun i _ -> (i + maxIndex + 1) % currentConfiguration.Length)
        |> Seq.iter (fun v -> currentConfiguration.[v] <- currentConfiguration.[v] + 1)
        if not (List.exists (fun e -> e = currentConfiguration) seenConfigurations) then
            seenConfigurations <- List.append seenConfigurations [Array.copy currentConfiguration]
        cycleCount <- cycleCount + 1
    (cycleCount, seenConfigurations.[seenConfigurations.Length - 1])

let (|Regex|_|) pattern input = 
    let m = Regex.Match (input, pattern)
    match m.Success with
    | true -> Some(List.tail [for g in m.Groups -> g.Value])
    | false -> None