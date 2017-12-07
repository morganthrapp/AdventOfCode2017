module Utilities

open System
open System.IO



let inputFiles = "C:\FSharpWorkspace\AdventOfCode\AdventOfCode\InputFiles"
let read() = stdin.ReadLine()
let readInt() = Int32.Parse(read())
let (+/) path1 path2 = Path.Combine(path1, path2)
let readChallengeInput challengeNumber =
    let challenge = challengeNumber.ToString()
    if File.Exists(inputFiles +/ challenge) then File.ReadAllText(inputFiles +/ challenge) else stdin.ReadLine()

let splitLines (lines: string )= lines.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)

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
    splitLines s
    |> Seq.map (fun(d: string) -> d.Split([|' ';'\t'|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse)

let spiralPosition input =
    let mutable n = input - 1
    match n with
        | 0 -> [0; 0]
        | _ ->
            n <- n - 1
            let r = int (floor ((sqrt (float n + float 1)) - float 1) / float 2) + 1
            let p = 8 * r * (r - 1) / 2
            let en = r * 2
            let a = (1 + n - p) % (r * 8)
            let mutable left = 0
            let mutable right = 0
            match int (floor (float (a / (r * 2)))) with
                | 0 -> left <- a - r; right <- -r
                | 1 -> left <- r; right <- (a % en) - r
                | 2 -> left <- r - (a % en); right <- r
                | 3 -> left <- -r; right <- r - (a % en)
                | _ -> raise (IndexOutOfRangeException())
            [left; right]
            
let spiralSum position =
    match position with
    | 0 -> Seq.cast<int> [|0|]
    | 1 -> Seq.cast<int> [|1|]
    | _ ->

    let mutable spiral = Array2D.create position position 0

    let generateInitialCoord() = match position with | 2 -> 0 | _ -> position / 2
    let mutable y = generateInitialCoord()
    let mutable x = generateInitialCoord()
    let mutable dy = 0
    let mutable dx = -1
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
    let mutable counter = 0;
    while isValidCoordPair x y && counter <= (position * position) do
        let r = neighbors 
                |> Seq.where (fun n -> isValidCoordPair (y + n.[0]) (x + n.[1]))
                |> Seq.map (fun n -> spiral.[y + n.[0], x + n.[1]]) 
                |> Seq.sum
        spiral.[y, x] <- match r with | 0 -> 1 | _ -> r 
        let turnLeft = 
            match (dy, dx) with
            | (1, 0) -> (0, 1)
            | (0, 1) -> (-1, 0)
            | (-1, 0) -> (0, -1)
            | (0, -1) -> (1, 0)
            | _ -> raise(ArgumentOutOfRangeException())
        let (newDy, newDx) = turnLeft
        let newY = y + newDy
        let newX = x + newDx
        if isValidCoordPair newY newX && spiral.[newY, newX] = 0
            then
                y <- newY
                x <- newX
                dy <- newDy
                dx <- newDx
            else
                y <- y + dy
                x <- x + dx
        counter <- counter + 1
    let adjustCoord c = if isValidCoord c then c else if c >= position then c - 1 else c + 1
    y <- adjustCoord y
    x <- adjustCoord x
    Seq.cast<int> spiral


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