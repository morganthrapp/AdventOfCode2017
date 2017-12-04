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

[<EntryPoint>]
let main argv = 
    let challenge = readInt()
    let result = match challenge with
                 | 1 -> firstChallenge()
                 | 2 -> secondChallenge()
                 | 3 -> thirdChallenge()
                 | 4 -> fourthChallenge()
                 | 5 -> fifthChallenge()
                 | 6 -> sixthChallenge()
                 | _ -> raise (NotSupportedException())
    printfn "%A" result
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code