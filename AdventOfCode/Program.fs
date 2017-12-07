open System
open System.IO
open System.Text.RegularExpressions
open Node

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
                 | _ -> raise (NotSupportedException())
    printfn "%A" result
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code