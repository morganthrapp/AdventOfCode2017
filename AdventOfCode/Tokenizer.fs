module Patterns

type Tokenizer(instructions: string) =
    let mutable depth = 1;
    let mutable groups = 0;
    let mutable score = 0;
    let mutable pointer = 0;
    let mutable garbageCount = 0;
    let mutable ignoreNext = false;
    let mutable isGarbageBlock = false;
    
    let next() =
        if ignoreNext
            then ignoreNext <- false
        else
            match instructions.[pointer] with
            | '{' -> if not isGarbageBlock 
                        then depth <- depth + 1
                        else garbageCount <- garbageCount + 1
            | '}' -> if not isGarbageBlock 
                        then 
                            depth <- depth - 1
                            groups <- groups + 1
                            score <- depth + score
                        else
                            garbageCount <- garbageCount + 1
            | '!' -> ignoreNext <- true
            | '<' -> if isGarbageBlock 
                        then garbageCount <- garbageCount + 1
                        else isGarbageBlock <- true
            | '>' -> isGarbageBlock <- false
            | _ -> if isGarbageBlock then garbageCount <- garbageCount + 1
        pointer <- pointer + 1

    do while pointer < instructions.Length do next()

    member __.Groups = groups
    member __.Score = score
    member __.GarbageCount = garbageCount

