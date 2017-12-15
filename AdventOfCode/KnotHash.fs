module KnotHash


let (|GreaterThan|LessThan|) (test, target) = if test > target then GreaterThan else LessThan


type CyclicListValue<'a> =
    Nil | Cons of 'a * Lazy<CyclicList<'a>>
and CyclicList<'a>(value: CyclicListValue<'a>) = 
    member __.Value = value

let map f (cl: CyclicList<_>) =
    let rec mapAux start (l: CyclicList<_>) lazyRes =
        match l.Value with
        | Nil -> new CyclicList<_>(Nil)
        | Cons(v, rest) when rest.Value = start -> lazyRes()
        | Cons(v, rest) ->
            let value = Cons(f v, lazy mapAux start rest.Value lazyRes)
            new CyclicList<_>(value)
    let rec res = mapAux cl cl (fun () -> res)
    res
    

//type KnotHasher(lengths: int[]) =
//    let mutable skipSize = 0
//    let mutable currentPosition = 0
//    let mutable numberList = [0..255]
//    let numbers = CyclicList<int[]>(CyclicListValue<numberList>)
    
//    let next (length: int) =
//        let currentSlice = numbers |> Seq.skip currentPosition |> Seq.take length
//        let reversed = Seq.rev currentSlice |> Seq.toList
//        numberList <- List.concat [|reversed; (Seq.toList (Seq.skip length numberList))|]
//        currentPosition <- currentPosition + length + skipSize
//        skipSize <- skipSize + 1

//    do Seq.iter next lengths

