module Instruction

open Utilities
open System
open System.IO
open System.Collections.Generic

type OperatorEnum = Eq | Neq | Gt | Lt | Gte | Lte
        
type Operator(op: string) =
    let operator = 
        match op with
            | "<" -> OperatorEnum.Lt
            | "<=" -> OperatorEnum.Lte
            | ">" -> OperatorEnum.Gt
            | ">=" -> OperatorEnum.Gte
            | "==" -> OperatorEnum.Eq
            | "!=" -> OperatorEnum.Neq
            | _ -> raise(InvalidDataException(op))
    member __.Op = operator

type Condition(line: string) =

    let (left, op, right) = 
        match line with
        | Regex @"(\w+)\s+(.*?)\s+(.+)" [ leftRegister; operator; rightValue ] -> (leftRegister, operator, rightValue)
        | _ -> raise(InvalidDataException(line))

    member __.Evaluate(registers: Dictionary<string, int>) =
        if not (registers.ContainsKey left)
            then registers.Add(left, 0)
        let leftVal = registers.[left]
        let rightVal = Int32.Parse right
        let operator = Operator(op)
        let comp = 
            match operator.Op with
            | OperatorEnum.Lt -> fun l r -> l < r
            | OperatorEnum.Lte -> fun l r -> l <= r
            | OperatorEnum.Gt -> fun l r -> l > r
            | OperatorEnum.Gte -> fun l r -> l >= r
            | OperatorEnum.Eq -> fun l r -> l = r
            | OperatorEnum.Neq -> fun l r -> not (l = r)
        comp leftVal rightVal

type Instruction(line: string) =    
    let (register, direction, amt, cond) =
        match line with
        | Regex @"(\w+)\s+(\w+)\s+(.*?)\s+if (.*)$" [ register; direction; amt; cond ] -> (register, direction, amt, cond)
        | _ -> raise(InvalidDataException(line))
    
    member __.Evaluate(registers: Dictionary<string, int>) =
        if not (registers.ContainsKey register)
            then registers.Add(register, 0)
        let amount = Int32.Parse amt
        let currentVal = registers.[register]
        let condition = new Condition(cond)
        match condition.Evaluate(registers) with
        | true -> match direction with
                  | "inc" -> registers.[register] <- currentVal + amount
                  | "dec" -> registers.[register] <- currentVal - amount
                  | _ -> raise(InvalidDataException(direction))
        | false -> ()
        registers

