module Instruction

open Utilities
open System
open System.IO
open System.Collections.Generic

type OperatorEnum = Eq | Neq | Gt | Lt | Gte | Lte

type Register() =
    let registers = new Dictionary<string, int>()

    member __.GetRegister name =
        if not (registers.ContainsKey name)
            then registers.Add(name, 0)
        registers.[name]

    member __.SetRegister name value =
        registers.[name] <- value

    member __.Max = Seq.max registers.Values
        
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

    member __.Evaluate(register: Register) =
        let leftVal = register.GetRegister left
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
    let (left, direction, amt, cond) =
        match line with
        | Regex @"(\w+)\s+(\w+)\s+(.*?)\s+if (.*)$" [ left; direction; amt; cond ] -> (left, direction, amt, cond)
        | _ -> raise(InvalidDataException(line))
    
    member __.Evaluate(register: Register) =
        let amount = Int32.Parse amt
        let currentVal = register.GetRegister left
        let condition = new Condition(cond)
        match condition.Evaluate(register) with
        | true -> match direction with
                  | "inc" -> register.SetRegister left (currentVal + amount)
                  | "dec" -> register.SetRegister left (currentVal - amount)
                  | _ -> raise(InvalidDataException(direction))
        | false -> ()

