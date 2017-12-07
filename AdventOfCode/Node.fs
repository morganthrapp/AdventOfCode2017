module Node

open System.Collections.Generic
open System.Text.RegularExpressions
open System

type Node private(name: string) =
    static let (|Regex|_|) pattern input = 
        let m = Regex.Match (input, pattern)
        match m.Success with
        | true -> Some(List.tail [for g in m.Groups -> g.Value])
        | false -> None
    
    static let rec topNode (node: Node) : option<Node> =
        match node.Parent with
        | None -> Some(node)
        | Some(n) -> topNode n

    static let mutable nodes = new Dictionary<string, Node>()
    let mutable parent : option<Node> = None
    let mutable children = Array.empty<Node>
    let mutable weight = 0

    static member getOrCreate (name: string) : Node =
        let n = name.Trim ' '
        if not (nodes.ContainsKey n) then
            nodes.Add(n, new Node(n))
        nodes.[n]

    static member fromLine (line: string) =
        match line with
        | Regex @"^(\w+)\s\((\d+)\)(?:.*?)([\w,].*)" [name; weight; children] -> 
            let node = Node.getOrCreate name
            node.Weight <- Int32.Parse weight
            children.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.iter node.AddChild
            node
        | Regex @"^(\w+)\s\((\d+)\)(?:.*?)" [name; weight] -> 
            let node = Node.getOrCreate name
            node.Weight <- Int32.Parse weight
            node
        | _ -> raise (ArgumentException(line))
        

    override this.GetHashCode() =
        hash (this.Name, this.Weight, this.Children)

    override this.Equals(v) =
        match v with
        | :? Node as n -> (this.Name, this.Weight, this.Children) = (n.Name, n.Weight, n.Children)
        | _ -> false
        
    member __.Name = name
    member __.Children
        with get() = children
        and private set(v) = children <- Array.append children v
    member __.Weight
        with get() = weight
        and set(v) = weight <- v
    member __.Parent
        with get() = parent
        and set(v) = parent <- v
    member this.RWeight = this.Weight + (Seq.map (fun (c: Node) -> c.RWeight) this.Children |> Seq.sum)
    member this.Top = topNode(this)

    
    member this.Siblings = 
        match this.Parent with
        | None -> Array.empty<Node>
        | Some(v) -> Array.where (fun n -> not (n = this)) v.Children

    member this.AddChild (name: string) =
        let child = Node.getOrCreate(name)
        child.Parent <- Some(this)
        this.Children <- [|child|]
        nodes.Remove name |> ignore
        nodes.Add(name, child)

    



