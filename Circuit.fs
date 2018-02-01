module Circuit

type Gene = string
type Circuit = Value of bool | Node of Gene | And of Circuit * Circuit | Or of Circuit * Circuit | Not of Circuit

let inhibition pre activ = And (activ, Not pre)
let andList cs = Seq.reduce (fun x y -> And (x, y)) cs
let orList cs = Seq.reduce (fun x y -> Or (x, y)) cs

let rec variables circuit =
    match circuit with
    | Node v -> set [v]
    | Value _ -> Set.empty
    | Not c -> variables c
    | And (c1, c2) | Or (c1, c2) -> variables c1 + variables c2

let rec printCircuit c =
    match c with
    | Value b -> string b
    | And (c1, c2) -> sprintf "And(%s, %s)" (printCircuit c1) (printCircuit c2)
    | Or (c1, c2) -> sprintf "Or(%s, %s)" (printCircuit c1) (printCircuit c2)
    | Not c -> sprintf "Not %s" (printCircuit c)
    | Node name -> name

// makes a transition iff all sub-models do
let combineCircuits gene cs =
    Or (And (Not (Node gene), andList cs),
        And (Node gene, orList cs))
