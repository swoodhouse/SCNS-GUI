module StableStates

open SAT
open Microsoft.Z3
open Data
open Circuit
open FSharpx.Collections

let private varName name = "v_" + name

let private circuitToConstraint (solver : SATSolver) c =
    let rec circuitToConstraint c =
        match c with
        | Value true -> solver.True
        | Value false -> solver.False
        | And (c1, c2) -> solver.And (circuitToConstraint c1, circuitToConstraint c2)
        | Or (c1, c2) -> solver.Or (circuitToConstraint c1, circuitToConstraint c2)
        | Not c -> solver.Not (circuitToConstraint c)
        | Node name -> solver.Bool (varName name)
    circuitToConstraint c

let private isFixpoint (solver : SATSolver) (geneName, circuit) =
    solver.Eq(circuitToConstraint solver circuit, solver.Bool <| varName geneName)
    
let private constraintsBool (solver : SATSolver) (m : Model) (d : FuncDecl) =
    let x = System.Boolean.Parse(m.[d].ToString())
    if x then solver.Not (solver.Bool (d.Name.ToString()))
    else solver.Bool (d.Name.ToString())

let private constraints'' (solver : SATSolver) (m : Model) (ds : FuncDecl []) =
    solver.Or <| Array.map (constraintsBool solver m) ds

// makes a transition iff all sub-models do
let combineCircuits gene cs =
    Or (And (Not (Node gene), andList cs),
        And (Node gene, orList cs))

let stableStates model =
    use solver = new SATSolver()
    solver.Add(solver.And <| Seq.map (isFixpoint solver) (Map.toSeq model))

    let mutable i = 0
    [ while solver.Check() = Status.SATISFIABLE && i < 20 do
          i <- i + 1
          let m = solver.Model
          let variableDecls = m.ConstDecls
                              |> Array.filter (fun (d : FuncDecl) -> d.Name.ToString().StartsWith("v_"))

          let variableAssignments = variableDecls |> Array.map (fun (d : FuncDecl) -> (d.Name.ToString().Remove(0, 2), System.Boolean.Parse <| m.[d].ToString()))
          solver.Add(constraints'' solver m variableDecls)
          yield Map.ofArray variableAssignments ]

let knockOut genes model =
    let knockOut model gene =
        if not <| Map.containsKey gene model then model
        else Map.add gene (Value false) model
    Array.fold knockOut model genes

let overExpress genes model =
    let overExpress model gene =
        if not <| Map.containsKey gene model then model
        else Map.add gene (Value true) model
    Array.fold overExpress model genes
