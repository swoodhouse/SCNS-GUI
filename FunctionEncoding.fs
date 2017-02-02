// This file needs to be refactored. And move to Left encoding
module FunctionEncoding

open Data
open SAT

let [<Literal>] private AND = 0
let [<Literal>] private OR = 1
let private NOTHING numGenes = numGenes + 2
let private GENE_IDS numGenes = seq {2 .. ((NOTHING numGenes) - 1)}
let private indexToName i geneNames = Seq.item (i - 2) geneNames
let private nameToIndex g geneNames = Seq.findIndex ((=) g) geneNames + 2

let activatorVars = set [ for i in 1..7 do yield "a" + string i ]
let repressorVars = set [ for i in 1..7 do yield "r" + string i ]

let private leftChild i = ((i + 1) * 2) - 1
let private rightChild i = leftChild i + 1

let makeCircuitVar numGenes (solver : SATSolver) = let numBits = (uint32 << ceil <| System.Math.Log(float numGenes, 2.0)) + 3u
                                                   fun name -> solver.BitVec (name, numBits)

let private variableDomains (solver : SATSolver) (lowerBound : int) (upperBound : int) (var : BitVec) =
    solver.And (solver.Ge (var, lowerBound), solver.Le(var, upperBound))

let private parentsOfNothingArentGates numGenes (solver : SATSolver) (a : BitVec []) (r : BitVec []) =
    let f c1 c2 p = solver.Implies (solver.Or(solver.Eq(c1, NOTHING numGenes), solver.Eq(c2, NOTHING numGenes)),
                                    solver.And(solver.NotEq(p, AND), solver.NotEq(p, OR)))

    let aParents = solver.And [ solver.Implies (solver.Or [solver.Eq(a.[1], NOTHING numGenes); solver.Eq(a.[2], NOTHING numGenes)],
                                                solver.And [ solver.NotEq(a.[0], AND); solver.NotEq(a.[0], OR); solver.NotEq(a.[0], NOTHING numGenes)])
                                f a.[3] a.[4] a.[1]
                                f a.[5] a.[6] a.[2] ]

    let rParents = solver.And [ f r.[1] r.[2] r.[0]
                                f r.[3] r.[4] r.[1]
                                f r.[5] r.[6] r.[2] ]
    solver.And [aParents; rParents]

let private parentsOfRestAreGates numGenes (solver : SATSolver) (a : BitVec []) (r : BitVec []) =
    let f c1 c2 p = solver.Implies (solver.Or[solver.NotEq(c1, NOTHING numGenes); solver.NotEq(c2, NOTHING numGenes)],
                                    solver.Or[solver.Eq(p, AND); solver.Eq(p, OR)])
    
    let aParents = solver.And [ f a.[1] a.[2] a.[0]
                                f a.[3] a.[4] a.[1]
                                f a.[5] a.[6] a.[2] ]

    let rParents = solver.And [ f r.[1] r.[2] r.[0]
                                f r.[3] r.[4] r.[1]
                                f r.[5] r.[6] r.[2] ]
    solver.And [aParents; rParents]

let private fixMaxInputs numGenes v (solver : SATSolver) max =
    match max with
    | 0 -> solver.Eq(makeCircuitVar numGenes solver (v + "1"), NOTHING numGenes)
    | 1 -> solver.Eq(makeCircuitVar numGenes solver (v + "2"), NOTHING numGenes)
    | 2 -> solver.Eq(makeCircuitVar numGenes solver (v + "4"), NOTHING numGenes)
    | 3 -> solver.Eq(makeCircuitVar numGenes solver (v + "6"), NOTHING numGenes)
    | _ -> solver.True

let private fixMaxActivators numGenes solver max = fixMaxInputs numGenes "a" solver max
let private fixMaxRepressors numGenes solver max = fixMaxInputs numGenes "r" solver max

let encodeUpdateFunction numGenes solver gene geneNames =
    let a = [| for i in 1..7 -> makeCircuitVar numGenes solver (sprintf "a%i" i) |]
    let r = [| for i in 1..7 -> makeCircuitVar numGenes solver (sprintf "r%i" i) |]

    let circuitEncoding = solver.And [ variableDomains solver 0 (NOTHING numGenes - 1) a.[0]
                                       Array.map (variableDomains solver 0 <| NOTHING numGenes) a.[1..2] |> solver.And
                                       Array.map (variableDomains solver 0 <| NOTHING numGenes) r.[0..2] |> solver.And
                                       Array.map (variableDomains solver 2 <| NOTHING numGenes) a.[3..6] |> solver.And
                                       Array.map (variableDomains solver 2 <| NOTHING numGenes) r.[3..6] |> solver.And
                                       parentsOfNothingArentGates numGenes solver a r
                                       parentsOfRestAreGates numGenes solver a r ]
    (circuitEncoding, a, r)
   
let private evaluateUpdateFunction (solver : SATSolver) (geneNames : seq<Gene>) (aVars : BitVec []) (rVars : BitVec []) state =
    let intermediateValueVariablesA = [| for i in 1 .. 7 -> solver.Bool <| sprintf "va%i_%s" i state.Name |]
    let intermediateValueVariablesR = [| for i in 1 .. 7 -> solver.Bool <| sprintf "vr%i_%s" i state.Name |]

    let andConstraints (symVars : BitVec []) (variables : Bool []) pi c1i c2i =
        solver.Implies (solver.Eq(symVars.[pi], AND), solver.Eq(variables.[pi], solver.And [variables.[c1i]; variables.[c2i]]))

    let orConstraints (symVars : BitVec []) (variables : Bool []) pi c1i c2i =
        solver.Implies (solver.Eq(symVars.[pi], OR),
                        solver.Eq(variables.[pi], solver.Or[variables.[c1i]; variables.[c2i]]))

    let variableConstraints (symVars : BitVec []) (intermediateVars : Bool []) =
        let f i symVar =
            [  for v in GENE_IDS (Seq.length geneNames) do
                   yield solver.Implies (solver.Eq(symVar, v), solver.Eq(intermediateVars.[i], Map.find (indexToName v geneNames) state.Values))
            ] |> solver.And
        Array.mapi f symVars |> solver.And

    let circuitValue =
        let noRepressors = solver.Eq(rVars.[0], NOTHING <| Seq.length geneNames)
        solver.If (noRepressors,
                   intermediateValueVariablesA.[0],
                   solver.And [intermediateValueVariablesA.[0]; solver.Not intermediateValueVariablesR.[0]])

    let circuitVal = solver.Bool <| sprintf "circuit_%s" state.Name

    (solver.And [ variableConstraints aVars intermediateValueVariablesA
                  variableConstraints rVars intermediateValueVariablesR
                  [ for i in 0 .. 2 -> andConstraints aVars intermediateValueVariablesA i (leftChild i) (rightChild i) ] |> solver.And
                  [ for i in 0 .. 2 -> andConstraints rVars intermediateValueVariablesR i (leftChild i) (rightChild i) ] |> solver.And
                  [ for i in 0 .. 2 -> orConstraints aVars intermediateValueVariablesA i (leftChild i) (rightChild i) ] |> solver.And
                  [ for i in 0 .. 2 -> orConstraints rVars intermediateValueVariablesR i (leftChild i) (rightChild i) ] |> solver.And
                  solver.Eq(circuitVal, circuitValue) ], circuitVal)

let circuitEvaluatesToDifferent solver gene geneNames aVars rVars state =
    let evaluationEncoding, circuitVal = evaluateUpdateFunction solver geneNames aVars rVars state
    (evaluationEncoding, solver.Eq(circuitVal, not <| Map.find gene state.Values))

let solutionToCircuit geneNames activatorAssignment repressorAssignment =
    let toCircuit assignment =
        let rec toCircuit i =
            match Seq.item i assignment with
            | AND -> Circuit.And (toCircuit <| leftChild i, toCircuit <| rightChild i)
            | OR -> Circuit.Or (toCircuit <| leftChild i, toCircuit <| rightChild i)
            | n when n = (NOTHING <| Seq.length geneNames) -> failwith "solutionToCircuit pattern match error"
            | var -> Circuit.Node (indexToName var geneNames)
        toCircuit 0

    if Seq.head repressorAssignment = (NOTHING <| Seq.length geneNames) then
        toCircuit activatorAssignment
    else
        toCircuit activatorAssignment |> Circuit.inhibition (toCircuit repressorAssignment)

let circuitToEncoding (solver : SATSolver) circuit geneNames (a : BitVec []) (r : BitVec []) =
    let fixMaxActivators = fixMaxActivators (Seq.length geneNames) solver
    let fixMaxRepressors = fixMaxRepressors (Seq.length geneNames) solver

    let circuitToEncoding (a : BitVec []) circuit fix =
        match circuit with
        | Circuit.Node n -> solver.And [| solver.Eq(a.[0], nameToIndex n geneNames); fix 1 |]
        | Circuit.Or (Circuit.Node n1, Circuit.Node n2) ->
            let n1 = nameToIndex n1 geneNames
            let n2 = nameToIndex n2 geneNames
            solver.And [| solver.Eq(a.[0], OR); solver.Eq(a.[1], n1); solver.Eq(a.[2], n2); fix 2 |]
        | Circuit.And (Circuit.Node n1, Circuit.Node n2) ->
            let n1 = nameToIndex n1 geneNames
            let n2 = nameToIndex n2 geneNames
            solver.And [| solver.Eq(a.[0], AND); solver.Eq(a.[1], n1); solver.Eq(a.[2], n2); fix 2 |]
        | Circuit.Or (Circuit.Or(Circuit.Node n2, Circuit.Node n3), Circuit.Node n1) ->
            let n1 = nameToIndex n1 geneNames
            let n2 = nameToIndex n2 geneNames
            let n3 = nameToIndex n3 geneNames
            solver.And [| solver.Eq(a.[0], OR); solver.Eq(a.[1], OR); solver.Eq(a.[2], n1); solver.Eq(a.[3], n2); solver.Eq(a.[4], n3); fix 3 |]
        | Circuit.And (Circuit.And(Circuit.Node n2, Circuit.Node n3), Circuit.Node n1) ->
            let n1 = nameToIndex n1 geneNames
            let n2 = nameToIndex n2 geneNames
            let n3 = nameToIndex n3 geneNames
            solver.And [| solver.Eq(a.[0], AND); solver.Eq(a.[1], AND); solver.Eq(a.[2], n1); solver.Eq(a.[3], n2); solver.Eq(a.[4], n3); fix 3 |]
        | Circuit.Or (Circuit.And(Circuit.Node n2, Circuit.Node n3), Circuit.Node n1) ->
            let n1 = nameToIndex n1 geneNames
            let n2 = nameToIndex n2 geneNames
            let n3 = nameToIndex n3 geneNames
            solver.And [| solver.Eq(a.[0], OR); solver.Eq(a.[1], AND); solver.Eq(a.[2], n1); solver.Eq(a.[3], n2); solver.Eq(a.[4], n3); fix 3 |]
        | Circuit.And (Circuit.Or(Circuit.Node n2, Circuit.Node n3), Circuit.Node n1) ->
            let n1 = nameToIndex n1 geneNames
            let n2 = nameToIndex n2 geneNames
            let n3 = nameToIndex n3 geneNames
            solver.And [| solver.Eq(a.[0], AND); solver.Eq(a.[1], OR); solver.Eq(a.[2], n1); solver.Eq(a.[3], n2); solver.Eq(a.[4], n3); fix 3 |]
        // TODO: remove duplicate syntactically different circuits
        | Circuit.Or (Circuit.Node n1, Circuit.And(Circuit.Node n2, Circuit.Node n3)) ->
            let n1 = nameToIndex n1 geneNames
            let n2 = nameToIndex n2 geneNames
            let n3 = nameToIndex n3 geneNames
            solver.And [| solver.Eq(a.[0], OR); solver.Eq(a.[1], AND); solver.Eq(a.[2], n1); solver.Eq(a.[3], n2); solver.Eq(a.[4], n3); fix 3 |]
        | Circuit.And (Circuit.Node n1, Circuit.Or(Circuit.Node n2, Circuit.Node n3)) ->
            let n1 = nameToIndex n1 geneNames
            let n2 = nameToIndex n2 geneNames
            let n3 = nameToIndex n3 geneNames
            solver.And [| solver.Eq(a.[0], AND); solver.Eq(a.[1], OR); solver.Eq(a.[2], n1); solver.Eq(a.[3], n2); solver.Eq(a.[4], n3); fix 3 |]

    match circuit with
    | Circuit.And (act, Circuit.Not rep) -> solver.And [| circuitToEncoding a act fixMaxActivators; circuitToEncoding r rep fixMaxRepressors |]
    | circuit -> solver.And [| circuitToEncoding a circuit fixMaxActivators; fixMaxRepressors 0 |]

let private bitvecToIndices i numGenes =
    let mutable i = i
    let mutable r = 0
    let mutable indices = []
    while i <> 0UL do
        if (i &&& 1UL) <> 0UL then
            indices <- r :: indices
        i <- i >>> 1
        r <- r + 1
    indices |> List.map (fun i -> numGenes - i - 1)

let bitvecToCircuitOr (geneNames : string []) a =
    let genes = [ for i in bitvecToIndices a (Array.length geneNames) do yield Circuit.Node geneNames.[i] ]
    Circuit.orList genes

let bitvecToCircuitAnd (geneNames : string []) a =
    let genes = [ for i in bitvecToIndices a (Array.length geneNames) do yield Circuit.Node geneNames.[i] ]
    Circuit.andList genes

let bitvecToCircuitAndOr (geneNames : string []) (a1, a2) =
    Circuit.Or (bitvecToCircuitAnd geneNames a1, bitvecToCircuitAnd geneNames a2)
    
let bitvecToCircuitOrAnd (geneNames : string []) (a1, a2) =
    Circuit.And (bitvecToCircuitOr geneNames a1, bitvecToCircuitOr geneNames a2)

let bitvecToCircuitOrNotOr (geneNames : string []) (a, r) =
    bitvecToCircuitOr geneNames a |> Circuit.inhibition (bitvecToCircuitOr geneNames r)

let bitvecToCircuitOrNotAnd (geneNames : string []) (a, r) =
    bitvecToCircuitOr geneNames a |> Circuit.inhibition (bitvecToCircuitAnd geneNames r)

let bitvecToCircuitAndNotOr (geneNames : string []) (a, r) =
    bitvecToCircuitAnd geneNames a |> Circuit.inhibition (bitvecToCircuitOr geneNames r)

let bitvecToCircuitAndNotAnd (geneNames : string []) (a, r) =
    bitvecToCircuitAnd geneNames a |> Circuit.inhibition (bitvecToCircuitAnd geneNames r)

let bitvecToCircuitAndOrNotOr (geneNames : string []) ((a1, a2), r) =
    bitvecToCircuitAndOr geneNames (a1, a2) |> Circuit.inhibition (bitvecToCircuitOr geneNames r)

let bitvecToCircuitAndOrNotAnd (geneNames : string []) ((a1, a2), r) =
    bitvecToCircuitAndOr geneNames (a1, a2) |> Circuit.inhibition (bitvecToCircuitAnd geneNames r)

let bitvecToCircuitOrAndNotOr (geneNames : string []) ((a1, a2), r) =
    bitvecToCircuitOrAnd geneNames (a1, a2) |> Circuit.inhibition (bitvecToCircuitOr geneNames r)

let bitvecToCircuitOrAndNotAnd (geneNames : string []) ((a1, a2), r) =
    bitvecToCircuitOrAnd geneNames (a1, a2) |> Circuit.inhibition (bitvecToCircuitAnd geneNames r)

let bitvecToCircuitOrNotAndOr (geneNames : string []) (a, (r1, r2)) =
    bitvecToCircuitOr geneNames a |> Circuit.inhibition (bitvecToCircuitAndOr geneNames (r1, r2))

let bitvecToCircuitOrNotOrAnd (geneNames : string []) (a, (r1, r2)) =
    bitvecToCircuitOr geneNames a |> Circuit.inhibition (bitvecToCircuitOrAnd geneNames (r1, r2))

let bitvecToCircuitAndNotAndOr (geneNames : string []) (a, (r1, r2)) =
    bitvecToCircuitAnd geneNames a |> Circuit.inhibition (bitvecToCircuitAndOr geneNames (r1, r2))

let bitvecToCircuitAndNotOrAnd (geneNames : string []) (a, (r1, r2)) =
    bitvecToCircuitAnd geneNames a |> Circuit.inhibition (bitvecToCircuitOrAnd geneNames (r1, r2))

let bitvecToCircuitAndOrNotAndOr (geneNames : string []) (a1, a2, r1, r2) =
    bitvecToCircuitAndOr geneNames (a1, a2) |> Circuit.inhibition (bitvecToCircuitAndOr geneNames (r1, r2))

let bitvecToCircuitAndOrNotOrAnd (geneNames : string []) (a1, a2, r1, r2) =
    bitvecToCircuitAndOr geneNames (a1, a2) |> Circuit.inhibition (bitvecToCircuitOrAnd geneNames (r1, r2))

let bitvecToCircuitOrAndNotOrAnd (geneNames : string []) (a1, a2, r1, r2) =
    bitvecToCircuitOrAnd geneNames (a1, a2) |> Circuit.inhibition (bitvecToCircuitOrAnd geneNames (r1, r2))

let bitvecToCircuitOrAndNotAndOr (geneNames : string []) (a1, a2, r1, r2) =
    bitvecToCircuitOrAnd geneNames (a1, a2) |> Circuit.inhibition (bitvecToCircuitAndOr geneNames (r1, r2))
