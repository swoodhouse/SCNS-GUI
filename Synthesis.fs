// This file needs to be refactored
module Synthesis

open System.Collections.Generic
open FSharpx.Collections
open Microsoft.Z3
open Data
open ShortestPaths
open SAT
open FunctionEncoding

let private constraintsBitVec (solver : SATSolver) ctor (m : Model) (d : FuncDecl) =
    let x = System.Int32.Parse(m.[d].ToString())
    solver.NotEq (ctor (d.Name.ToString()), x)

let private constraintsCircuitVar numGenes (solver : SATSolver) (m : Model) (ds : FuncDecl []) =
    solver.Or <| Array.map (constraintsBitVec solver (makeCircuitVar numGenes solver) m) ds

let private buildGraph edges =
    let build graph (u, v) =
        let neighbours = if Map.containsKey u graph then v :: Map.find u graph else [v]
        Map.add u neighbours graph
    Set.fold build Map.empty edges

let inline private explictEvalToDifferentOr gene circuit state =
    let activator = state &&& circuit
    let current = state &&& gene
    if current = 0UL then activator <> 0UL else activator = 0UL

let inline private explictEvalToDifferentAnd gene circuit state =
    let activator = state &&& circuit
    let current = state &&& gene
    if current = 0UL then activator = circuit else activator <> circuit

let inline private explictEvalToDifferentAndOr gene circuit1 circuit2 state =
    explictEvalToDifferentAnd gene circuit1 state || explictEvalToDifferentAnd gene circuit2 state

let inline private explictEvalToDifferentOrAnd gene circuit1 circuit2 state =
    explictEvalToDifferentOr gene circuit1 state && explictEvalToDifferentOr gene circuit2 state

let inline explictNot fA fR gene state =
    let current = state &&& gene
    if current <> 0UL && not fR then true
    elif current = 0UL && fR then false
    else fA

let inline private explictEvalToDifferentOrNotOr gene activator repressor state =
    explictNot (explictEvalToDifferentOr gene activator state) (explictEvalToDifferentOr gene repressor state) gene state

let inline private explictEvalToDifferentOrNotAnd gene activator repressor state =
    explictNot (explictEvalToDifferentOr gene activator state) (explictEvalToDifferentAnd gene repressor state) gene state

let inline private explictEvalToDifferentAndNotOr gene activator repressor state =
    explictNot (explictEvalToDifferentAnd gene activator state) (explictEvalToDifferentOr gene repressor state) gene state

let inline private explictEvalToDifferentAndNotAnd gene activator repressor state =
    explictNot (explictEvalToDifferentAnd gene activator state) (explictEvalToDifferentAnd gene repressor state) gene state

let inline private explictEvalToDifferentAndOrNotOr gene activator1 activator2 repressor state =
    explictNot (explictEvalToDifferentAndOr gene activator1 activator2 state) (explictEvalToDifferentOr gene repressor state) gene state

let inline private explictEvalToDifferentAndOrNotAnd gene activator1 activator2 repressor state =
    explictNot (explictEvalToDifferentAndOr gene activator1 activator2 state) (explictEvalToDifferentAnd gene repressor state) gene state

let inline private explictEvalToDifferentAndOrNotAndOr gene activator1 activator2 repressor1 repressor2 state  =
    explictNot (explictEvalToDifferentAndOr gene activator1 activator2 state) (explictEvalToDifferentAndOr gene repressor1 repressor2 state) gene state

let inline private explictEvalToDifferentAndOrNotOrAnd gene activator1 activator2 repressor1 repressor2 state =
    explictNot (explictEvalToDifferentAndOr gene activator1 activator2 state) (explictEvalToDifferentOrAnd gene repressor1 repressor2 state) gene state

let inline private explictEvalToDifferentOrAndNotOr gene activator1 activator2 repressor state =
    explictNot (explictEvalToDifferentOrAnd gene activator1 activator2 state) (explictEvalToDifferentOr gene repressor state) gene state

let inline private explictEvalToDifferentOrAndNotAnd gene activator1 activator2 repressor state =
    explictNot (explictEvalToDifferentOrAnd gene activator1 activator2 state) (explictEvalToDifferentAnd gene repressor state) gene state

let inline private explictEvalToDifferentOrAndNotAndOr gene activator1 activator2 repressor1 repressor2 state =
    explictNot (explictEvalToDifferentOrAnd gene activator1 activator2 state) (explictEvalToDifferentAndOr gene repressor1 repressor2 state) gene state

let inline private explictEvalToDifferentOrAndNotOrAnd gene activator1 activator2 repressor1 repressor2 state =
    explictNot (explictEvalToDifferentOrAnd gene activator1 activator2 state) (explictEvalToDifferentOrAnd gene repressor1 repressor2 state) gene state

let inline private explictEvalToDifferentOrNotAndOr gene activator repressor1 repressor2 state =
    explictNot (explictEvalToDifferentOr gene activator state) (explictEvalToDifferentAndOr gene repressor1 repressor2 state) gene state

let inline private explictEvalToDifferentOrNotOrAnd gene activator repressor1 repressor2 state  =
    explictNot (explictEvalToDifferentOr gene activator state) (explictEvalToDifferentOrAnd gene repressor1 repressor2 state) gene state

let inline private explictEvalToDifferentAndNotAndOr gene activator repressor1 repressor2 state  =
    explictNot (explictEvalToDifferentAnd gene activator state) (explictEvalToDifferentAndOr gene repressor1 repressor2 state) gene state

let inline private explictEvalToDifferentAndNotOrAnd gene activator repressor1 repressor2 state  =
    explictNot (explictEvalToDifferentAnd gene activator state) (explictEvalToDifferentOrAnd gene repressor1 repressor2 state) gene state

let private log2 i =
    let mutable i = i
    let mutable r = 0
    while (i <- i >>> 1; i <> 0UL) do
      r <- r + 1
    r

let private generateBitvecs numInputs numGenes =
    let rec generate v =
        let v = int64 v
        let t = (v ||| (v - 1L)) + 1L
        t ||| ((((t &&& (t * -1L)) / (v &&& -v)) >>> 1) - 1L) |> uint64

    match numInputs with
    | 1 -> Seq.unfold (fun i -> Some (i, i <<< 1)) 1UL |> Seq.truncate numGenes
    | 2 -> Seq.unfold (fun x -> Some (x, generate x)) 3UL |> Seq.takeWhile (fun x -> log2 x < numGenes)
    | 3 -> Seq.unfold (fun x -> Some (x, generate x)) 7UL |> Seq.takeWhile (fun x -> log2 x < numGenes)
    | _ -> failwith "unimplemented"

let private eval f gene threshold statesWithGeneTransitions statesWithoutGeneTransitions circuit =
    let max = Set.count statesWithoutGeneTransitions
    let threshold = max * threshold / 100
    
    let t = Seq.sumBy (fun s -> if f gene circuit s then 0 else 1) statesWithoutGeneTransitions
    if t < threshold then (t, Set.empty)
    else
        (t, set [ for s in statesWithGeneTransitions do
                      if f gene circuit s then yield s])

let eval2 f gene threshold statesWithGeneTransitions statesWithoutGeneTransitions circuit1 circuit2 =
    let max = Set.count statesWithoutGeneTransitions
    let threshold = max * threshold / 100

    let t = Seq.sumBy (fun s -> if f gene circuit1 circuit2 s then 0 else 1) statesWithoutGeneTransitions
    if t < threshold then (t, Set.empty)
    else
        (t, set [ for s in statesWithGeneTransitions do
                      if f gene circuit1 circuit2 s then yield s])

let private eval3 f gene threshold statesWithGeneTransitions statesWithoutGeneTransitions circuit1 circuit2 circuit3 =
    let max = Set.count statesWithoutGeneTransitions
    let threshold = max * threshold / 100

    let t = Seq.sumBy (fun s -> if f gene circuit1 circuit2 circuit3 s then 0 else 1) statesWithoutGeneTransitions
    if t < threshold then (t, Set.empty)
    else
        (t, set [ for s in statesWithGeneTransitions do
                      if f gene circuit1 circuit2 circuit3 s then yield s])
                        
let private eval4 f gene threshold statesWithGeneTransitions statesWithoutGeneTransitions circuit1 circuit2 circuit3 circuit4 =
    let max = Set.count statesWithoutGeneTransitions
    let threshold = max * threshold / 100

    let t = Seq.sumBy (fun s -> if f gene circuit1 circuit2 circuit3 circuit4 s then 0 else 1) statesWithoutGeneTransitions
    if t < threshold then (t, Set.empty)
    else
        (t, set [ for s in statesWithGeneTransitions do
                      if f gene circuit1 circuit2 circuit3 circuit4 s then yield s])

let private dictToSet dictionary =
    (dictionary :> seq<_>)
    |> Seq.map (|KeyValue|)
    |> Set.ofSeq

let findAllowedEdges gene geneNames maxActivators maxRepressors threshold statesWithGeneTransitions statesWithoutGeneTransitions =
    if maxActivators > 2 && maxRepressors > 2 then failwith "maxActivators > 2 && maxRepressors > 2 not implemented yet"
    let gene = Seq.findIndex ((=) gene) (Seq.rev geneNames)
    let gene = 1UL <<< gene
    let numGenes = Array.length geneNames
    let statesWithoutGeneTransitions = Set.map (fun s -> s.Id) statesWithoutGeneTransitions
    let mutable statesWithGeneTransitions' = set [ for (a, b) in statesWithGeneTransitions do yield a.Id; yield b.Id ]

    let act1 = generateBitvecs 1 numGenes |> Seq.cache
    let act2 = if maxActivators >= 2 then generateBitvecs 2 numGenes |> Seq.cache else Seq.empty
    let act23 = if maxActivators >= 3 then Seq.append act2 (generateBitvecs 3 numGenes |> Seq.cache) else act2
    let f b = ((~~~gene) &&& b) <> 0UL
    let rep1 = if maxRepressors >= 1 then act1 |> Seq.filter f |> Seq.cache else Seq.empty
    let rep2 = if maxRepressors >= 2 then generateBitvecs 2 numGenes |> Seq.filter f |> Seq.cache else Seq.empty
    let rep23 = if maxRepressors >= 3 then Seq.append rep2 (generateBitvecs 3 numGenes) |> Seq.filter f |> Seq.cache else rep2

    let mapCircuitToScoreOr = Dictionary<uint64, int>()
    let mapCircuitToScoreAnd = Dictionary<uint64, int>()
    let mapCircuitToScoreOrAnd = Dictionary<uint64 * uint64, int>()
    let mapCircuitToScoreAndOr = Dictionary<uint64 * uint64, int>()
    let mapCircuitToScoreOrNotOr = Dictionary<uint64 * uint64, int>()
    let mapCircuitToScoreOrNotAnd = Dictionary<uint64 * uint64, int>()
    let mapCircuitToScoreAndNotOr = Dictionary<uint64 * uint64, int>()
    let mapCircuitToScoreAndNotAnd = Dictionary<uint64 * uint64, int>()
    let mapCircuitToScoreOrAndNotOr = Dictionary<(uint64 * uint64) * uint64, int>()
    let mapCircuitToScoreOrAndNotAnd = Dictionary<(uint64 * uint64) * uint64, int>()
    let mapCircuitToScoreAndOrNotOr = Dictionary<(uint64 * uint64) * uint64, int>()
    let mapCircuitToScoreAndOrNotAnd = Dictionary<(uint64 * uint64) * uint64, int>()
    let mapCircuitToScoreOrNotOrAnd = Dictionary<uint64 * (uint64 * uint64), int>()
    let mapCircuitToScoreOrNotAndOr = Dictionary<uint64 * (uint64 * uint64), int>()
    let mapCircuitToScoreAndNotOrAnd = Dictionary<uint64 * (uint64 * uint64), int>()
    let mapCircuitToScoreAndNotAndOr = Dictionary<uint64 * (uint64 * uint64), int>()

    for c in act1 do
        let score, trueEdges = eval explictEvalToDifferentOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions c
        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
        mapCircuitToScoreOr.Add(c, score)
    for c in act23 do
        let score, trueEdges = eval explictEvalToDifferentOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions c
        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
        mapCircuitToScoreOr.Add(c, score)
        let score, trueEdges = eval explictEvalToDifferentAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions c
        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
        mapCircuitToScoreAnd.Add(c, score)

    if maxActivators >= 3 then
        for c1 in act2 do
            for c2 in act1 do
                if c1 &&& c2 = 0UL then
                    let score, trueEdges = eval2 explictEvalToDifferentOrAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions c1 c2
                    statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                    mapCircuitToScoreOrAnd.Add((c1, c2), score)
                    let score, trueEdges = eval2 explictEvalToDifferentAndOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions c1 c2
                    statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                    mapCircuitToScoreAndOr.Add((c1, c2), score)

    for a in act1 do
        for r in rep1 do
            if a <> r then
                let score, trueEdges = eval2 explictEvalToDifferentOrNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r
                statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                mapCircuitToScoreOrNotOr.Add((a, r), score)
    for a in act23 do
        for r in rep1 do
            let score, trueEdges = eval2 explictEvalToDifferentOrNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r
            statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
            mapCircuitToScoreOrNotOr.Add((a, r), score)
            let score, trueEdges = eval2 explictEvalToDifferentAndNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r
            statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
            mapCircuitToScoreAndNotOr.Add((a, r), score)
    for a in act1 do
        for r in rep23 do
            let score, trueEdges = eval2 explictEvalToDifferentOrNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r
            statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
            mapCircuitToScoreOrNotOr.Add((a, r), score)
            let score, trueEdges = eval2 explictEvalToDifferentOrNotAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r
            statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
            mapCircuitToScoreOrNotAnd.Add((a, r), score)
    for a in act23 do
        for r in rep23 do
            if a <> r then
                let score, trueEdges = eval2 explictEvalToDifferentOrNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r
                statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                mapCircuitToScoreOrNotOr.Add((a, r), score)
                let score, trueEdges = eval2 explictEvalToDifferentOrNotAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r
                statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                mapCircuitToScoreOrNotAnd.Add((a, r), score)
                let score, trueEdges = eval2 explictEvalToDifferentAndNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r
                statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                mapCircuitToScoreAndNotOr.Add((a, r), score)
                let score, trueEdges = eval2 explictEvalToDifferentAndNotAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r
                statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                mapCircuitToScoreAndNotAnd.Add((a, r), score)

    if maxActivators >= 3 then
        for a1 in act2 do
            for a2 in act1 do
                if a1 &&& a2 = 0UL then
                    for r in rep1 do
                        let score, trueEdges = eval3 explictEvalToDifferentOrAndNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a1 a2 r
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreOrAndNotOr.Add(((a1, a2), r), score)
                        let score, trueEdges = eval3 explictEvalToDifferentAndOrNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a1 a2 r
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreAndOrNotOr.Add(((a1, a2), r), score)
                    for r in rep23 do
                        let score, trueEdges = eval3 explictEvalToDifferentOrAndNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a1 a2 r
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreOrAndNotOr.Add(((a1, a2), r), score)
                        let score, trueEdges = eval3 explictEvalToDifferentAndOrNotOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a1 a2 r
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreAndOrNotOr.Add(((a1, a2), r), score)
                        let score, trueEdges = eval3 explictEvalToDifferentOrAndNotAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a1 a2 r
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreOrAndNotAnd.Add(((a1, a2), r), score)
                        let score, trueEdges = eval3 explictEvalToDifferentAndOrNotAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a1 a2 r
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreAndOrNotAnd.Add(((a1, a2), r), score)
    if maxRepressors >= 3 then
        for a in act1 do
            for r1 in rep2 do
                for r2 in rep1 do
                    if r1 &&& r2 = 0UL then
                        let score, trueEdges = eval3 explictEvalToDifferentOrNotAndOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r1 r2
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreOrNotAndOr.Add((a, (r1, r2)), score)
                        let score, trueEdges = eval3 explictEvalToDifferentOrNotOrAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r1 r2
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreOrNotOrAnd.Add((a, (r1, r2)), score)
        for a in act23 do
            for r1 in rep2 do
                for r2 in rep1 do
                    if r1 &&& r2 = 0UL then
                        let score, trueEdges = eval3 explictEvalToDifferentOrNotAndOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r1 r2
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreOrNotAndOr.Add((a, (r1, r2)), score)
                        let score, trueEdges = eval3 explictEvalToDifferentOrNotOrAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r1 r2
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreOrNotOrAnd.Add((a, (r1, r2)), score)
                        let score, trueEdges = eval3 explictEvalToDifferentAndNotAndOr gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r1 r2
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreAndNotAndOr.Add((a, (r1, r2)), score)
                        let score, trueEdges = eval3 explictEvalToDifferentAndNotOrAnd gene threshold statesWithGeneTransitions' statesWithoutGeneTransitions a r1 r2
                        statesWithGeneTransitions' <- statesWithGeneTransitions' - trueEdges
                        mapCircuitToScoreAndNotOrAnd.Add((a, (r1, r2)), score)

    let max = Set.count statesWithoutGeneTransitions
    let threshold = max * threshold / 100

    let circuitsOr = dictToSet mapCircuitToScoreOr |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitOr geneNames c, s * 100 / max))
    let circuitsAnd = dictToSet mapCircuitToScoreAnd |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitAnd geneNames c, s * 100 / max))
    let circuitsAndOr = dictToSet mapCircuitToScoreAndOr |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitAndOr geneNames c, s * 100 / max))
    let circuitsOrAnd = dictToSet mapCircuitToScoreOrAnd |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitOrAnd geneNames c, s * 100 / max))
    let circuitsOrNotOr = dictToSet mapCircuitToScoreOrNotOr |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitOrNotOr geneNames c, s * 100 / max))
    let circuitsOrNotAnd = dictToSet mapCircuitToScoreOrNotAnd |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitOrNotAnd geneNames c, s * 100 / max))
    let circuitsAndNotOr = dictToSet mapCircuitToScoreAndNotOr |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitAndNotOr geneNames c, s * 100 / max))
    let circuitsAndNotAnd = dictToSet mapCircuitToScoreAndNotAnd |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitAndNotAnd geneNames c, s * 100 / max))
    let circuitsAndOrNotOr = dictToSet mapCircuitToScoreAndOrNotOr |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitAndOrNotOr geneNames c, s * 100 / max))
    let circuitsAndOrNotAnd = dictToSet mapCircuitToScoreAndOrNotAnd |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitAndOrNotAnd geneNames c, s * 100 / max))
    let circuitsOrAndNotOr = dictToSet mapCircuitToScoreOrAndNotOr |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitOrAndNotOr geneNames c, s * 100 / max))
    let circuitsOrAndNotAnd = dictToSet mapCircuitToScoreOrAndNotAnd |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitOrAndNotAnd geneNames c, s * 100 / max))
    let circuitsOrNotAndOr = dictToSet mapCircuitToScoreOrNotAndOr |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitOrNotAndOr geneNames c, s * 100 / max))
    let circuitsOrNotOrAnd = dictToSet mapCircuitToScoreOrNotOrAnd |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitOrNotOrAnd geneNames c, s * 100 / max))
    let circuitsAndNotAndOr = dictToSet mapCircuitToScoreAndNotAndOr |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitAndNotAndOr geneNames c, s * 100 / max))
    let circuitsAndNotOrAnd = dictToSet mapCircuitToScoreAndNotOrAnd |> Set.filter (fun (x, y) -> y >= threshold) |> Set.map (fun (c, s) -> (bitvecToCircuitAndNotOrAnd geneNames c, s * 100 / max))

    let circuits = circuitsOr + circuitsAnd + circuitsAndOr + circuitsOrAnd + circuitsOrNotOr + circuitsOrNotAnd + circuitsAndNotOr + circuitsAndNotAnd +
                   circuitsAndOrNotOr + circuitsAndOrNotAnd + circuitsOrAndNotOr + circuitsOrAndNotAnd + circuitsOrNotAndOr + circuitsOrNotOrAnd + circuitsAndNotAndOr + circuitsAndNotOrAnd
                   
    (set [ for (a, b) in statesWithGeneTransitions do
               if not (Set.contains a.Id statesWithGeneTransitions') then yield (a, b)
               if not (Set.contains b.Id statesWithGeneTransitions') then yield (b, a) ], circuits)

let findPaths allowedEdges initialStates targetStates = 
    let reducedStateGraph = buildGraph allowedEdges
    
    let shortestPaths = initialStates |> Array.map (fun initial ->
        async {
            let targetStates = targetStates |> Set.ofArray |> Set.remove initial
            return shortestPathMultiSink reducedStateGraph initial targetStates })
    
    let shortestPaths = shortestPaths |> Async.Parallel |> Async.RunSynchronously

    [| for i in 0 .. Array.length targetStates - 1 do
           yield [ for j in 0 .. Array.length initialStates - 1 do
                       for path in shortestPaths.[j] do
                           match path with
                           | [] -> ()
                           | l -> if List.item (List.length l - 1) l = targetStates.[i] then yield l ] |]

let findFunctions gene geneNames maxActivators maxRepressors threshold shortestPaths statesWithGeneTransitions candidates =
    let solver = new SATSolver()
    let candidates = Map.ofList (Set.toList candidates)
    let circuitEncoding, aVars, rVars = encodeUpdateFunction (Seq.length geneNames) solver gene geneNames

    let encodeTransition (stateA, stateB) =
        if not (Set.contains (stateA, stateB) statesWithGeneTransitions || Set.contains (stateB, stateA) statesWithGeneTransitions)
        then
            solver.True
        else
            let differentA = (let e, v = circuitEvaluatesToDifferent solver gene geneNames aVars rVars stateA in solver.And(e, v))
            differentA

    let encodePath path =
        let f (formula, u) v = (solver.And [ formula; encodeTransition (u, v) ], v)
        List.fold f (solver.True, List.head path) (List.tail path) |> fst
    
    let pathsEncoding = if Seq.isEmpty shortestPaths then solver.True else
                        solver.And [| for paths in shortestPaths do
                                          if List.isEmpty paths then
                                              yield solver.True
                                          else
                                              yield solver.Or (List.map encodePath paths) |]

    let candidateEncoding = if Map.isEmpty candidates then solver.False else
                            solver.Or [| for c in Map.keys candidates do yield circuitToEncoding solver c geneNames aVars rVars |]

    solver.Add (circuitEncoding,
                pathsEncoding,
                candidateEncoding)
                
    // TODO: is this faster with reset/pop and re-adding candidates - [circuit]?
    seq { while solver.Check() = Status.SATISFIABLE do
              let m = solver.Model
              let activatorDecls = Array.filter (fun (d : FuncDecl) ->
                                                   Set.contains (d.Name.ToString()) activatorVars) m.ConstDecls
                                                   |> Array.sortBy (fun d -> d.Name.ToString().Remove(0,1) |> int)

              let repressorDecls = Array.filter (fun (d : FuncDecl) ->
                                                   Set.contains (d.Name.ToString()) repressorVars) m.ConstDecls
                                                   |> Array.sortBy (fun d -> d.Name.ToString().Remove(0,1) |> int)

              let activatorAssignment = activatorDecls |> Seq.map (fun d -> System.Int32.Parse(m.[d].ToString()))
              let repressorAssignment = repressorDecls |> Seq.map (fun d -> System.Int32.Parse(m.[d].ToString()))
              let circuit = solutionToCircuit geneNames activatorAssignment repressorAssignment

              yield (circuit, Map.find circuit candidates)
              solver.Add(constraintsCircuitVar (Seq.length geneNames) solver m (activatorDecls ++ repressorDecls))
          solver.Dispose() }
