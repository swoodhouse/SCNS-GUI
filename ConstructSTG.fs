module ConstructSTG

open Data
open System.Collections
open System.Collections.Generic
open FSharp.Data
open FSharpx.Collections

let private loadCsv (filename : string) =
    let csv = CsvFile.Load(filename)
    if csv.NumberOfColumns - 1 > 64 then
        failwith "SCNS does not support datasets with more than 64 genes"

    let parseRow (r : CsvRow) =
        let name, class' = r.GetColumn 0, r.GetColumn 1
        ((name, class'), [| for i in 2 .. csv.NumberOfColumns - 1 do
                                yield System.Boolean.Parse (r.GetColumn i) |])
    
    let header = (Option.get csv.Headers).[2..]
    let rows = Seq.map parseRow csv.Rows
    (header, rows)

let private boolArrayToUint32 (a : bool []) =
    let a = BitArray (Array.rev a)
    let u = [| 0 |]
    a.CopyTo(u, 0)
    uint32 (u.[0])

let private boolArrayToUint64 a =
    if Array.length a <= 32 then
        uint64 (boolArrayToUint32 a)
    else
        let high = boolArrayToUint32 a.[.. 31]
        let low = boolArrayToUint32 a.[32 ..]
        (uint64 high <<< (Array.length a.[32 ..])) ||| uint64 low

let private toUniqueBitvectors rows =
    let seen = HashSet<uint64>()
    Map.ofList [ for (id, array) in rows do
                     let bitVec = boolArrayToUint64 array
                     if not <| seen.Contains bitVec then
                         yield (bitVec, id) ]

let inline private allOnes numGenes = System.UInt64.MaxValue >>> (64 - numGenes)

let private possibleOneNeighbours numGenes state =
    let flipRightmost0 i = i ||| i + 1UL
    let flip = ref state
    seq { while !flip <> allOnes numGenes do
              let newFlip = flipRightmost0 !flip
              yield (newFlip ^^^ !flip) ||| state
              flip := newFlip }

let private geneChange (genes : string []) (state : uint64) state' =
    let index = Array.length genes - (int <| System.Math.Log(float <| state ^^^ state', 2.0)) - 1
    genes.[index]

let private constructSTG genes map =
    seq { for state in Map.keys map do
              for state' in possibleOneNeighbours (Array.length genes) state do
                  if Map.containsKey state' map then
                      yield (geneChange genes state state', (Map.find state map, Map.find state' map)) }

let loadSTG filename =
  let genes, rows = loadCsv filename
  let map = toUniqueBitvectors rows

  let uniqueStates =
    seq { for ((name, class'), values) in rows do
           match Map.tryFindKey (fun _ v -> v = (name, class')) map with
           | None -> ()
           | Some k -> yield ((name, class'), { Id = k
                                                Name = name
                                                Class = class'
                                                Values = Array.zip genes values |> Map.ofArray }) } |> Map.ofSeq

  let edges = constructSTG genes map

  let statesWithGeneTransitions =
      seq { for g in genes do
              yield g, set [ for (g', (a, b)) in edges do
                               if g = g' then yield (Map.find a uniqueStates, Map.find b uniqueStates) ] } |> Map.ofSeq

  let statesWithoutGeneTransitions =
      statesWithGeneTransitions |> Map.map (fun _ e -> Set.difference (Set.ofSeq <| Map.values uniqueStates)
                                                                      (Set.map (fun (a, b) -> set [a; b]) e |> Set.unionMany))

  let edges = edges |> Seq.map (fun (_, ((x, _), (y, _))) -> sprintf "{ \"id\": \"%sto%s\", \"source\": \"%s\", \"target\": \"%s\" }" x y x y)
                    |> Seq.reduce (fun x y -> x + ", " + y)

  let classes =  Map.keys uniqueStates |> Set.ofSeq |> Set.map (fun (_, class') -> class')

  let nodes = uniqueStates |> Map.toSeq
                           |> Seq.map (fun ((name, class'), _) -> sprintf "{ \"id\": \"%s\", \"sort\": \"Class%i\"}" name (Seq.findIndex ((=) class') classes))
                           |> Seq.reduce (fun x y -> x + ", " + y)

  let json = sprintf "{ \"dataSchema\": { \"nodes\": [ { \"name\": \"sort\", \"type\": \"string\" } ] }, \"data\": { \"nodes\": [ %s ], \"edges\": [ %s ] } }" nodes edges

  { states = Map.values uniqueStates |> Set.ofSeq
    statesWithGeneTransitions = statesWithGeneTransitions
    statesWithoutGeneTransitions = statesWithoutGeneTransitions
    cellClasses = classes
    json = json }
