module Data

open Circuit
open System

[<CustomEquality; CustomComparison>]
type State =
  { Id : uint64
    Name : string
    Class : string
    Values : Map<Gene, bool> }

    override x.Equals(yobj) =
        match yobj with
        | :? State as y -> x.Id = y.Id
        | _ -> invalidArg "yobj" "cannot test for equality of a State with a non-State type"

    override x.GetHashCode() = hash x.Name
    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? State as y -> compare x.Id y.Id
          | _ -> invalidArg "yobj" "cannot compare a State with a non-State type"
          
type Stg =
  { states : Set<State>
    statesWithGeneTransitions : Map<Gene, Set<State * State>>
    statesWithoutGeneTransitions : Map<Gene, Set<State>>
    cellClasses : Set<string>
    json : string }

type ConcurrentRef<'a> (c) = class
    let mutable contents : 'a = c

    member x.Get =
        lock x (fun () -> contents)
    member x.Set c =
        lock x (fun () -> contents <- c)
end
