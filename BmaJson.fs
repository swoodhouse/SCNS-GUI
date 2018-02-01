module BmaJson

open Circuit
open FSharpx.Collections

let rec private circuitToBmaTargetFunction c =
    match c with
    | Value true -> "1"
    | Value false -> "0"
    | And (c1, c2) -> sprintf "min(%s, %s)" (circuitToBmaTargetFunction c1) (circuitToBmaTargetFunction c2)
    | Or (c1, c2) -> sprintf "max(%s, %s)" (circuitToBmaTargetFunction c1) (circuitToBmaTargetFunction c2)
    | Not c -> sprintf "1 - %s" (circuitToBmaTargetFunction c)
    | Node name -> sprintf "var(%s)" name

let edges (var, circuit) =
    seq { for var' in variables circuit do
              yield (var', var) }

let modelToBmaJson (model : Map<Gene, Circuit>) =
    if Map.isEmpty model then ""
    else
      let keys = Map.keys model
      let model = Map.toSeq model
      let concat = Seq.reduce (fun s s' -> s + "," + s')
      let vars = model
                 |> Seq.mapi (fun i (n, c) -> sprintf "{\"Name\":\"%s\",\"Id\":%i,\"RangeFrom\":0,\"RangeTo\":1,\"Formula\":\"%s\"}" n i (circuitToBmaTargetFunction c))
                 |> concat

      let printEdge i (x, y) = sprintf "{\"Id\":%i,\"FromVariable\":%i,\"ToVariable\":%i,\"Type\":\"Activator\"}" i x y
      let relationships = model
                          |> Seq.map edges
                          |> Seq.concat
                          |> Seq.map (fun (x, y) -> (Seq.findIndex ((=) x) keys, Seq.findIndex ((=) y) keys))
                          |> Seq.mapi printEdge
                          |> concat
      let vars' = model
                  |> Seq.mapi (fun i (n, _) -> sprintf "{\"Id\":%i,\"Name\":\"%s\",\"Type\":\"Default\",\"ContainerId\":1,
                                                         \"PositionX\":500.0,\"PositionY\":-154.0,\"CellX\":0,\"CellY\":0,\"Angle\":0}" i n)
                  |> concat

      sprintf "{\"Model\": {
                 \"Name\":\"Model\",
                 \"Variables\": [%s],
                 \"Relationships\": [%s]},
               \"Layout\": {
                 \"Variables\": [%s],
                 \"Containers\": [{\"Id\":1,\"Name\":\"Cell\",\"Size\":1,\"PositionX\":2,\"PositionY\":-1}]},
               \"ltl\": {\"states\":[],\"operations\":[]}}" vars relationships vars'
