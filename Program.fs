// This file needs to be refactored
open Suave
open Suave.Files
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Writers
open FSharpx.Collections
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading
open MBrace.Azure
open MBrace.Azure.Management
open MBrace.Core
open RProvider
open RProvider.utils
open Circuit
open BmaJson
open Data
open ConstructSTG
open StaticFiles

let mimeTypes =
  defaultMimeTypesMap
    @@ (function | ".swf" -> createMimeType "application/x-shockwave-flash" false | _ -> None)
    
let webConfig = { defaultConfig with
                    mimeTypesMap = mimeTypes }

let readLines filePath = System.IO.File.ReadAllLines(filePath)

let parseJson (req : HttpRequest) =
    let data = req.form |> List.head |> fst
    let data = data.Split(',')
    let left, right = data.[0], data.[1]
    let left = left.Split(':').[1]
    let right = right.Split(':').[1]
    let key = left.Trim('"')
    let value = right.Trim('"', '}') |> System.Int32.Parse
    (key, value)

let parseKoOe (req : HttpRequest) =
    let data = req.form |> List.head |> fst
    let data = data.Split([|"],"|], 2, System.StringSplitOptions.None)
    let left, right = data.[0].Split(':').[1], data.[1].Split(':').[1]
    let KOs = left.Trim('[', ']', '}').Replace("\"", "").Split(',')
    let OEs = right.Trim('[', ']', '}').Replace("\"", "").Split(',')
    (KOs, OEs)

let noCache = 
  setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  >=> setHeader "Pragma" "no-cache"
  >=> setHeader "Expires" "0"

let Sequential (ops:Async<'T> seq) = async {
  let mutable res = []
  for v in ops do
    let! value = v
    res <- value :: res
  return res }
    
let deploy () =
    try
        let file = "azure.publishsettings"
        if not <| System.IO.File.Exists file then
            None
        else
            let subscription = SubscriptionManager.FromPublishSettingsFile(file, Region.North_Europe, logger = new ConsoleLogger())
            try
                let deployment = subscription.GetDeployment("SCNSmbrace")
                Some deployment
            with
            | ex ->
                let deployment = subscription.Provision(10, serviceName = "SCNSmbrace", vmSize = VMSize.A5)
                Some deployment
    with
    | ex -> None

let connectToCluster' (deployment : Deployment) =
    try
        let cluster' = AzureCluster.Connect(deployment, 
                                            logger = ConsoleLogger(true), 
                                            logLevel = LogLevel.Info)
                 
        let contentDir = __SOURCE_DIRECTORY__ + "/lib/"
        cluster'.RegisterNativeDependency (contentDir + "libz3.dll")
        Some cluster'
    with
    | ex -> None
    
let private installRpackages () =
    if not (R.is_element("gplots", R.rownames(R.installed_packages())).GetValue()) then
        System.Console.WriteLine "Installing required R packages"
        R.install_packages("gplots", repos="https://cloud.r-project.org") |> ignore
//        R.install_packages("bitops_1.0-6.zip") |> ignore
//        R.install_packages("KernSmooth_2.23-15.zip") |> ignore
//        R.install_packages("gtools_3.5.0.zip") |> ignore
//        R.install_packages("gdata_2.17.0.zip") |> ignore
//        R.install_packages("caTools_1.17.1.zip") |> ignore
//        R.install_packages("gplots_3.0.1.zip") |> ignore

type ResultsStream = IDictionary<Gene, (Circuit * int) list>

let getModel (resultsStream : ResultsStream) =
    if not (Seq.isEmpty resultsStream.Keys) && resultsStream.Values |> Seq.map (not << List.isEmpty) |> Seq.reduce (&&) then // refactor
        Map.ofSeq [for x in resultsStream do yield (x.Key, combineCircuits x.Key (List.unzip x.Value |> fst))] |> Some
    else
        None

type ServerState () = class
    let stg = ConcurrentRef<Stg option>(None)
    let geneParameters = ConcurrentRef<Map<Gene, int*int*int>>(Map.empty)
   
    let initialClasses = ConcurrentRef<Set<string>>(Set.empty)
    let targetClasses = ConcurrentRef<Set<string>>(Set.empty)
    let (resultsStream : ResultsStream) = ConcurrentDictionary() :> ResultsStream
    let (synthesisTerminated : IDictionary<Gene, bool>) = ConcurrentDictionary() :> IDictionary<Gene, bool>
    let stableStatesTerminated = ConcurrentRef<bool>(false)
    let koGenes = ConcurrentRef<Set<string>>(Set.empty)
    let oeGenes = ConcurrentRef<Set<string>>(Set.empty)

    let synthesisCancellation = ConcurrentRef<CancellationTokenSource>(new CancellationTokenSource())
    let stableStatesCancellation = ConcurrentRef<CancellationTokenSource>(new CancellationTokenSource())

    let deployment = ConcurrentRef<Deployment option>(deploy())

    let connectToCluster () =
        let file = "settings.txt"
        if not <| System.IO.File.Exists file then None
        else
            if System.IO.File.ReadAllText file <> "true" then None
            else
                Option.bind connectToCluster' deployment.Get
                
    let cluster = ConcurrentRef<AzureCluster option>(connectToCluster())
    
    do
        installRpackages ()
        System.Diagnostics.Process.Start "http://localhost:8080" |> ignore
    
    member x.resetVariables =
        initialClasses.Set Set.empty
        targetClasses.Set Set.empty
        resultsStream.Clear()
        synthesisTerminated.Clear()
        stableStatesTerminated.Set false
        koGenes.Set Set.empty
        oeGenes.Set Set.empty
        synthesisCancellation.Get.Cancel()
        synthesisCancellation.Set(new CancellationTokenSource())
        stableStatesCancellation.Get.Cancel()
        stableStatesCancellation.Set(new CancellationTokenSource())

    member x.App =
        choose
          [ GET >=> choose
              (List.append staticFiles 
              [path "/data" >=> request (fun _ ->
                                             match stg.Get with
                                             | None -> RequestErrors.BAD_REQUEST "No data" 
                                             | Some stg' ->
                                                 let cellClasses = stg'.cellClasses
                                                                |> Seq.map (sprintf "\"%s\"")
                                                                |> Seq.reduce (fun x y -> x + ", " + y)
                                                                |> sprintf "[ %s ]"

                                                 let parameters = geneParameters.Get |> Map.toList
                                                               |> List.map (fun (g, (a, r, t)) -> 
                                                                               sprintf "{ \"gene\":\"%s\", \"activators\":%i, \"repressors\":%i, \"threshold\":%i }" g a r t)
                                                               |> List.reduce (fun x y -> x + ", " + y)
                                                               |> sprintf "[ %s ]"

                                                 let initialClasses = if Set.isEmpty <| initialClasses.Get then "[]"
                                                                      else
                                                                        initialClasses.Get
                                                                     |> Seq.map (sprintf "\"%s\"")
                                                                     |> Seq.reduce (fun x y -> x + ", " + y)
                                                                     |> sprintf "[ %s ]"

                                                 let targetClasses = if Set.isEmpty <| targetClasses.Get then "[]"
                                                                     else
                                                                       targetClasses.Get
                                                                    |> Seq.map (sprintf "\"%s\"")
                                                                    |> Seq.reduce (fun x y -> x + ", " + y)
                                                                    |> sprintf "[ %s ]"
                                             
                                                 let json = sprintf "{ \"stg\": %s, \"cellClasses\": %s, \"parameters\": %s, \"initialClasses\": %s, \"targetClasses\": %s }"
                                                                    stg'.json cellClasses parameters initialClasses targetClasses
                                                 OK json >=> Writers.setMimeType "application/json")

               path "/results" >=> request (fun _ ->
                                                 if resultsStream.Keys |> Seq.isEmpty then RequestErrors.BAD_REQUEST "Synthesis not executed"
                                                   else
                                                    let results = Seq.zip resultsStream.Keys resultsStream.Values
                                                               |> Seq.map (fun (g, cs) -> let r = if List.isEmpty cs then
                                                                                                      if synthesisTerminated.[g] then
                                                                                                          "{ \"fun\": \"No solutions found! Recommend loosening parameters: decrease threshold and/or decrease activators/repressors\", \"score\": \"-\" }"
                                                                                                      else
                                                                                                          "{ \"fun\": \"Searching...\", \"score\": \"-\" }"
                                                                                                  else cs |> List.map (fun (c, t) -> sprintf "{ \"fun\": \"%s\", \"score\":\"%s\" }" (printCircuit c) (string t))
                                                                                                          |> (fun l -> if List.length l > 5 then "{ \"fun\": \"More solutions exist. Recommend tightening parameters: increase threshold and/or decrease activators/repressors\", \"score\": \"-\" }" :: List.take 5 l else l)
                                                                                                          |> List.reduce (fun x y -> x + ", " + y)
                                                                                          sprintf "{ \"title\": \"%s\", \"results\": [ %s ] }" g r)
                                                               |> Seq.reduce (fun x y -> x + ", " + y)
                                                               |> sprintf "[ %s ]"

                                                    let terminated = Seq.zip synthesisTerminated.Keys synthesisTerminated.Values
                                                                  |> Seq.map (fun (g, b) -> sprintf "\"%s\":%b" g b)
                                                                  |> Seq.reduce (fun x y -> x + ", " + y)
                                                                  |> sprintf "{ %s }"

                                                    let completeModelFound = if getModel resultsStream |> Option.isSome then "true" else "false"

                                                    let json = sprintf "{ \"results\": %s, \"terminated\": %s, \"completeModelFound\": %s }" results terminated completeModelFound
                                                    OK json >=> Writers.setMimeType "application/json")
               path "/stableStatesTerminated" >=> request (fun _ -> if stableStatesTerminated.Get then OK "true" else OK "false")
               path "/stableStatesHeatmap.png" >=> request (fun _ -> noCache >=> file "stableStatesHeatmap.png")
               path "/graph.json" >=> request (fun _ -> noCache >=> Writers.setMimeType "application/json" >=> OK (Option.get stg.Get).json)

               path "/model.json" >=> request (fun _ -> if resultsStream.Keys |> Seq.isEmpty then RequestErrors.BAD_REQUEST "Synthesis not executed"
                                                        else
                                                            match getModel resultsStream with
                                                            | Some model ->
                                                                let json = modelToBmaJson model
                                                                noCache >=> Writers.setMimeType "application/json" >=> OK json
                                                            | None -> Writers.setMimeType "application/json" >=> OK "")

               path "/genes" >=> request (fun _ -> if geneParameters.Get |> Map.isEmpty then RequestErrors.BAD_REQUEST "No data"
                                                   else
                                                       let f x = if Seq.isEmpty x then "[ ]"
                                                                 else
                                                                     Seq.map (sprintf "\"%s\"") x
                                                                  |> Seq.reduce (fun x y -> x + ", " + y)
                                                                  |> sprintf "[ %s ]"
                                                       let json = sprintf "{ \"genes\": %s, \"kos\": %s, \"oes\": %s }" (geneParameters.Get |> Map.keys |> f) (f koGenes.Get) (f oeGenes.Get)
                                                       OK json >=> Writers.setMimeType "application/json")

               path "/cloudProvisioned" >=> request (fun _ -> if Option.isSome deployment.Get then OK "true" else OK "false") 
               path "/runOnCloud">=> request (fun _ -> if Option.isSome cluster.Get then OK "true" else OK "false") ])

            POST >=> choose
              [ path "/uploadCSV" >=> request (fun req -> let file = List.head req.files
                                                          x.resetVariables
                                                          let stg' = loadSTG file.tempFilePath
                                                          stg.Set(Some stg')
                                                          let genes = stg'.statesWithGeneTransitions |> Map.keys
                                                          let parameters = Seq.replicate (Seq.length genes) (2, 1, 80) |> Seq.zip genes |> Map.ofSeq
                                                          geneParameters.Set(parameters)
                                                          OK "Uploaded")

                path "/synthesise" >=> request (fun _ ->
                                                  let geneParameters' = geneParameters.Get
                                                  let stg' = Option.get stg.Get
                                                  let initialClasses = initialClasses.Get
                                                  let targetClasses = targetClasses.Get
                                                  if Map.isEmpty geneParameters' then
                                                    RequestErrors.BAD_REQUEST "No data"
                                                  else
                                                    stableStatesCancellation.Get.Cancel()
                                                    stableStatesTerminated.Set false
                                                    let cts = synthesisCancellation.Get
                                                    cts.Cancel()
                                                    let cts = new CancellationTokenSource()
                                                    synthesisCancellation.Set(cts)

                                                    for g in Map.keys geneParameters' do
                                                        resultsStream.[g] <- []
                                                        synthesisTerminated.[g] <- false

                                                    let comp = async {
                                                        let initialStates = stg'.states |> Set.filter (fun (s : State) -> Set.contains s.Class initialClasses) |> Set.toArray
                                                        let targetStates = stg'.states |> Set.filter (fun (s : State) -> Set.contains s.Class targetClasses) |> Set.toArray
                                                        let geneNames = Map.keys geneParameters' |> Array.ofSeq
                                                        let geneParameters = Map.toArray geneParameters'
                                                        let circuitMaps = ConcurrentDictionary()
                                                        let! allowedEdges = geneParameters |> Array.map (fun (g, (a, r, t)) ->
                                                                                                 async { let edges, circuits = Synthesis.findAllowedEdges g geneNames a r t (Map.find g stg'.statesWithGeneTransitions) (Map.find g stg'.statesWithoutGeneTransitions)
                                                                                                         circuitMaps.[g] <- circuits
                                                                                                         System.Console.WriteLine (sprintf "%s has %i edges / %i edges (%i neg edges) and %i candidate circuits." g (Set.count edges) (2 * Set.count (Map.find g stg'.statesWithGeneTransitions)) (Map.find g stg'.statesWithoutGeneTransitions |> Set.count) (Set.count circuits))
                                                                                                         return edges })
                                                                                            |> Async.Parallel
                                                        let allowedEdges = Set.unionMany allowedEdges
                                                        let paths = Synthesis.findPaths allowedEdges initialStates targetStates 

                                                        System.Console.WriteLine (sprintf "%i out %i targets reachable" (paths |> Array.map (fun l -> if List.isEmpty l then 0 else 1) |> Array.sum) (Array.length targetStates))

                                                        let results = match cluster.Get with
                                                                      | None ->
                                                                          geneParameters |> Array.map (fun (g, (a, r, t)) ->
                                                                                                         Seq.truncate 6 <| Synthesis.findFunctions g geneNames a r t paths (Map.find g stg'.statesWithGeneTransitions) circuitMaps.[g])
                                                                      | Some cluster ->
                                                                          geneParameters |> Array.map (fun (g, (a, r, t)) ->
                                                                                                         local { return Set.ofSeq << Seq.truncate 6 <| Synthesis.findFunctions g geneNames a r t paths (Map.find g stg'.statesWithGeneTransitions) circuitMaps.[g] }
                                                                                                         |> Cloud.WithFaultPolicy(FaultPolicy.WithMaxRetries 3))
                                                                                                         |> Cloud.Parallel
                                                                                                         |> cluster.Run
                                                                                                         |> Array.map (Set.toSeq)

                                                        do! results |> Array.mapi
                                                                (fun i results -> 
                                                                   async {
                                                                     let g = geneNames.[i]
                                                                     for r in results do
                                                                         resultsStream.[g] <- r :: resultsStream.[g]
                                                                     synthesisTerminated.[g] <- true
                                                                   }) |> Sequential |> Async.Ignore
                                                                   
                                                        match getModel resultsStream with
                                                            | Some model ->
                                                                Heatmap.heatmap "stableStatesHeatmap.png" (StableStates.stableStates model)
                                                                stableStatesTerminated.Set true
                                                            | None -> ignore ()
                                                    }
                                                    Async.Start(comp, cts.Token)
                                                    OK "Executing")
                path "/updateActivators" >=> request (fun req ->
                                                          let geneParameters' = geneParameters.Get
                                                          let gene, updatedActivators = parseJson req
                                                          let (_, currentRepressors, currentThreshold) = Map.find gene geneParameters'
                                                          let updatedActivators = if currentRepressors >= 3 && updatedActivators >= 3 then 2 else updatedActivators // rule out 3 3
                                                          let geneParameters' = Map.add gene (updatedActivators, currentRepressors, currentThreshold) geneParameters'
                                                          geneParameters.Set(geneParameters')
                                                          OK "Updated")
                path "/updateRepressors" >=> request (fun req ->
                                                          let geneParameters' = geneParameters.Get
                                                          let gene, updatedRepressors = parseJson req
                                                          let (currentActivators, _, currentThreshold) = Map.find gene geneParameters' // rule out 3 3
                                                          let updatedRepressors = if currentActivators >= 3 && updatedRepressors >= 3 then 2 else updatedRepressors // rule out 3 3
                                                          let geneParameters' = Map.add gene (currentActivators, updatedRepressors, currentThreshold) geneParameters'
                                                          geneParameters.Set(geneParameters')
                                                          OK "Updated")
                path "/updateThreshold" >=> request (fun req ->
                                                          let geneParameters' = geneParameters.Get
                                                          let gene, updatedThreshold = parseJson req
                                                          let (currentActivators, currentRepressors, _) = Map.find gene geneParameters'
                                                          let geneParameters' = Map.add gene (currentActivators, currentRepressors, updatedThreshold) geneParameters'
                                                          geneParameters.Set(geneParameters')
                                                          OK "Updated")
                path "/addInitialCellClass" >=> request (fun req ->
                                                             let initialClasses' = initialClasses.Get
                                                             let data = req.form |> List.head |> fst
                                                             initialClasses.Set <| Set.add data initialClasses'
                                                             OK "Updated")
                path "/removeInitialCellClass" >=> request (fun req ->
                                                             let initialClasses' = initialClasses.Get
                                                             let data = req.form |> List.head |> fst
                                                             initialClasses.Set <| Set.remove data initialClasses'
                                                             OK "Updated")
                path "/addTargetCellClass" >=> request (fun req ->
                                                             let targetClasses' = targetClasses.Get
                                                             let data = req.form |> List.head |> fst
                                                             targetClasses.Set <| Set.add data targetClasses'
                                                             OK "Updated")
                path "/removeTargetCellClass" >=> request (fun req ->
                                                             let targetClasses' = targetClasses.Get
                                                             let data = req.form |> List.head |> fst
                                                             targetClasses.Set <| Set.remove data targetClasses'
                                                             OK "Updated")
                path "/stableStates" >=> request (fun req ->
                                                      let kos, oes = parseKoOe req
                                                      koGenes.Set (Set.ofArray kos)
                                                      oeGenes.Set (Set.ofArray oes)
                                                      let cts' = stableStatesCancellation.Get
                                                      cts'.Cancel()
                                                      let cts' = new CancellationTokenSource()
                                                      stableStatesCancellation.Set(cts')
                                                      stableStatesTerminated.Set false
                                                      let comp = async {
                                                            match getModel resultsStream with
                                                            | Some model ->
                                                                let model = model
                                                                         |> StableStates.knockOut kos
                                                                         |> StableStates.overExpress oes
                                                                Heatmap.heatmap "stableStatesHeatmap.png" (StableStates.stableStates model)
                                                                stableStatesTerminated.Set true
                                                            | None -> ignore ()
                                                      }
                                                      Async.Start (comp, cts'.Token)
                                                      OK "Executing")

                path "/updateRunOnCloud" >=> request (fun req ->
                                                          cluster.Set None
                                                          if req.form |> List.head |> fst = "true" then
                                                              match deployment.Get with
                                                              | Some deployment ->
                                                                  let cluster' = connectToCluster' deployment
                                                                  match cluster' with
                                                                  | Some cluster' ->
                                                                      cluster.Set (Some cluster')
                                                                      System.IO.File.WriteAllText("settings.txt", "true")
                                                                      OK "Connected to Azure"
                                                                  | None ->
                                                                      ServerErrors.INTERNAL_ERROR "Could not connect to Azure"
                                                              | None ->
                                                                 ServerErrors.INTERNAL_ERROR "Could not connect to Azure"
                                                          else
                                                              System.IO.File.WriteAllText("settings.txt", "false")
                                                              OK "Set to run locally") ] ]
end

[<EntryPoint>]
let main _= 
    let state = ServerState()
    startWebServer webConfig state.App
    0
