module Heatmap

open RProvider
open RProvider.grDevices
open RProvider.gplots // this is a dependency users need to install
open FSharpx.Collections

let private boolToInt b = if b then 1 else 0

let private unwrap data =
    List.map (Map.valueList) data |> List.concat |> List.map (boolToInt)

let private eval (text:string) =
  R.eval(R.parse(namedParams ["text", text ]))

let private plotHeatmap genes data =
    let data = if List.length data = 1 then [List.head data; List.head data] else data
    let data = R.matrix(nrow=List.length data, ncol=Seq.length genes, data=unwrap data, byrow=true)

    let na = eval("NA")
    R.heatmap_2(data,
                dendrogram = "none",
                trace = "none",
                rowsep = [1..9],
                colsep = [1..20],
                sepwidth = [0.1; 0.1],
                sepcolor = "grey",
                col = ["#3c55a5"; "#eb2323"],               
                lhei = [1; 12],
                lwid = [1; 12],
                cexRow = 3,
                cexCol = 3,
                margins = [12; 6],
                key = false,
                labCol = genes,
                labRow = na) |> ignore

let heatmap filename data =
    if List.length data >= 20 then ()
    else
        try
            let genes = List.head data |> Map.keys
            R.png(filename, width=1000, height=500) |> ignore
            plotHeatmap genes data
            R.dev_off() |> ignore
        with
        | _ -> ()
