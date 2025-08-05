#load "Parser.fsx"

open Parser
open System.IO

let path_res = __SOURCE_DIRECTORY__ + @"/../res/bwl+mc/"
let path_html = __SOURCE_DIRECTORY__ + @"/../pages/BWL+MC-points.html"
let files = Directory.GetFiles(path_res)

let itemsBWLMC =
    { entries = SrPointSystem.itemsFromFile (files |> Array.find (fun s -> Path.GetFileName(s) = "bwl+mc-items.csv")) }

let softReserves =
    files
    |> Array.filter (fun f ->
        Path.GetFileName(f).StartsWith("bwl-sr")
        || Path.GetFileName(f).StartsWith("mc-sr"))
    |> Array.map SoftReserves.loadFromFile
    |> Array.concat

let lootHistory =
    files
    |> Array.filter (fun f -> Path.GetFileName(f).StartsWith("bwl+mc-loot"))
    |> Array.map (Gargul.loadFromFile)
    |> Array.concat

let pointsOnItems = SrPointSystem.computePoints itemsBWLMC 10 softReserves lootHistory


HtmlRenderer.generateView "BWL+MC" pointsOnItems
|> HtmlRenderer.save path_html
|> ignore
