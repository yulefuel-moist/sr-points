#load "Parser.fsx"

open Parser
open System.IO

let path_res = __SOURCE_DIRECTORY__ + @"/../res/aq40/"
let path_html = __SOURCE_DIRECTORY__ + @"/../pages/AQ40-points.html"
let files = Directory.GetFiles(path_res)

let itemsAQ40 =
    { entries = SrPointSystem.itemsFromFile (files |> Array.find (fun s -> Path.GetFileName(s) = "aq40-items.csv")) }

let softReserves =
    files
    |> Array.filter (fun f -> Path.GetFileName(f).StartsWith("aq40-sr"))
    |> Array.map SoftReserves.loadFromFile
    |> Array.concat

let lootHistory =
    files
    |> Array.filter (fun f -> Path.GetFileName(f).StartsWith("aq40-loot"))
    |> Array.map (Gargul.loadFromFile)
    |> Array.concat

let pointsOnItems = SrPointSystem.computePoints itemsAQ40 softReserves lootHistory


HtmlRenderer.generateView "AQ40" pointsOnItems
|> HtmlRenderer.save path_html
|> ignore
