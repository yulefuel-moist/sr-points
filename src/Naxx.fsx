#load "Parser.fsx"

open Parser
open System.IO

let path_res = __SOURCE_DIRECTORY__ + @"/../res/naxx/"
let path_html = __SOURCE_DIRECTORY__ + @"/../pages/Naxx-points.html"
let files = Directory.GetFiles(path_res)

let itemsNaxx =
    { entries = SrPointSystem.itemsFromFile (files |> Array.find (fun s -> Path.GetFileName(s) = "naxx-items.csv")) |> Array.sortBy (fun el -> el.itemName) }

let softReserves =
    files
    |> Array.filter (fun f -> Path.GetFileName(f).StartsWith("naxx-sr"))
    |> Array.map SoftReserves.loadFromFile
    |> Array.concat

let lootHistory =
    files
    |> Array.filter (fun f -> Path.GetFileName(f).StartsWith("naxx-loot"))
    |> Array.map (Gargul.loadFromFile)
    |> Array.concat

let pointsOnItems = SrPointSystem.computePoints itemsNaxx 5 softReserves lootHistory


HtmlRenderer.generateView "Naxx" pointsOnItems
|> HtmlRenderer.save path_html
|> ignore
