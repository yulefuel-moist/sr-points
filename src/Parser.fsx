#r "nuget: Giraffe.ViewEngine"

open System.IO
open Giraffe.ViewEngine


//
// A simple generic immutable database
// --------------------------------------------------------

type SimpleDb<'Entry> = { entries: array<'Entry> }

//
// Methods
// --------------------------------------------------------

module SimpleDb =
    let createEmpty<'Entry> () =
        let db: SimpleDb<'Entry> = { entries = [||] }
        db

    let add entry database =
        { entries = (Array.append database.entries [| entry |]) }

    let tryFind predicate database =
        database.entries |> Array.tryFind predicate


//
// Domain types
// --------------------------------------------------------

type ItemId = int
type RaiderName = string

type SrEntry =
    { itemId: ItemId
      raiderName: RaiderName
      dateTime: System.DateTime }

type Points = int

type ItemInPointSystem =
    { itemName: RaiderName
      itemId: ItemId
      dateAdded: System.DateTime }

type PointsRecord = array<RaiderName * Points>

type PointsOnItem =
    { itemId: ItemId; points: PointsRecord }


//
// Parser and tokenizer for soft-reserves and raid logs
// --------------------------------------------------------

module ParserUtils =

    //
    // Types
    // --------------------------------------------------------

    type Token =
        | Str of string
        | Id of string
        | Num of int

    //
    // Utility functions
    // --------------------------------------------------------

    let isChar c = System.Char.IsLetter c
    let isDigit c = System.Char.IsAsciiDigit c


    //
    // Character parsers
    // --------------------------------------------------------

    let parseInline stopChar input =
        let toStr (seq: list<char>) = seq |> System.String.Concat

        let rec loop acc input =
            match input with
            | c :: rest ->
                if c = stopChar then
                    Some(toStr (List.rev acc), rest)
                else
                    loop (c :: acc) rest
            | [] -> None

        loop [] input

    let parseString input = parseInline '\"' input

    let parseParen input = parseInline ']' input

    let parseIdentifier input =
        let toStr (acc: list<char>) = List.rev acc |> System.String.Concat

        let rec loop acc input =
            match input with
            | c :: rest ->
                if isChar c then
                    loop (c :: acc) rest
                else
                    Some(toStr acc, rest)
            | [] -> Some(toStr acc, [])

        loop [] input

    let parseNumber input =
        let toNum (seq: list<char>) rest =
            match seq |> System.String.Concat |> System.Int32.TryParse with
            | true, out -> Some(out, rest)
            | false, _ -> None

        let rec loop acc input =
            match input with
            | c :: rest ->
                if isDigit (c) then
                    loop (c :: acc) rest
                else
                    toNum (List.rev acc) rest
            | [] -> toNum acc []

        loop [] input

    //
    // Tokenizer
    // --------------------------------------------------------

    let tokenize input =
        let rec loop tokens input =
            match input with
            | c :: tail ->
                if isDigit c then
                    match parseNumber (c :: tail) with
                    | Some(num, rest) -> loop ((Num num) :: tokens) rest
                    | _ -> tokens
                elif isChar c then
                    match parseIdentifier (c :: tail) with
                    | Some(id, rest) -> loop ((Id id) :: tokens) rest
                    | _ -> tokens
                elif c = '\"' then
                    match parseString tail with
                    | Some(str, rest) -> loop ((Str str) :: tokens) rest
                    | _ -> tokens
                elif c = '[' then
                    match parseParen tail with
                    | Some(str, rest) -> loop ((Str str) :: tokens) rest
                    | _ -> tokens
                else
                    loop tokens tail
            | [] -> tokens

        loop [] input |> List.rev


    //
    // Token parsers
    // --------------------------------------------------------

    let tryParseItemId (itemId: Token) =
        match itemId with
        | Num num -> Some num
        | _ -> None

    let tryParseItemName (itemName: Token) =
        match itemName with
        | Str name -> Some name
        | _ -> None

    let tryParseRaiderName (raiderName: Token) =
        match raiderName with
        | Id name -> Some name
        | _ -> None

    let tryParseRaiderId (raiderId: Token) =
        match raiderId with
        | Num id -> Some id
        | _ -> None

    let tryParseDatetime (dateTime: Token) =
        match dateTime with
        | Str dt ->
            match System.DateTime.TryParse dt with
            | true, result -> Some result
            | false, _ -> None
        | _ -> None


//
// Soft-reserves export parser
// --------------------------------------------------------

module SoftReserves =
    open ParserUtils

    let tryParseSoftReserveInput (itemId: Token) (raiderName: Token) (date: Token) =
        let parsedItemId = tryParseItemId itemId
        let parsedRaiderName = tryParseRaiderName raiderName
        let parsedDatetime = tryParseDatetime date

        match parsedItemId, parsedRaiderName, parsedDatetime with
        | Some itemId, Some raiderName, Some date ->
            Some
                { itemId = itemId
                  raiderName = raiderName
                  dateTime = date }
        | _ -> None

    let parse (input: array<string>) =
        let header = input[0] |> List.ofSeq |> tokenize
        let indexRaiderName = List.findIndex ((=) (Id "Name")) header
        let indexItemId = List.findIndex ((=) (Id "ItemId")) header
        let indexDatetime = List.findIndex ((=) (Id "Date")) header

        input
        |> Array.skip 1
        |> Array.map (fun line ->
            let tokens = line |> List.ofSeq |> tokenize |> List.toArray
            tryParseSoftReserveInput tokens[indexItemId] tokens[indexRaiderName] tokens[indexDatetime])

    let loadFromFile filename =
        filename |> File.ReadAllLines |> parse |> Array.choose id


//
// Gargul export parser
// --------------------------------------------------------

module Gargul =
    open ParserUtils

    let tryParseGargulInput (itemId: Token) (raiderName: Token) (date: Token) =
        let parsedItemId = tryParseItemId itemId
        let parsedRaiderName = tryParseRaiderName raiderName
        let parsedDatetime = tryParseDatetime date

        match parsedItemId, parsedRaiderName, parsedDatetime with
        | Some itemId, Some raiderId, Some date -> Some(itemId, raiderId, date)
        | _ -> None

    let parse (input: array<string>) =
        let header = input[0] |> List.ofSeq |> tokenize

        let indexRaiderName = 2 + List.findIndex ((=) (Id "character")) header
        let indexItemId = 2 + List.findIndex ((=) (Id "itemID")) header

        input
        |> Array.skip 1
        |> Array.map (fun line ->
            printfn "%s\n" line
            let tokens = line |> List.ofSeq |> tokenize |> List.toArray

            let dateToken =
                match tokens[0], tokens[1], tokens[2] with
                | Num y, Num m, Num d -> Str(sprintf "%d-%d-%d" y m d)
                | _ -> Str ""

            tryParseGargulInput tokens[indexItemId] tokens[indexRaiderName] dateToken)

    let loadFromFile filename =
        filename |> File.ReadAllLines |> parse |> Array.choose id



module SrPointSystem =
    let createEmpty () =
        let entries: array<ItemInPointSystem> = [||]
        { entries = entries }

    let add (fst: PointsRecord) (snd: PointsRecord) =
        Array.append fst snd
        |> Array.groupBy (fun (raiderName, _) -> raiderName)
        |> Array.map (fun (raiderName, entries) -> (raiderName, entries |> Array.sumBy (fun (_, points) -> points)))

    let itemsFromFile file =
        let lines = File.ReadAllLines file

        seq {
            for line in lines do
                let tokens = ParserUtils.tokenize (List.ofSeq line)
                let itemName = ParserUtils.tryParseItemName tokens[0]
                let itemId = ParserUtils.tryParseItemId tokens[1]
                let dateAdded = ParserUtils.tryParseDatetime tokens[2]

                match itemName, itemId, dateAdded with
                | Some itemName, Some itemId, Some dateAdded ->
                    yield
                        Some
                            { itemName = itemName
                              itemId = itemId
                              dateAdded = dateAdded }
                | _ -> yield None
        }
        |> Seq.skip 1
        |> Seq.choose id
        |> Seq.toArray

    let computePoints
        (itemsDb: SimpleDb<ItemInPointSystem>)
        (softReserves: array<SrEntry>)
        (lootHistory: array<ItemId * RaiderName * System.DateTime>)
        =
        let itemIds = itemsDb.entries |> Array.map (fun entry -> entry.itemId)
        let date = softReserves |> Array.map (fun entry -> entry.dateTime) |> Array.max

        let srPoints =
            softReserves
            |> Array.filter (fun srEntry -> Array.contains (srEntry.itemId) itemIds)
            |> Array.groupBy (fun srEntry -> srEntry.itemId)
            |> Array.map (fun (itemId, srEntries) ->
                let pointsOnItem =
                    srEntries
                    |> Array.map (fun srEntry -> srEntry.raiderName)
                    |> Array.countBy id
                    |> Array.map (fun (raider, count) -> (raider, count * 10))

                { itemId = itemId
                  points = pointsOnItem })

        [| for item in itemsDb.entries do
               match srPoints |> Array.tryFind (fun p -> p.itemId = item.itemId) with
               | Some points ->
                   if item.dateAdded < date then
                       points
                   else
                       { itemId = item.itemId; points = [||] }
               | None -> { itemId = item.itemId; points = [||] } |]
        |> Array.map (fun pointsOnItem ->
            let wonBy =
                lootHistory
                |> Array.filter (fun (itemId, _, _) -> itemId = pointsOnItem.itemId)
                |> Array.map (fun (_, raiderName, _) -> raiderName)

            let updatedPoints =
                pointsOnItem.points
                |> Array.filter (fun (raiderName, _) -> wonBy |> Array.contains raiderName |> not)

            { itemId = pointsOnItem.itemId
              points = updatedPoints })


module HtmlRenderer =

    let generateView instanceName (pointsOnItems: PointsOnItem[]) =
        html
            []
            [ head
                  []
                  [ script
                        [ _type "application/javascript" ]
                        [ rawText
                              """
                        const whTooltips = {colorLinks: true, iconizeLinks: true, renameLinks: true};
                    """ ]
                    script [ _type "application/javascript"; _src "https://wow.zamimg.com/js/tooltips.js" ] []
                    (tag "link") [ attr "rel" "stylesheet"; attr "href" "../style.css" ] []
                    title [] [ str (sprintf "%s Points" instanceName) ] ]
              body
                  []
                  [ h1 [] [ str (sprintf "%s Soft-reserve points" instanceName) ]
                    p [] [ str (sprintf "Updated %s" (System.DateTime.Now.ToString())) ]
                    table
                        []
                        [ for pointsOnItem in pointsOnItems ->
                              table
                                  []
                                  [ h3
                                        []
                                        [ a
                                              [ _href (
                                                    sprintf
                                                        "https://wow.wowhead.com/classic/item=%s"
                                                        (pointsOnItem.itemId |> string)
                                                ) ]
                                              [ str "items" ] ]
                                    p
                                        []
                                        [ yield!
                                              pointsOnItem.points
                                              |> Array.map (fun (name, points) ->
                                                  tr [] [ td [] [ str name ]; td [] [ str (points |> string) ] ]) ] ] ] ] ]

    let save file view =
        let document = RenderView.AsString.htmlNode view
        File.WriteAllText(file, document)
