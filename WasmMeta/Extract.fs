module WasmMeta.Extract

open System

open WasmMeta.Tokenize

let (|Begin|_|) =
    function
    | head :: tail ->
        match head with
        | TexCommand tc -> if tc.Head.Name = "begin" then Some tail else None
        | _ -> None
    | _ -> None

let (|End|_|) =
    function
    | head :: tail ->
        match head with
        | TexCommand tc -> if tc.Head.Name = "end" then Some tail else None
        | _ -> None
    | _ -> None

let (|Production|_|) =
    function
    | head :: tail ->
        match head with
        | TexCommand prod ->
            if prod.Head.Name = "production" then
                Some(tail, prod.Args.Head.Name)
            else
                None
        | _ -> None
    | _ -> None

let (|Equal|_|) =
    function
    | head :: tail when head = TexWord "::=" -> Some tail
    | _ -> None

let (|Term|_|) =
    function
    | head :: tail ->
        match head with
        | TexCommand tc when tc.Head.Name |> String.forall (fun c -> Char.IsUpper(c) || Char.IsNumber(c)) ->
            Some(tail, tc.Head.Name)
        | _ -> None
    | _ -> None

let (|Nonterm|_|) =
    function
    | head :: tail ->
        match head with
        | TexCommand tc when tc.Head.Name |> String.forall (fun c -> Char.IsLower(c) || Char.IsNumber(c)) ->
            Some(tail, tc.Head.Name)
        | _ -> None
    | _ -> None

let (|Or|_|) =
    function
    | head :: tail when head = TexWord "|" -> Some tail
    | _ -> None

let (|MapsTo|_|) =
    function
    | head :: tail ->
        match head with
        | TexCommand { Head = { Name = name } } -> if name = "to" then Some tail else None
        | _ -> None
    | _ -> None

let (|RecordStart|_|) =
    function
    | head :: tail when head = TexWord @"\{" -> Some tail
    | _ -> None

let (|RecordEnd|_|) =
    function
    | head :: tail when head = TexWord @"\}" -> Some tail
    | _ -> None

let (|OptSeq|_|) =
    function
    | h1 :: h2 :: tail ->
        match h1, h2 with
        | TexWord "^", TexWord "?" -> Some tail
        | _ -> None
    | _ -> None

let (|ManyPossibleEmpty|_|) =
    function
    | h1 :: h2 :: tail ->
        match h1, h2 with
        | TexWord "^", TexWord "*" -> Some tail
        | _ -> None
    | _ -> None

let (|ManyN|_|) =
    function
    | h1 :: h2 :: tail ->
        match h1, h2 with
        | TexWord "^", TexWord n -> Some(tail, n)
        | _ -> None
    | _ -> None

let (|ManyNonEmpty|_|) =
    function
    | h1 :: h2 :: tail ->
        match h1, h2 with
        | TexWord "^", TexWord "+" -> Some tail
        | _ -> None
    | _ -> None

type SequenceType =
    | Optional
    | PossibleEmpty
    | NonEmpty
    | N of string


type Symbol =
    | STerm of string
    | SNonterm of string
    | SSeq of Symbol * SequenceType


type ProductionRule =
    { Name: string
      Lhs: Symbol list
      Rhs: Symbol list list }

type Extractor(source: TexToken list) =
    let rules = ResizeArray<ProductionRule>(1)

    let consume tokens =
        let mutable prodName = None
        let mutable onLhs = true
        let lhs = ResizeArray()
        let rhs = ResizeArray()
        let rhsBag = ResizeArray()
        let mutable inRecord = false

        let consumeTail tokens =
            match tokens with
            | OptSeq tail -> (tail, Optional) |> Some
            | ManyPossibleEmpty tail -> (tail, PossibleEmpty) |> Some
            | ManyN(tail, n) -> (tail, N n) |> Some
            | ManyNonEmpty tail -> (tail, NonEmpty) |> Some
            | _ -> None


        let rec loop tokens =
            match tokens with
            | Begin tail -> loop tail
            | End tail -> loop tail
            | Production(tail, name) ->
                match prodName with
                | None ->
                    prodName <- Some name
                    onLhs <- true
                    loop tail
                | Some _ -> tokens
            | Equal tail ->
                if not onLhs then
                    failwith "Equal cannot appear twice"
                else
                    onLhs <- false
                    loop tail
            | Term(tail, s) ->
                if onLhs then
                    lhs.Add(STerm s)
                    loop tail
                else
                    match consumeTail tail with
                    | Some(tail, kind) ->
                        rhsBag.Add(SSeq(STerm s, kind))
                        loop tail
                    | None ->
                        rhsBag.Add(STerm s)
                        loop tail
            | Nonterm(tail, s) ->
                if onLhs then
                    lhs.Add(STerm s)
                    loop tail
                else
                    match consumeTail tail with
                    | Some(tail, kind) ->
                        rhsBag.Add(SSeq(SNonterm s, kind))
                        loop tail
                    | None ->
                        rhsBag.Add(SNonterm s)
                        loop tail

            | Or tail ->
                let oldBag = ResizeArray(rhsBag)
                rhs.Add(oldBag)
                rhsBag.Clear()
                loop tail

            | MapsTo tail -> loop tail
            | RecordStart tail ->
                inRecord <- true
                loop tail
            | RecordEnd tail ->
                inRecord <- false
                loop tail

            | [] -> []
            | tks -> failwithf "Not addressed tokens %A" tks

        let tail = loop tokens

        if rhsBag.Count > 0 then
            let oldBag = ResizeArray(rhsBag)
            rhs.Add(oldBag)
            rhsBag.Clear()

        let rule =
            { Name = prodName.Value
              Lhs = Seq.toList lhs
              Rhs = Seq.toList (Seq.map (fun symbols -> Seq.toList symbols) rhs) }

        rules.Add(rule)
        tail

    member _.Run() =
        let rec loop tokens =
            match consume tokens with
            | [] -> ()
            | tail -> loop tail

        loop source
        rules
