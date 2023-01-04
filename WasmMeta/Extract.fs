module WasmMeta.Extract

open FSharp.Data
open FParsec


let extractTypeSections () =
    let document =
        HtmlDocument.Load(__SOURCE_DIRECTORY__ + "/../resources/webassembly.github.io/spec/core/syntax/types.html")
    document.CssSelect("section[id$='-types']")

let extractFormula (section:HtmlNode) =
    let mathDiv = section.CssSelect("div[class~='math']")
    if mathDiv.Length = 1
    then mathDiv.Head.InnerText()
    else failwith "<div class=\"math ...\"> must appear once in a section"

type Symbol =
    | Term of string
    | Nonterm of string
    | Optional of Symbol
    | Special of string
    | Named of string * Symbol

type Production =
    | Union of Symbol list
    | Tuple of Symbol list
    | Vec of Symbol
    | Record of Map<Symbol, Symbol>

let inline toString (cs: char list) =
    System.String.Concat(Array.ofList cs)

let private skipOpen = skipChar '{'
let private skipClose = skipChar '}'
let private skipOr = skipString @"~|~" .>> spaces

// Parsing Tex
[<RequireQualifiedAccess>]
module Tex =
    let skipBegin =
        spaces
        .>> skipString @"\[\begin{split}\begin{array}"
        .>> skipOpen
        .>> skipMany1Till skipAnyChar skipClose
        .>> spaces

    let skipEnd =
        skipString @"\end{array}\end{split}"
        .>> (skipString @"\]" <|> spaces)
        .>> spaces

    let skipDefine = skipString @"&::=&" .>> spaces
    let skipProdSep = skipString @"\\" .>> spaces

    let pMathDef =
        skipString @"\def\mathdef2599#1{{}}\mathdef2599"
        .>> skipOpen
        >>. many1Till anyChar skipClose
        .>> spaces .>> skipChar '&' .>> spaces

    let private skipHref' =
        skipString @"\href"
        .>> skipOpen
        .>> skipMany1Till skipAnyChar skipClose

    let skipHref = attempt skipHref' <|> spaces


[<RequireQualifiedAccess>]
module Symbol =
    let pNonterm =
        skipString @"\mathit"
        >>. skipOpen
        >>. many1Till anyChar skipClose
        |>> (fun cs -> Nonterm (toString cs))

    let pNontermOf s =
        skipString @"\mathit"
        >>. between skipOpen skipClose (pstring s)
        |>> Nonterm

    let pTerm =
        skipString @"\mathsf"
        >>. skipOpen
        >>. many1Till anyChar skipClose
        |>> (fun cs -> Term (toString cs))


    let pArrow = pstring @"\rightarrow" |>> Special

    let pSymbolInner = choice [pNonterm; pTerm; pArrow]

    let pSymbolNaive =
        Tex.skipHref
        >>. between skipOpen skipClose pSymbolInner
        .>> spaces

    let pSymbolOptional =
        Tex.skipHref
        >>. between skipOpen skipClose pSymbolInner
        .>> skipString @"^?"
        .>> spaces
        |>> Optional

    let pSymbolNamed =
        Tex.skipHref
        >>. between skipOpen skipClose pTerm
        .>> pchar '~'
        .>>. choice [
            attempt pSymbolOptional
            pSymbolNaive
        ]
        |>> (fun (name, sym) ->
            match name with
            | Term t -> Named (t, sym)
            | _ -> failwith "unreachable"
        )

    let pSymbol =
        choice [
            attempt pSymbolNamed
            attempt pSymbolOptional
            pSymbolNaive
        ]

[<RequireQualifiedAccess>]
module Production =
    /// A tuple separated by spaces
    let private pTupleSpaces = many1 Symbol.pSymbol

    /// A tuple separated by '~', but with more than two occurrences of symbols
    let private pTupleTilde =
        Symbol.pSymbol
        .>> pchar '~'
        .>>. sepBy1 Symbol.pSymbol (pchar '~')
        |>> (fun (head, tail) -> head :: tail)

    let pTuple =
        choice [
            attempt pTupleTilde
            pTupleSpaces
        ]
        |>> Tuple

    /// `A_1` | ... | `A_n`
    let pUnion = parse {
        let! symbols = sepBy1 Symbol.pSymbol skipOr
        if symbols.Length = 1
        then return! fail "Require more than one tuple"
        else return Union symbols
    }

    let pVec =
        Tex.skipHref
        >>. between skipOpen skipClose (Symbol.pNontermOf "vec")
        >>. between (skipChar '(') (skipChar ')') Symbol.pSymbolNaive
        |>> Vec

    let private pPair = Symbol.pSymbolNaive .>> pchar '~' .>>. Symbol.pSymbol
    let private pPairs = sepBy1 pPair (skipChar ',' .>> spaces)
    /// {key _term_, key _term_}
    let pRecord =
        between (skipString @"\{" .>> spaces) (skipString @"\}") pPairs
        |>> (Map >> Record)

// Parse formulae

let pLhs = Tex.pMathDef >>. Symbol.pSymbolNaive

/// Reference: https://www.quanttec.com/fparsec/users-guide/parsing-alternatives.html
let pRhs =
    choice [
        attempt Production.pRecord
        attempt Production.pUnion
        attempt Production.pVec
        Production.pTuple
    ]
    .>> spaces

type ProductionRule = Symbol * Production

let pProd: Parser<ProductionRule, _> =
    pLhs
    .>> Tex.skipDefine
    .>> (skipChar '[' <|> spaces)
    .>>. pRhs
    .>> (skipChar ']' <|> spaces)
    .>> spaces

let pFormula: Parser<_, unit> =
    between Tex.skipBegin Tex.skipEnd (sepEndBy1 pProd Tex.skipProdSep)