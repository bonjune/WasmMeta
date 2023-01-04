module WasmMeta.Extract

open FSharp.Data
open FParsec

let extractValueSections () =
    let document =
        HtmlDocument.Load(__SOURCE_DIRECTORY__ + "/../resources/webassembly.github.io/spec/core/syntax/values.html")
    document.CssSelect("section[id='values'] > section")

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
    | Special of string
    | Named of string * Symbol
    | Optional of Symbol            // A^?
    | ManyPossibleEmpty of Symbol   // A^*
    | Many of Symbol                // A^n
    | NonEmpty of Symbol            // A^+

type Production =
    | Union of Symbol list
    | Tuple of Symbol list
    | Vec of Symbol
    | Record of Map<Symbol, Symbol>

let inline toString (cs: char list) =
    System.String.Concat(Array.ofList cs)

let private skipTexSpaces = choice [
    skipString @"\quad"
    skipString @"\qquad"
    skipString @"\ "
    skipString @"\,"
    skipString @"\:"
    skipString @"\;"
    skipString @"\!"
]
let private ws = spaces .>> many skipTexSpaces .>> spaces

let private skipOpen = skipChar '{'
let private skipClose = skipChar '}'
let private skipOr = skipString @"~|~" .>> ws

let private pBracket inner = skipOpen >>. many1Till inner skipClose
let private skipBracket = skipOpen >>. skipMany1Till skipAnyChar skipClose

// Parsing Tex
[<RequireQualifiedAccess>]
module Tex =
    let skipBegin =
        ws
        .>> skipString @"\[\begin{split}\begin{array}"
        .>> skipBracket
        .>> ws

    let skipEnd =
        skipString @"\end{array}\end{split}"
        .>> (skipString @"\]" <|> ws)
        .>> ws

    let skipDefine = skipString @"&::=&" .>> ws
    let skipProdSep = skipString @"\\" .>> ws

    /// Parse `\def\mathdef2599#1{{}}\mathdef2599{number type} & `
    let pMathDef =
        skipString @"\def\mathdef"
        .>> pint32 // 2599
        .>> skipString @"#1{{}}\mathdef"
        .>> pint32 // 2599
        .>> skipBracket
        .>> ws .>> skipChar '&' .>> ws

    let private skipHref' = skipString @"\href" .>> skipBracket
    let skipHref = attempt skipHref' <|> ws


[<RequireQualifiedAccess>]
module Symbol =
    let pNonterm =
        skipString @"\mathit"
        >>. pBracket anyChar
        |>> (fun cs -> Nonterm (toString cs))

    let pNontermOf s =
        skipString @"\mathit"
        >>. between skipOpen skipClose (pstring s)
        |>> Nonterm

    let pTerm =
        skipString @"\mathsf"
        >>. pBracket anyChar
        |>> (fun cs -> Term (toString cs))


    let pArrow = pstring @"\rightarrow" |>> Special

    let private pSymbolFirst =
        Tex.skipHref
        >>. between skipOpen skipClose (choice [pNonterm; pTerm; pArrow])

    let pSymbolNaive    =   pSymbolFirst .>> ws

    let pSymbolOptional =   pSymbolFirst .>> skipString @"^?" .>> ws |>> Optional
    
    let pSymbolMany     =   pSymbolFirst .>> skipString @"^n" .>> ws |>> Many
    
    let pSymbolManyPossibleEmpty =
        pSymbolFirst .>> skipString @"^\ast" .>> ws |>> ManyPossibleEmpty

    let pSymbolNamed =
        Tex.skipHref
        >>. between skipOpen skipClose pTerm
        .>> pchar '~'
        .>>. choice [
            attempt pSymbolOptional
            attempt pSymbolMany
            attempt pSymbolManyPossibleEmpty
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
            attempt pSymbolMany
            attempt pSymbolManyPossibleEmpty
            pSymbolNaive
        ]

[<RequireQualifiedAccess>]
module Production =
    /// A tuple separated by ws
    let private pTuplews = many1 Symbol.pSymbol

    /// A tuple separated by '~', but with more than two occurrences of symbols
    let private pTupleTilde =
        Symbol.pSymbol
        .>> pchar '~'
        .>>. sepBy1 Symbol.pSymbol (pchar '~')
        |>> (fun (head, tail) -> head :: tail)

    let pTuple =
        choice [
            attempt pTupleTilde
            pTuplews
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
    let private pPairs = sepBy1 pPair (skipChar ',' .>> ws)
    /// {key _term_, key _term_}
    let pRecord =
        between (skipString @"\{" .>> ws) (skipString @"\}") pPairs
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
    .>> ws

type ProductionRule = Symbol * Production

let pProd: Parser<ProductionRule, _> =
    pLhs
    .>> Tex.skipDefine
    .>> (skipChar '[' <|> ws)
    .>>. pRhs
    .>> (skipChar ']' <|> ws)
    .>> ws

let pFormula: Parser<_, unit> =
    between Tex.skipBegin Tex.skipEnd (sepEndBy1 pProd Tex.skipProdSep)