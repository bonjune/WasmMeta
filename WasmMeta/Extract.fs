module WasmMeta.Extract

open FSharp.Data
open FParsec

type WasmSyntaxTypes = HtmlProvider<"/Users/bonjune/Research/WasmMeta/resources/webassembly.github.io/spec/core/syntax/types.html">

let extractTypeSections () =
    let document = WasmSyntaxTypes.GetSample().Html
    let sections = document.CssSelect("section[id$='-types']")
    sections

let extractFormula (section:HtmlNode) =
    let formula = section.CssSelect("div[class~='math']").Head.InnerText()
    formula.Replace(" \\[", "").Replace("\\]", "")

type Symbol =
    | Term of string
    | Nonterm of string
    | Optional of Symbol
    | Special of string

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

let private skipBegin =
    skipString @"\[" <|> spaces
    .>> skipString @"\begin{split}\begin{array}"
    .>> skipOpen
    .>> skipMany1Till skipAnyChar skipClose
    .>> spaces

let private skipEnd =
    skipString @"\end{array}\end{split}"
    .>> (skipString @"\]" <|> spaces)

let private skipDefine = skipString @"&::=&" .>> spaces
let private skipProdSep = skipString @"\\" .>> spaces

let private pMathDef =
    skipString @"\def\mathdef2599#1{{}}\mathdef2599"
    .>> skipOpen
    >>. many1Till anyChar skipClose
    .>> spaces .>> skipChar '&' .>> spaces

let private skipHref =
    skipString @"\href"
    .>> skipOpen
    .>> skipMany1Till skipAnyChar skipClose

// Parsing symbols

let private pNonterm =
    skipString @"\mathit"
    >>. skipOpen
    >>. many1Till anyChar skipClose
    |>> (fun cs -> Nonterm (toString cs))

let private pNontermOf s =
    skipString @"\mathit"
    >>. between skipOpen skipClose (pstring s)
    |>> Nonterm

let private pTerm =
    skipString @"\mathsf"
    >>. skipOpen
    >>. many1Till anyChar skipClose
    |>> (fun cs -> Term (toString cs))

let private pArrow = pstring @"\rightarrow" |>> Special

let private pSymbolInner = choice [pNonterm; pTerm; pArrow]

let private pSymbolNaive =
    skipHref
    >>. between skipOpen skipClose pSymbolInner
    .>> spaces

let private pSymbolOptional =
    skipHref
    >>. between skipOpen skipClose pSymbolInner
    .>> skipString @"^?"
    .>> spaces
    |>> Optional

let pSymbol = choice [attempt pSymbolOptional; pSymbolNaive]

// Parse productions

/// `A_1` | ... | `A_n`
let pUnion = parse {
    let! symbols = sepBy1 pSymbolNaive skipOr
    if symbols.Length = 1
    then return! fail "Require more than one symbol"
    else return Union symbols
}

/// `A_1` ... `A_n`
let pTuple = many1 pSymbolNaive |>> Tuple

let pVec =
    skipHref
    >>. between skipOpen skipClose (pNontermOf "vec")
    >>. between (skipChar '(') (skipChar ')') pSymbolNaive
    |>> Vec

let private pPair = pSymbolNaive .>> pchar '~' .>>. pSymbol
let private pPairs = sepBy1 pPair (skipChar ',' .>> spaces)
/// {key _term_, key _term_}
let pRecord = between (skipString @"\{" .>> spaces) (skipString @"\}") pPairs |>> (Map >> Record)

let pLhs = pMathDef >>. pSymbolNaive

/// Reference: https://www.quanttec.com/fparsec/users-guide/parsing-alternatives.html
let pRhs =
    choice [attempt pRecord; attempt pUnion; attempt pVec; pTuple]
    .>> spaces

let pProd =
    pLhs
    .>> skipDefine
    .>> (skipChar '[' <|> spaces)
    .>>. pRhs
    .>> (skipChar ']' <|> spaces)
    .>> spaces

let pFormula: Parser<_, unit> =
    between skipBegin skipEnd (sepEndBy1 pProd skipProdSep)