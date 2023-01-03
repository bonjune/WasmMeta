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
    | Special of string

type Production = {
    ProductionKind: ProductionKind
    Symbols: Symbol list
}
with
    static member MkUnion symbols =
        { ProductionKind = ProductionKind.Union; Symbols = symbols }
    static member MkTuple symbols =
        { ProductionKind = ProductionKind.Tuple; Symbols = symbols }
    static member MkVec symbol =
        { ProductionKind = ProductionKind.Vec; Symbols = [symbol] }

and ProductionKind =
    | Union = 1
    | Tuple = 2
    | Vec = 3

let inline toString (cs: char list) =
    System.String.Concat(Array.ofList cs)

let skipOpen = skipChar '{'
let skipClose = skipChar '}'
let skipOr = skipString @"~|~" .>> spaces

// Parsing Tex


let skipBegin =
    skipString @"\[" <|> spaces
    .>> skipString @"\begin{split}\begin{array}"
    .>> skipOpen
    .>> skipMany1Till skipAnyChar skipClose
    .>> spaces

let skipEnd =
    skipString @"\end{array}\end{split}"
    .>> (skipString @"\]" <|> spaces)

let skipDefine = skipString @"&::=&" .>> spaces
let skipProdSep = skipString @"\\" .>> spaces

let pMathDef =
    skipString @"\def\mathdef2599#1{{}}\mathdef2599"
    .>> skipOpen
    >>. many1Till anyChar skipClose
    .>> spaces .>> skipChar '&' .>> spaces

let skipHref =
    skipString @"\href"
    .>> skipOpen
    .>> skipMany1Till skipAnyChar skipClose

// Parsing texts into Symbols and Productions
let pMathit =
    skipString @"\mathit"
    >>. skipOpen
    >>. many1Till anyChar skipClose
    |>> (fun cs -> Nonterm (toString cs))

let pMathsf =
    skipString @"\mathsf"
    >>. skipOpen
    >>. many1Till anyChar skipClose
    |>> (fun cs -> Term (toString cs))

let pArrow = pstring @"\rightarrow" |>> Special

let pSymbol =
    skipHref
    >>. between skipOpen skipClose (pMathit <|> pMathsf <|> pArrow)
    .>> spaces

/// `A_1` | ... | `A_n`
let pUnion = parse {
    let! symbols = sepBy1 pSymbol skipOr
    if symbols.Length = 1
    then return! fail "Require more than one symbol"
    else return Production.MkUnion symbols
}

/// `A_1` ... `A_n`
let pTuple = many1 pSymbol |>> Production.MkTuple

let pLhs = pMathDef >>. pSymbol

/// Reference: https://www.quanttec.com/fparsec/users-guide/parsing-alternatives.html
let pRhs = choice [attempt pUnion; pTuple] .>> spaces
let pProd =
    pLhs
    .>> skipDefine
    .>> (skipChar '[' <|> spaces)
    .>>. pRhs
    .>> (skipChar ']' <|> spaces)

let pFormula: Parser<_, unit> =
    between skipBegin skipEnd (sepEndBy1 pProd skipProdSep)