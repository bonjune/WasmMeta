module WasmMeta.Extract

open FSharp.Data
open FParsec
open FParsec.CharParsers

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

and ProductionKind =
    | Union = 1
    | Tuple = 2



let inline toString (cs: char list) =
    System.String.Concat(Array.ofList cs)

let skipOpen = skipChar '{'
let skipClose = skipChar '}'
let skipOr = skipString @"~|~" .>> spaces

// Parsing Tex


let skipBegin =
    skipString @"\begin{split}\begin{array}"
    .>> skipOpen
    .>> skipMany1Till skipAnyChar skipClose
    .>> spaces

let skipEnd = skipString @"\end{array}\end{split}"

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
let pProd = pLhs .>> skipDefine .>>. pRhs

let pFormula: Parser<_, unit> =
    between skipBegin skipEnd (sepEndBy1 pProd skipProdSep)

// """
// \begin{split}\begin{array}{llcl}

// \def\mathdef2519#1{{}}\mathdef2519{width} & \mathit{nn}, \mathit{mm} &::=&
//   \mathsf{32} ~|~ \mathsf{64} \\

// \def\mathdef2519#1{{}}\mathdef2519{signedness} & \href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} &amp;::=&amp;
//   \mathsf{u} ~|~ \mathsf{s} \\

// \def\mathdef2519#1{{}}\mathdef2519{instruction} &amp; \href{../syntax/instructions.html#syntax-instr}{\mathit{instr}} &amp;::=&amp;
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{const}}~\href{../syntax/values.html#syntax-int}{\def\mathdef2558#1{{\mathit{i#1}}}\mathdef2558{\mathit{nn}}} ~|~
//   \mathsf{f}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{const}}~\href{../syntax/values.html#syntax-float}{\def\mathdef2559#1{{\mathit{f#1}}}\mathdef2559{\mathit{nn}}} \\&amp;&amp;|&amp;
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-iunop}{\mathit{iunop}} ~|~
//   \mathsf{f}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-funop}{\mathit{funop}} \\&amp;&amp;|&amp;
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-ibinop}{\mathit{ibinop}} ~|~
//   \mathsf{f}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-fbinop}{\mathit{fbinop}} \\&amp;&amp;|&amp;
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-itestop}{\mathit{itestop}} \\&amp;&amp;|&amp;
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-irelop}{\mathit{irelop}} ~|~
//   \mathsf{f}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-frelop}{\mathit{frelop}} \\&amp;&amp;|&amp;
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{extend}}\mathsf{8\_s} ~|~
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{extend}}\mathsf{16\_s} ~|~
//   \mathsf{i64.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{extend}}\mathsf{32\_s} \\&amp;&amp;|&amp;
//   \mathsf{i32.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{wrap}}\mathsf{\_i64} ~|~
//   \mathsf{i64.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{extend}}\mathsf{\_i32}\mathsf{\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} ~|~
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{trunc}}\mathsf{\_f}\mathit{mm}\mathsf{\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} \\&amp;&amp;|&amp;
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{trunc}}\mathsf{\_sat\_f}\mathit{mm}\mathsf{\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} \\&amp;&amp;|&amp;
//   \mathsf{f32.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{demote}}\mathsf{\_f64} ~|~
//   \mathsf{f64.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{promote}}\mathsf{\_f32} ~|~
//   \mathsf{f}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{convert}}\mathsf{\_i}\mathit{mm}\mathsf{\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} \\&amp;&amp;|&amp;
//   \mathsf{i}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{reinterpret}}\mathsf{\_f}\mathit{nn} ~|~
//   \mathsf{f}\mathit{nn}\mathsf{.}\href{../syntax/instructions.html#syntax-instr-numeric}{\mathsf{reinterpret}}\mathsf{\_i}\mathit{nn} \\&amp;&amp;|&amp;
//   \dots \\
// \def\mathdef2519#1{{}}\mathdef2519{integer unary operator} &amp; \href{../syntax/instructions.html#syntax-iunop}{\mathit{iunop}} &amp;::=&amp;
//   \mathsf{clz} ~|~
//   \mathsf{ctz} ~|~
//   \mathsf{popcnt} \\
// \def\mathdef2519#1{{}}\mathdef2519{integer binary operator} &amp; \href{../syntax/instructions.html#syntax-ibinop}{\mathit{ibinop}} &amp;::=&amp;
//   \mathsf{add} ~|~
//   \mathsf{sub} ~|~
//   \mathsf{mul} ~|~
//   \mathsf{div\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} ~|~
//   \mathsf{rem\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} \\&amp;&amp;|&amp;
//   \mathsf{and} ~|~
//   \mathsf{or} ~|~
//   \mathsf{xor} ~|~
//   \mathsf{shl} ~|~
//   \mathsf{shr\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} ~|~
//   \mathsf{rotl} ~|~
//   \mathsf{rotr} \\
// \def\mathdef2519#1{{}}\mathdef2519{floating-point unary operator} &amp; \href{../syntax/instructions.html#syntax-funop}{\mathit{funop}} &amp;::=&amp;
//   \mathsf{abs} ~|~
//   \mathsf{neg} ~|~
//   \mathsf{sqrt} ~|~
//   \mathsf{ceil} ~|~
//   \mathsf{floor} ~|~
//   \mathsf{trunc} ~|~
//   \mathsf{nearest} \\
// \def\mathdef2519#1{{}}\mathdef2519{floating-point binary operator} &amp; \href{../syntax/instructions.html#syntax-fbinop}{\mathit{fbinop}} &amp;::=&amp;
//   \mathsf{add} ~|~
//   \mathsf{sub} ~|~
//   \mathsf{mul} ~|~
//   \mathsf{div} ~|~
//   \mathsf{min} ~|~
//   \mathsf{max} ~|~
//   \mathsf{copysign} \\
// \def\mathdef2519#1{{}}\mathdef2519{integer test operator} &amp; \href{../syntax/instructions.html#syntax-itestop}{\mathit{itestop}} &amp;::=&amp;
//   \mathsf{eqz} \\
// \def\mathdef2519#1{{}}\mathdef2519{integer relational operator} &amp; \href{../syntax/instructions.html#syntax-irelop}{\mathit{irelop}} &amp;::=&amp;
//   \mathsf{eq} ~|~
//   \mathsf{ne} ~|~
//   \mathsf{lt\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} ~|~
//   \mathsf{gt\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} ~|~
//   \mathsf{le\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} ~|~
//   \mathsf{ge\_}\href{../syntax/instructions.html#syntax-sx}{\mathit{sx}} \\
// \def\mathdef2519#1{{}}\mathdef2519{floating-point relational operator} &amp; \href{../syntax/instructions.html#syntax-frelop}{\mathit{frelop}} &amp;::=&amp;
//   \mathsf{eq} ~|~
//   \mathsf{ne} ~|~
//   \mathsf{lt} ~|~
//   \mathsf{gt} ~|~
//   \mathsf{le} ~|~
//   \mathsf{ge} \\
// \end{array}\end{split}
// """