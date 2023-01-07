module WasmMeta.ParsingTests


open Xunit

open WasmMeta.Tokenize
open FParsec


let testWith expected (result: ParserResult<_, _>) =
    match result with
    | Success(result, _, _) -> Assert.StrictEqual(expected, result)
    | Failure(msg, _, _) -> Assert.True(false, sprintf "%A" msg)

[<Fact; Trait("Category", "Tokenize")>]
let ``Parse Number Type Math Block`` () =
    @"   \begin{array}{llll}
    \production{number type} & \numtype &::=&
        \I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\
    \end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "number type" ] }
          TexCommand
              { Head = { Name = "numtype"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexCommand
              { Head = { Name = "I32"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "I64"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "F32"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "F64"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]

[<Fact; Trait("Category", "Tokenize")>]
let ``Parse Vector Type Math Block`` () =
    @"   \begin{array}{llll}
    \production{vector type} & \vectype &::=&
        \V128 \\
    \end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "vector type" ] }
          TexCommand
              { Head = { Name = "vectype"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexCommand
              { Head = { Name = "V128"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]

[<Fact; Trait("Category", "Tokenize")>]
let ``Parse Reference Type Math Block`` () =
    @"   \begin{array}{llll}
    \production{reference type} & \reftype &::=&
        \FUNCREF ~|~ \EXTERNREF \\
    \end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "reference type" ] }
          TexCommand
              { Head = { Name = "reftype"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexCommand
              { Head = { Name = "FUNCREF"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "EXTERNREF"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]

[<Fact; Trait("Category", "Tokenize")>]
let ``Parse Value Type Math Block`` () =
    @"   \begin{array}{llll}
    \production{value type} & \valtype &::=&
        \numtype ~|~ \vectype ~|~ \reftype \\
    \end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "value type" ] }
          TexCommand
              { Head = { Name = "valtype"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexCommand
              { Head = { Name = "numtype"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "vectype"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "reftype"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]


[<Fact; Trait("Category", "Tokenize")>]
let ``Parse Result Type Math Block`` () =
    @"   \begin{array}{llll}
    \production{result type} & \resulttype &::=&
        [\vec(\valtype)] \\
    \end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "result type" ] }
          TexCommand
              { Head = { Name = "resulttype"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexWord "["
          TexCommand
              { Head = { Name = "vec"; OptParams = [] }
                Args = [] }
          TexWord "("
          TexCommand
              { Head = { Name = "valtype"; OptParams = [] }
                Args = [] }
          TexWord ")"
          TexWord "]"
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]

[<Fact; Trait("Category", "Tokenize")>]
let ``Parse Function Type Math Block`` () =
    @"   \begin{array}{llll}
    \production{function type} & \functype &::=&
        \resulttype \to \resulttype \\
    \end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "function type" ] }
          TexCommand
              { Head = { Name = "functype"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexCommand
              { Head = { Name = "resulttype"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "to"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "resulttype"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]

[<Fact; Trait("Category", "Tokenize")>]
let ``Parse Limits Math Block`` () =
    @"   \begin{array}{llll}
    \production{limits} & \limits &::=&
        \{ \LMIN~\u32, \LMAX~\u32^? \} \\
    \end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "limits" ] }
          TexCommand
              { Head = { Name = "limits"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexWord @"\{"
          TexCommand
              { Head = { Name = "LMIN"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "u32"; OptParams = [] }
                Args = [] }
          TexWord ","
          TexCommand
              { Head = { Name = "LMAX"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "u32"; OptParams = [] }
                Args = [] }
          TexWord "^"
          TexWord "?"
          TexWord @"\}"
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]

[<Fact; Trait("Category", "Tokenize")>]
let ``Parse Table Type Math Block`` () =
    @"   \begin{array}{llll}
    \production{table type} & \tabletype &::=&
        \limits~\reftype \\
    \end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "table type" ] }
          TexCommand
              { Head = { Name = "tabletype"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexCommand
              { Head = { Name = "limits"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "reftype"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]

[<Fact; Trait("Category", "Tokenize")>]
let ``Parse External Types Math Block`` () =
    @"   \begin{array}{llll}
\production{external types} & \externtype &::=&
 \ETFUNC~\functype ~|~
 \ETTABLE~\tabletype ~|~
 \ETMEM~\memtype ~|~
 \ETGLOBAL~\globaltype \\
\end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "external types" ] }
          TexCommand
              { Head = { Name = "externtype"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexCommand
              { Head = { Name = "ETFUNC"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "functype"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "ETTABLE"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "tabletype"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "ETMEM"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "memtype"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "ETGLOBAL"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "globaltype"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]

[<Fact; Trait("Category", "Tokenize")>]
let ``Parse Global Type Math Block`` () =
    @"   \begin{array}{llll}
\production{global type} & \globaltype &::=&
 \mut~\valtype \\
\production{mutability} & \mut &::=&
 \MCONST ~|~
 \MVAR \\
\end{array}"
    |> run parseTex
    |> testWith
        [ TexCommand
              { Head = { Name = "begin"; OptParams = [] }
                Args = [ Arg "array"; Arg "llll" ] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "global type" ] }
          TexCommand
              { Head = { Name = "globaltype"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexCommand
              { Head = { Name = "mut"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "valtype"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "production"; OptParams = [] }
                Args = [ Arg "mutability" ] }
          TexCommand
              { Head = { Name = "mut"; OptParams = [] }
                Args = [] }
          TexWord "::="
          TexCommand
              { Head = { Name = "MCONST"; OptParams = [] }
                Args = [] }
          TexWord "|"
          TexCommand
              { Head = { Name = "MVAR"; OptParams = [] }
                Args = [] }
          TexCommand
              { Head = { Name = "end"; OptParams = [] }
                Args = [ Arg "array" ] } ]
