module WasmMeta.ParsingTests


open Xunit
open Xunit.Abstractions

open WasmMeta.Tokenize
open FParsec

let test (result: ParserResult<_, _>) =
    match result with
    | Success(result, state, pos) -> Assert.True(true)
    | Failure(msg, err, state) -> Assert.True(false, sprintf "%A" msg)

let testWith expected (result: ParserResult<_, _>) =
    match result with
    | Success(result, _, _) -> Assert.StrictEqual(expected, result)
    | Failure(msg, _, _) -> Assert.True(false, sprintf "%A" msg)

type TokenizerTests(output: ITestOutputHelper) =
    [<Fact; Trait("Category", "Tokenize")>]
    let ``Parse Number Type Math Block`` () =
        @"   \begin{array}{llll}
        \production{number type} & \numtype &::=&
            \I32 ~|~ \I64 ~|~ \F32 ~|~ \F64 \\
        \end{array}"
        |> run parseTex
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "number type" ])
              TexCommand(("numtype", []), [])
              TexWord "::="
              TexCommand(("I32", []), [])
              TexWord "|"
              TexCommand(("I64", []), [])
              TexWord "|"
              TexCommand(("F32", []), [])
              TexWord "|"
              TexCommand(("F64", []), [])
              TexCommand(("end", []), [ Arg "array" ]) ]
        )

    [<Fact; Trait("Category", "Tokenize")>]
    let ``Parse Vecter Type Math Block`` () =
        @"   \begin{array}{llll}
        \production{vector type} & \vectype &::=&
            \V128 \\
        \end{array}"
        |> run parseTex
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "vector type" ])
              TexCommand(("vectype", []), [])
              TexWord "::="
              TexCommand(("V128", []), [])
              TexCommand(("end", []), [ Arg "array" ]) ]
        )

    [<Fact; Trait("Category", "Tokenize")>]
    let ``Parse Reference Type Math Block`` () =
        @"   \begin{array}{llll}
        \production{reference type} & \reftype &::=&
            \FUNCREF ~|~ \EXTERNREF \\
        \end{array}"
        |> run parseTex
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "reference type" ])
              TexCommand(("reftype", []), [])
              TexWord "::="
              TexCommand(("FUNCREF", []), [])
              TexWord "|"
              TexCommand(("EXTERNREF", []), [])
              TexCommand(("end", []), [ Arg "array" ]) ]
        )

    [<Fact; Trait("Category", "Tokenize")>]
    let ``Parse Value Type Math Block`` () =
        @"   \begin{array}{llll}
        \production{value type} & \valtype &::=&
            \numtype ~|~ \vectype ~|~ \reftype \\
        \end{array}"
        |> run parseTex
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "value type" ])
              TexCommand(("valtype", []), [])
              TexWord "::="
              TexCommand(("numtype", []), [])
              TexWord "|"
              TexCommand(("vectype", []), [])
              TexWord "|"
              TexCommand(("reftype", []), [])
              TexCommand(("end", []), [ Arg "array" ]) ]
        )


    [<Fact; Trait("Category", "Tokenize")>]
    let ``Parse Result Type Math Block`` () =
        @"   \begin{array}{llll}
        \production{result type} & \resulttype &::=&
            [\vec(\valtype)] \\
        \end{array}"
        |> run parseTex
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "result type" ])
              TexCommand(("resulttype", []), [])
              TexWord "::="
              TexWord "["
              TexCommand(("vec", []), [])
              TexWord "("
              TexCommand(("valtype", []), [])
              TexWord ")"
              TexWord "]"
              TexCommand(("end", []), [ Arg "array" ]) ]
        )

    [<Fact; Trait("Category", "Tokenize")>]
    let ``Parse Function Type Math Block`` () =
        @"   \begin{array}{llll}
        \production{function type} & \functype &::=&
            \resulttype \to \resulttype \\
        \end{array}"
        |> run parseTex
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "function type" ])
              TexCommand(("functype", []), [])
              TexWord "::="
              TexCommand(("resulttype", []), [])
              TexCommand(("to", []), [])
              TexCommand(("resulttype", []), [])
              TexCommand(("end", []), [ Arg "array" ]) ]
        )

    [<Fact; Trait("Category", "Tokenize")>]
    let ``Parse Limits Math Block`` () =
        @"   \begin{array}{llll}
        \production{limits} & \limits &::=&
            \{ \LMIN~\u32, \LMAX~\u32^? \} \\
        \end{array}"
        |> run parseTex
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "limits" ])
              TexCommand(("limits", []), [])
              TexWord "::="
              TexWord @"\{"
              TexCommand(("LMIN", []), [])
              TexCommand(("u32", []), [])
              TexWord ","
              TexCommand(("LMAX", []), [])
              TexCommand(("u32", []), [])
              TexWord "^"
              TexWord "?"
              TexWord @"\}"
              TexCommand(("end", []), [ Arg "array" ]) ]
        )

    [<Fact; Trait("Category", "Tokenize")>]
    let ``Parse Table Type Math Block`` () =
        @"   \begin{array}{llll}
        \production{table type} & \tabletype &::=&
            \limits~\reftype \\
        \end{array}"
        |> run parseTex
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "table type" ])
              TexCommand(("tabletype", []), [])
              TexWord "::="
              TexCommand(("limits", []), [])
              TexCommand(("reftype", []), [])
              TexCommand(("end", []), [ Arg "array" ]) ]
        )

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
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "external types" ])
              TexCommand(("externtype", []), [])
              TexWord "::="
              TexCommand(("ETFUNC", []), [])
              TexCommand(("functype", []), [])
              TexWord "|"
              TexCommand(("ETTABLE", []), [])
              TexCommand(("tabletype", []), [])
              TexWord "|"
              TexCommand(("ETMEM", []), [])
              TexCommand(("memtype", []), [])
              TexWord "|"
              TexCommand(("ETGLOBAL", []), [])
              TexCommand(("globaltype", []), [])
              TexCommand(("end", []), [ Arg "array" ]) ]
        )

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
        |> testWith (
            [ TexCommand(("begin", []), [ Arg "array"; Arg "llll" ])
              TexCommand(("production", []), [ Arg "global type" ])
              TexCommand(("globaltype", []), [])
              TexWord "::="
              TexCommand(("mut", []), [])
              TexCommand(("valtype", []), [])
              TexCommand(("production", []), [ Arg "mutability" ])
              TexCommand(("mut", []), [])
              TexWord "::="
              TexCommand(("MCONST", []), [])
              TexWord "|"
              TexCommand(("MVAR", []), [])
              TexCommand(("end", []), [ Arg "array" ]) ]
        )
