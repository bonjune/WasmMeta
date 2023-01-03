module WasmMeta.Tests

open System
open Xunit
open Xunit.Abstractions

open WasmMeta.Extract
open FParsec

type ParserTests(output: ITestOutputHelper) =
    let test (result: ParserResult<_, _>) =
        match result with
        | Success (result, state, pos) ->
            output.WriteLine (sprintf "%A" result)
            Assert.True(true)
        | Failure (msg, err, state) ->
            Assert.True(false, sprintf "%A" msg)

    let testWith expected (result: ParserResult<_, _>) =
        match result with
        | Success (result, _, _) -> Assert.StrictEqual(expected, result)
        | Failure (msg, _, _) -> Assert.True(false, sprintf "%A" msg)

    // Parsing symbols

    [<Fact; Trait("Category", "Symbol")>]
    member _.``Parse Terminal`` () =
        @"\href{../syntax/types.html#syntax-valtype}{\mathsf{f32}}"
        |> run pSymbol
        |> testWith (Term "f32")

    [<Fact; Trait("Category", "Symbol")>]
    member _.``Parse Non-terminal`` () =
        @"\href{../syntax/types.html#syntax-numtype}{\mathit{numtype}}"
        |> run pSymbol
        |> testWith (Nonterm "numtype")

    // Parsing productions

    [<Fact; Trait("Category", "Production")>]
    member _.``Parse Union Production`` () =
        @"\href{../syntax/types.html#syntax-valtype}{\mathsf{i32}}
        ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{i64}}
        ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{f32}}
        ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{f64}}"
        |> run pUnion
        |> testWith (Production.MkUnion [Term "i32"; Term "i64"; Term "f32"; Term "f64"])

    [<Fact; Trait("Category", "Production")>]
    member _.``Parse Tuple Production`` () =
        @"\href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}} \href{../syntax/types.html#syntax-functype}{\rightarrow} \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}}"
        |> run pTuple
        |> testWith (Production.MkTuple [Nonterm "resulttype"; Special @"\rightarrow"; Nonterm "resulttype"])

    [<Fact; Trait("Category", "Production")>]
    member _.``Parse Number Types Production`` () =
        """\def\mathdef2599#1{{}}\mathdef2599{number type} & \href{../syntax/types.html#syntax-numtype}{\mathit{numtype}} &::=&
        \href{../syntax/types.html#syntax-valtype}{\mathsf{i32}} ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{i64}} ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{f32}} ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{f64}}"""
        |> run pProd
        |> testWith (Nonterm "numtype", Production.MkUnion [Term "i32"; Term "i64"; Term "f32"; Term "f64"])

    [<Fact; Trait("Category", "Production")>]
    member _.``Parse Function Types Production`` () =
        """\def\mathdef2599#1{{}}\mathdef2599{function type} & \href{../syntax/types.html#syntax-functype}{\mathit{functype}} &::=&
        \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}}
        \href{../syntax/types.html#syntax-functype}{\rightarrow}
        \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}}"""
        |> run pProd
        |> testWith (Nonterm "functype", Production.MkTuple [Nonterm "resulttype"; Special @"\rightarrow"; Nonterm "resulttype"])

    // Parsing whole formula
    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Number Type`` () =
        """\begin{split}\begin{array}{llll}
        \def\mathdef2599#1{{}}\mathdef2599{number type} & \href{../syntax/types.html#syntax-numtype}{\mathit{numtype}} &::=&
        \href{../syntax/types.html#syntax-valtype}{\mathsf{i32}} ~|~
        \href{../syntax/types.html#syntax-valtype}{\mathsf{i64}} ~|~
        \href{../syntax/types.html#syntax-valtype}{\mathsf{f32}} ~|~
        \href{../syntax/types.html#syntax-valtype}{\mathsf{f64}} \\ \end{array}\end{split}"""
        |> run pFormula
        |> test

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Function Type`` () =
        """\begin{split}\begin{array}{llll}
        \def\mathdef2599#1{{}}\mathdef2599{function type} & \href{../syntax/types.html#syntax-functype}{\mathit{functype}} &::=&
        \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}}
        \href{../syntax/types.html#syntax-functype}{\rightarrow}
        \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}} \\ \end{array}\end{split}"""
        |> run pFormula
        |> test


    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Types`` () =
        extractTypeSections ()
        |> List.map extractFormula
        |> List.map (run pFormula)
        |> List.map test
