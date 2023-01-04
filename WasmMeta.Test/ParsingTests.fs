module WasmMeta.ParsingTests

open System
open Xunit
open Xunit.Abstractions

open WasmMeta.Tokenize
open WasmMeta.Extract
open FParsec

let test (result: ParserResult<_, _>) =
    match result with
    | Success(result, state, pos) -> Assert.True(true)
    | Failure(msg, err, state) -> Assert.True(false, sprintf "%A" msg)

let testWith expected (result: ParserResult<_, _>) =
    match result with
    | Success(result, _, _) -> Assert.StrictEqual(expected, result)
    | Failure(msg, _, _) -> Assert.True(false, sprintf "%A" msg)

type TypeSectionTests(output: ITestOutputHelper) =

    // Parsing symbols
    [<Fact; Trait("Category", "Symbol")>]
    member _.``Parse Terminal f32``() =
        @"\href{../syntax/types.html#syntax-valtype}{\mathsf{f32}}"
        |> run Symbol.pSymbol
        |> testWith (Term "f32")

    [<Fact; Trait("Category", "Symbol")>]
    member _.``Parse Non-terminal numtype``() =
        @"\href{../syntax/types.html#syntax-numtype}{\mathit{numtype}}"
        |> run Symbol.pSymbol
        |> testWith (Nonterm "numtype")

    [<Fact; Trait("Category", "Symbol")>]
    member _.``Parse Named Symbol``() =
        @"\href{../syntax/types.html#syntax-externtype}{\mathsf{func}}~\href{../syntax/types.html#syntax-functype}{\mathit{functype}}"
        |> run Symbol.pSymbolNamed
        |> testWith (Named("func", Nonterm "functype"))

    // Parsing productions

    [<Fact; Trait("Category", "Production")>]
    member _.``Parse Union Production``() =
        @"\href{../syntax/types.html#syntax-valtype}{\mathsf{i32}}
        ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{i64}}
        ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{f32}}
        ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{f64}}"
        |> run Production.pUnion
        |> testWith (Union [ Term "i32"; Term "i64"; Term "f32"; Term "f64" ])

    [<Fact; Trait("Category", "Production")>]
    member _.``Parse Record``() =
        """\{ \href{../syntax/types.html#syntax-limits}{\mathsf{min}}~\href{../syntax/values.html#syntax-int}{\mathit{u32}}, \href{../syntax/types.html#syntax-limits}{\mathsf{max}}~\href{../syntax/values.html#syntax-int}{\mathit{u32}}^? \}"""
        |> run Production.pRecord
        |> testWith (Record(Map [ Term "min", Nonterm "u32"; Term "max", Optional <| Nonterm "u32" ]))

    [<Fact; Trait("Category", "Production")>]
    member _.``Parse Tuple Production``() =
        @"\href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}} \href{../syntax/types.html#syntax-functype}{\rightarrow} \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}}"
        |> run Production.pTuple
        |> testWith (Tuple [ Nonterm "resulttype"; Special @"\rightarrow"; Nonterm "resulttype" ])

    [<Fact; Trait("Category", "Production")>]
    member _.``Parse Number Types Production``() =
        """\def\mathdef2599#1{{}}\mathdef2599{number type} & \href{../syntax/types.html#syntax-numtype}{\mathit{numtype}} &::=&
        \href{../syntax/types.html#syntax-valtype}{\mathsf{i32}} ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{i64}} ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{f32}} ~|~ \href{../syntax/types.html#syntax-valtype}{\mathsf{f64}}"""
        |> run pProd
        |> testWith (Nonterm "numtype", Union [ Term "i32"; Term "i64"; Term "f32"; Term "f64" ])

    [<Fact; Trait("Category", "Production")>]
    member _.``Parse Function Types Production``() =
        """\def\mathdef2599#1{{}}\mathdef2599{function type} & \href{../syntax/types.html#syntax-functype}{\mathit{functype}} &::=&
        \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}}
        \href{../syntax/types.html#syntax-functype}{\rightarrow}
        \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}}"""
        |> run pProd
        |> testWith (Nonterm "functype", Tuple [ Nonterm "resulttype"; Special @"\rightarrow"; Nonterm "resulttype" ])

    // Parsing whole formula
    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Number Type``() =
        """\[\begin{split}\begin{array}{llll}
        \def\mathdef2599#1{{}}\mathdef2599{number type} & \href{../syntax/types.html#syntax-numtype}{\mathit{numtype}} &::=&
        \href{../syntax/types.html#syntax-valtype}{\mathsf{i32}} ~|~
        \href{../syntax/types.html#syntax-valtype}{\mathsf{i64}} ~|~
        \href{../syntax/types.html#syntax-valtype}{\mathsf{f32}} ~|~
        \href{../syntax/types.html#syntax-valtype}{\mathsf{f64}} \\ \end{array}\end{split}\]"""
        |> run pFormula
        |> testWith [ (Nonterm "numtype", Union [ Term "i32"; Term "i64"; Term "f32"; Term "f64" ]) ]

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Function Type``() =
        """\[\begin{split}\begin{array}{llll}
        \def\mathdef2599#1{{}}\mathdef2599{function type} & \href{../syntax/types.html#syntax-functype}{\mathit{functype}} &::=&
        \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}} \href{../syntax/types.html#syntax-functype}{\rightarrow} \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}} \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> testWith
            [ (Nonterm "functype", Tuple [ Nonterm "resulttype"; Special @"\rightarrow"; Nonterm "resulttype" ]) ]

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Reference Type``() =
        """\[\begin{split}\begin{array}{llll}
        \def\mathdef2599#1{{}}\mathdef2599{reference type} & \href{../syntax/types.html#syntax-reftype}{\mathit{reftype}} &::=&
        \href{../syntax/types.html#syntax-reftype}{\mathsf{funcref}} ~|~ \href{../syntax/types.html#syntax-reftype}{\mathsf{externref}} \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> testWith [ (Nonterm "reftype", Union [ Term "funcref"; Term "externref" ]) ]

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Value Type``() =
        """\[\begin{split}\begin{array}{llll}
        \def\mathdef2599#1{{}}\mathdef2599{value type} & \href{../syntax/types.html#syntax-valtype}{\mathit{valtype}} &::=&
        \href{../syntax/types.html#syntax-numtype}{\mathit{numtype}} ~|~ \href{../syntax/types.html#syntax-vectype}{\mathit{vectype}} ~|~ \href{../syntax/types.html#syntax-reftype}{\mathit{reftype}} \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> testWith [ (Nonterm "valtype", Union [ Nonterm "numtype"; Nonterm "vectype"; Nonterm "reftype" ]) ]

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Result Type``() =
        """\[\begin{split}\begin{array}{llll}
        \def\mathdef2599#1{{}}\mathdef2599{result type} & \href{../syntax/types.html#syntax-resulttype}{\mathit{resulttype}} &::=&
        [\href{../syntax/conventions.html#syntax-vec}{\mathit{vec}}(\href{../syntax/types.html#syntax-valtype}{\mathit{valtype}})] \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> testWith [ (Nonterm "resulttype", Vec(Nonterm "valtype")) ]

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Limits``() =
        """\[\begin{split}\begin{array}{llll}
        \def\mathdef2599#1{{}}\mathdef2599{limits} & \href{../syntax/types.html#syntax-limits}{\mathit{limits}} &::=&
        \{ \href{../syntax/types.html#syntax-limits}{\mathsf{min}}~\href{../syntax/values.html#syntax-int}{\mathit{u32}}, \href{../syntax/types.html#syntax-limits}{\mathsf{max}}~\href{../syntax/values.html#syntax-int}{\mathit{u32}}^? \} \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> testWith
            [ (Nonterm "limits", Record(Map [ Term "min", Nonterm "u32"; Term "max", Optional <| Nonterm "u32" ])) ]

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Table Type``() =
        """\[\begin{split}\begin{array}{llll}
        \def\mathdef2599#1{{}}\mathdef2599{table type} & \href{../syntax/types.html#syntax-tabletype}{\mathit{tabletype}} &::=&
        \href{../syntax/types.html#syntax-limits}{\mathit{limits}}~\href{../syntax/types.html#syntax-reftype}{\mathit{reftype}} \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> testWith [ (Nonterm "tabletype", Tuple [ Nonterm "limits"; Nonterm "reftype" ]) ]

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Types``() =
        extractTypeSections ()
        |> List.map extractFormula
        |> List.map (run pFormula)
        |> List.map test

type ValueSectionsTests(output: ITestOutputHelper) =
    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Names``() =
        """\[\begin{split}\begin{array}{llclll}
        \def\mathdef2638#1{{}}\mathdef2638{name} & \href{../syntax/values.html#syntax-name}{\mathit{name}} &::=&
        \href{../syntax/values.html#syntax-name}{\mathit{char}}^\ast \qquad\qquad (\mathrel{\mbox{if}} |\href{../binary/values.html#binary-utf8}{\mathrm{utf8}}(\href{../syntax/values.html#syntax-name}{\mathit{char}}^\ast)| &lt; 2^{32}) \\
        \def\mathdef2638#1{{}}\mathdef2638{character} & \href{../syntax/values.html#syntax-name}{\mathit{char}} &::=&
        \def\mathdef2679#1{\mathrm{U{+}#1}}\mathdef2679{00} ~|~ \dots ~|~ \def\mathdef2680#1{\mathrm{U{+}#1}}\mathdef2680{D7FF} ~|~
        \def\mathdef2681#1{\mathrm{U{+}#1}}\mathdef2681{E000} ~|~ \dots ~|~ \def\mathdef2682#1{\mathrm{U{+}#1}}\mathdef2682{10FFFF} \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> test

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Bytes``() =
        """\[\begin{split}\begin{array}{llll}
        \def\mathdef2638#1{{}}\mathdef2638{byte} & \href{../syntax/values.html#syntax-byte}{\mathit{byte}} &::=&
        \def\mathdef2677#1{\mathtt{0x#1}}\mathdef2677{00} ~|~ \dots ~|~ \def\mathdef2678#1{\mathtt{0x#1}}\mathdef2678{FF} \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> test

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Integers``() =
        """\[\begin{split}\begin{array}{llll}
        \def\mathdef2638#1{{}}\mathdef2638{unsigned integer} & \href{../syntax/values.html#syntax-int}{\mathit{u}N} &::=&
        0 ~|~ 1 ~|~ \dots ~|~ 2^N{-}1 \\
        \def\mathdef2638#1{{}}\mathdef2638{signed integer} & \href{../syntax/values.html#syntax-int}{\mathit{s}N} &::=&
        -2^{N-1} ~|~ \dots ~|~ {-}1 ~|~ 0 ~|~ 1 ~|~ \dots ~|~ 2^{N-1}{-}1 \\
        \def\mathdef2638#1{{}}\mathdef2638{uninterpreted integer} & \href{../syntax/values.html#syntax-int}{\mathit{i}N} &::=&
        \href{../syntax/values.html#syntax-int}{\mathit{u}N} \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> test

    [<Fact; Trait("Category", "Formula")>]
    member _.``Parse Floating-Point``() =
        """\[\begin{split}\begin{array}{llcll}
        \def\mathdef2638#1{{}}\mathdef2638{floating-point value} & \href{../syntax/values.html#syntax-float}{\mathit{f}N} &::=&
        {+} \href{../syntax/values.html#syntax-float}{\mathit{f}\mathit{Nmag}} ~|~ {-} \href{../syntax/values.html#syntax-float}{\mathit{f}\mathit{Nmag}} \\
        \def\mathdef2638#1{{}}\mathdef2638{floating-point magnitude} & \href{../syntax/values.html#syntax-float}{\mathit{f}\mathit{Nmag}} &::=&
        (1 + \href{../syntax/values.html#syntax-int}{\mathit{u}M}\cdot 2^{-M}) \cdot 2^e & (\mathrel{\mbox{if}} -2^{E-1}+2 \leq e \leq 2^{E-1}-1) \\ &&|&
        (0 + \href{../syntax/values.html#syntax-int}{\mathit{u}M}\cdot 2^{-M}) \cdot 2^e & (\mathrel{\mbox{if}} e = -2^{E-1}+2) \\ &&|&
        \infty \\ &&|&
        \href{../syntax/values.html#syntax-float}{\mathsf{nan}}(n) & (\mathrel{\mbox{if}} 1 \leq n &lt; 2^M) \\
        \end{array}\end{split}\]"""
        |> run pFormula
        |> test

type TokenizerTests(output: ITestOutputHelper) =
    [<Fact>]
    member _.``Parse Simple Tex Command``() =
        @"\mathsf{f32}" |> run command |> testWith ((@"mathsf", []), [ Arg "f32" ])

    [<Fact>]
    member _.``Parse Nested Tex Command 1``() =
        @"\href{../syntax/types.html#syntax-valtype}{\mathsf{f32}}"
        |> run command
        |> testWith (
            (@"href", []),
            [ Arg "../syntax/types.html#syntax-valtype"
              Cmd((@"mathsf", []), [ Arg "f32" ]) ]
        )

        @"\href{../syntax/types.html#syntax-numtype}{\mathit{numtype}}"
        |> run command
        |> testWith (
            (@"href", []),
            [ Arg "../syntax/types.html#syntax-numtype"
              Cmd((@"mathit", []), [ Arg "numtype" ]) ]
        )

    // TODO: add token for `^?`
    [<Fact>]
    member _.``Parse Nested Tex Command 2``() =
        @"\href{../syntax/values.html#syntax-int}{\mathit{u32}}^?"
        |> run command
        |> testWith (
            (@"href", []),
            [ Arg "../syntax/values.html#syntax-int"
              Cmd((@"mathit", []), [ Arg "u32" ]) ]
        )

    [<Fact>]
    member _.``Parse Nested Tex Command 3``() =
        """\def\mathdef2599#1{{}}\mathdef2599{value type}"""
        |> run (many command)
        |> testWith ( [
            (("def", []), [])
            (("mathdef2599#1", []), [Arg ""])
            (("mathdef2599", []), [Arg "value type"])
        ] )
