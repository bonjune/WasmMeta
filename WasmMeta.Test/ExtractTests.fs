module WasmMeta.ExtractTests

open Xunit

open WasmMeta.Tokenize
open WasmMeta.Extract
open FParsec

let getTokens result =
    match result with
    | Success(r, _, _) -> r

[<Fact; Trait("Category", "Extract")>]
let ``Extract Global Type`` () =
    let tokens =
        @"   \begin{array}{llll}
        \production{global type} & \globaltype &::=&
        \mut~\valtype \\
        \production{mutability} & \mut &::=&
        \MCONST ~|~
        \MVAR \\
        \end{array}"
        |> run parseTex
        |> getTokens

    let e = Extractor(tokens)
    let rules = e.Run()

    Assert.Equal(
        { Name = "global type"
          Lhs = [ STerm "globaltype" ]
          Rhs = [ [ SNonterm "mut"; SNonterm "valtype" ] ] },
        rules.[0]
    )

    Assert.Equal(

        { Name = "mutability"
          Lhs = [ STerm "mut" ]
          Rhs = [ [ STerm "MCONST" ]; [ STerm "MVAR" ] ] },
        rules.[1]
    )
