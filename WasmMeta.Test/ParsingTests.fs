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

type TokenizerTests(output: ITestOutputHelper) =
    [<Fact>]
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
              TexWord ":"
              TexWord ":"
              TexWord "="
              TexCommand(("I32", []), [])
              TexWord "|"
              TexCommand(("I64", []), [])
              TexWord "|"
              TexCommand(("F32", []), [])
              TexWord "|"
              TexCommand(("F64", []), [])
              TexCommand(("end", []), [ Arg "array" ]) ]
        )
