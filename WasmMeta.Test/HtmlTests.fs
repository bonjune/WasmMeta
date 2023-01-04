module WasmMeta.HtmlTests

open Xunit
open FSharp.Data

open WasmMeta.Extract


[<Fact>]
let ``Can extract value syntaxes from HTML`` () =
    let valueSyntaxes = extractValueSections ()
    Assert.NotEqual(valueSyntaxes.Length, 0)

