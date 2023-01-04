module WasmMeta.Tokenize

open FParsec

type Command = CommandHead * Argument list
/// Command Name and Optional Parameters
and CommandHead = string * string list
/// An argument can be a string or a command
and Argument =
    | Arg of string
    | Cmd of Command

let letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
let texSpecials = [ '#'; '$'; '%'; '^'; '&'; '_'; '{'; '}'; '~'; '\\' ]

// let startMath = pstring @"\["
// let endMath = pstring @"\]"

let openBracket = pchar '{'
let closeBracket = pchar '}'
let slash = pchar '\\'
let comma = pchar ','
let ch = anyOf letters
let word = many1Chars ch

// Parse Command Heads
let commandName = many1Chars2 slash ch
let optionalParams = skipChar '[' >>. sepBy1 word comma .>> skipChar ']'

let commandHead = commandName .>>. (attempt optionalParams <|> preturn [])

// Parse Command Arguments
// Referece: Creating a recursive parser in FParsec
// - https://hestia.typepad.com/flatlander/2011/07/recursive-parsers-in-fparsec.html
// - https://stackoverflow.com/questions/71328877/how-to-parse-recusrive-grammar-in-fparsec?noredirect=1&lq=1
let command, commandRef = createParserForwardedToRef ()
let strArg = many1Chars (anyOf (letters + "./-#")) |>> Arg
let cmdArg = command |>> Cmd
let argument: Parser<Argument, unit> = between openBracket closeBracket (attempt strArg <|> cmdArg)

commandRef.Value <- commandHead .>>. many argument

let special: Parser<_, unit> =
    texSpecials
    |> List.map pchar
    |> choice


