module WasmMeta.Tokenize

open FParsec

// Parse Tex Commands
// Reference: https://en.wikipedia.org/wiki/Help:Displaying_a_formula#LaTeX_basics
type Command = CommandHead * Argument list
/// Command Name and Optional Parameters
and CommandHead = string * string list

/// An argument can be a string or a command
and Argument =
    | Arg of string
    | Cmd of Command

let letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let private openBracket = pchar '{'
let private closeBracket = pchar '}'
let private slash = pchar '\\'
let private comma = pchar ','
let private letter = anyOf letters
let private letterWith cs = anyOf (letters + cs)
let private word = many1Chars letter
let private texSpaces =
    choice [
        skipString @"\quad"
        skipString @"\qquad"
        skipString @"\ "
        skipString @"\,"
        skipString @"\:"
        skipString @"\;"
        skipString @"\!"
        skipChar   '~'
    ]
    .>> spaces
let private ws = spaces .>> many texSpaces

// Parse Command Heads
let private commandName = slash >>. many1Chars2 letter (letterWith "./-#")
let private optionalParams = skipChar '[' >>. sepBy1 word comma .>> skipChar ']'

let private commandHead = commandName .>>. (attempt optionalParams <|> preturn [])

/// ```
/// <command>  ::= <commandHead> <argument>*
/// 
/// <commandHead> ::= <commandName> <optionalParams>?
/// 
/// <commandName> ::= \<letter> <letter>*
/// 
/// <optionalParams> ::= [ <word> , ... , <word> ]
/// 
/// <argument\ ::= { <command> }
///            |     <string>
///
/// <string>   ::=   <letter>*
///            |   { <string> }
/// ```
/// Referece: Creating a recursive parser in FParsec
/// - https://hestia.typepad.com/flatlander/2011/07/recursive-parsers-in-fparsec.html
/// - https://stackoverflow.com/questions/71328877/how-to-parse-recusrive-grammar-in-fparsec?noredirect=1&lq=1
let command, private commandRef = createParserForwardedToRef ()

let strArg, private strArgRef = createParserForwardedToRef ()
let private cmdArg = command |>> Cmd

let private argument: Parser<Argument, unit> =
    between openBracket closeBracket (attempt cmdArg <|> (strArg |>> Arg))

strArgRef.Value <-
    attempt (between openBracket closeBracket strArg)
    <|> manyChars (letterWith "./-# ")
commandRef.Value <- commandHead .>>. many argument .>> ws

type SingleCommand = string

// TODO: Define non-letter character parser
let nonLetter = anyChar
let singleCommand = slash >>. anyChar

// Parse special characters
type Special = char

let private texSpecials =
    List.map pchar [ '#'; '$'; '%'; '^'; '&'; '_'; '{'; '}'; '~'; '\\' ]

let special: Parser<Special, unit> = choice texSpecials

type TexToken =
    | TexCommand of Command
    | TexWord of string

/// ```
/// <token> ::= <command>
///         |   <word>
/// ```
let token =
    choice [
        attempt command |>> TexCommand
        attempt word |>> TexWord
        nonLetter |>> (string >> TexWord)
    ]
    .>> ws


let parseTex = many token
