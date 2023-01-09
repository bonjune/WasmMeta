module WasmMeta.Tokenize

open FParsec

// Parse Tex Commands
// Reference: https://en.wikipedia.org/wiki/Help:Displaying_a_formula#LaTeX_basics
type Command = {
    Head: CommandHead
    Args: Argument list
}
with
    static member Create head = { Head = head; Args = [] }
    static member Create (head, args) = { Head = head; Args = args }
/// Command Name and Optional Parameters

and [<Struct>] CommandHead = {
    Name: string
    OptParams: string list
}

with
    static member Create name = { Name = name; OptParams = [] }
    static member Create (name, opts) = {Name = name; OptParams = opts }

/// An argument can be a string or a command
and Argument =
    | Arg of string
    | Cmd of Command

    member self.Name =
        match self with
        | Arg s -> s
        | Cmd cmd -> cmd.Head.Name

type TexToken =
    | TexCommand of Command
    | TexWord of string

let private letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let private openBracket = pchar '{'
let private closeBracket = pchar '}'
let private slash = pchar '\\'
let private comma = pchar ','
let private equal = pstring @"::="
let private openBracketLiteral = pstring @"\{"
let private closeBracketLiteral = pstring @"\}"
let private letter = anyOf letters
let private nonLetter = satisfy (isNoneOf letters)
let private letterWith cs = anyOf (letters + cs)
let private word = many1Chars letter

let private texSpaces =
    choice
        [ skipString @"\quad"
          skipString @"\qquad"
          skipString @"\ "
          skipString @"\,"
          skipString @"\:"
          skipString @"\;"
          skipString @"\!"
          skipString @"\\"
          skipChar '&'
          skipChar '~' ]
    .>> spaces

let private ws = spaces .>> many texSpaces

// Parse Command Heads
let private commandName = slash >>. many1Chars2 letter (letterWith "./-#")
let private optionalParams = skipChar '[' >>. sepBy1 word comma .>> skipChar ']'

let private commandHead = commandName .>>. (attempt optionalParams <|> preturn []) |>> CommandHead.Create

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
let private command, private commandRef = createParserForwardedToRef ()

let private strArg, private strArgRef = createParserForwardedToRef ()
let private cmdArg = command |>> Cmd

let private argument: Parser<Argument, unit> =
    between openBracket closeBracket (attempt cmdArg <|> (strArg |>> Arg))

strArgRef.Value <-
    attempt (between openBracket closeBracket strArg)
    <|> manyChars (letterWith "./-# ")

commandRef.Value <-
    commandHead
    .>>. many argument |>> Command.Create
    .>> ws




/// ```
/// <token> ::= <command>
///         |   <word>
/// ```
let private token =
    choice
        [ attempt command |>> TexCommand
          attempt openBracketLiteral |>> TexWord
          attempt closeBracketLiteral |>> TexWord
          attempt equal |>> TexWord
          attempt word |>> TexWord
          nonLetter |>> (string >> TexWord) ]
    .>> ws

type Tokenizer = Parser<TexToken list, unit>

let parseTex: Tokenizer = ws >>. many token
