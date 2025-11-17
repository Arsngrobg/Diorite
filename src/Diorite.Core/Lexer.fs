// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Lexer.fs
// Summary: Implementation details for Lexer.fsi
// Author:  Arsngrobg, Borngle
// Version: v1.7
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

module Lexer =
    // function signature for a function that transforms type 'a to type 'b
    type Transformer<'a, 'b> = 'a -> 'b

    // function signature for a predicate that the consumer uses to determine whether a character should be consumed
    type ConsumerPredicate = char -> bool

    // transformers
    [<RequireQualifiedAccess>]
    module Transformers =
        // converts a string into its individual characters
        let stringToChars: Transformer<string, char list> = Seq.toList

        // converts the individual character into its integer representation
        let parseDigit: Transformer<char, int> = (fun c -> int c - int '0')

        // converts the characters into a string
        let charsToString: Transformer<char list, string> = (Array.ofList >> System.String)

        // converts the characters into its numerical representation
        let parseNumber: Transformer<char list, float> = (charsToString >> System.Double.Parse)

    // predicates
    [<RequireQualifiedAccess>]
    module Predicates =
        let isLetter: ConsumerPredicate = System.Char.IsLetter

        let isDigit: ConsumerPredicate = System.Char.IsDigit

        let isBlank: ConsumerPredicate = System.Char.IsWhiteSpace

        let notNewline: ConsumerPredicate = (fun c -> c <> '\n')

        let isNewline: ConsumerPredicate = (fun c -> c = '\n')

        let nonBlank: ConsumerPredicate = (isBlank >> not)

    // Implementation
    type TokenType =
        | IllegalToken
        | Number   of float
        | Variable of VariableType
        | Symbol
        | Undefined
        | Infinity
        | Pi
        | Tau
        | Euler
        | Colon
        | Arrow
        | Comma
        | Equals
        | LessThan
        | GreaterThan
        | LessThanOrEqual
        | GreaterThanOrEqual
        | NotEqual
        | Hat
        | Exclamation
        | Asterisk
        | ForwardSlash
        | DoubleForwardSlash
        | Percentage
        | Plus
        | Hyphen
        | If
        | Otherwise
        | LeftParenthesis
        | RightParenthesis
        | LeftBracket
        | RightBracket
        | LeftBrace
        | RightBrace
        | Bar
        | SemiColon

    // Implementation
    [<Struct>]
    type Token = {
        lexeme: string
        id:     TokenType
        line:   int
        column: int
    }

    // Implementation
    type TokenStream = Token list

    // Implementation
    let strToken (token: Token): string =
        $"Token['{token.lexeme}', {token.id}, [{token.line}:{token.column}]]"

    // Implementation
    let rec strTokens (tokens: TokenStream): string =
        match tokens with
         | [] | [{id = TokenType.SemiColon}]  -> ""
         | {id = TokenType.SemiColon} :: tail -> $"\n{strTokens tail}"
         | t                          :: tail -> $"({strToken t}) {strTokens tail}"

    // Implementation
    let rec getError (tokens: TokenStream): DioriteError option =
        let getErrorMsg (illegal: Token): string =
            $"Unexpected token: '{illegal.lexeme}' at [{illegal.line}:{illegal.column}]"

        match tokens with
         | []                                                 -> None
         | head :: _    when head.id = TokenType.IllegalToken -> (getErrorMsg >> DioriteError.SyntaxError >> Some) head
         | _    :: tail                                       -> getError tail

    // Implementation
    let tokenize (source: string): TokenStream =
        // recursively consume character given that they satisfy the given predicate
        // returns the consumed characters and the remaining characters
        let rec consume (predicate: ConsumerPredicate) (src: char list): char list * char list =
            match src with
             | c :: tail when predicate c ->
                let (consumed: char list), (remaining: char list) = consume predicate tail
                (c :: consumed, remaining)
             | _                          -> ([], src)

        let rec scan (src: char list) (line: int) (column: int): TokenStream =
            match src with
             // empty string
             | [] -> []

             // numbers
             | c :: _ when Predicates.isDigit c ->
                 let (integerComponent: char list), (numberTail: char list) = consume Predicates.isDigit src
                 match numberTail with
                  // integer component + decimal component
                  | '.' :: c :: tail when Predicates.isDigit c ->
                      let (decimalComponent: char list), (remaining: char list) = consume Predicates.isDigit (c :: tail)
                      let raw: char list = integerComponent @ ['.'] @ decimalComponent
                      let head: Token = {
                          lexeme = raw |> Transformers.charsToString
                          id     = raw |> (Transformers.parseNumber >> TokenType.Number)
                          line   = line
                          column = column
                      }
                      let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                      head :: tail
                  // integer component + '.' (but no decimal component)
                  | '.' :: remaining ->
                      let head: Token = {
                          lexeme = integerComponent |> Transformers.charsToString
                          id     = integerComponent |> (Transformers.parseNumber >> TokenType.Number)
                          line   = line
                          column = column
                      }
                      let illegal: Token = {
                          lexeme = "."
                          id     = TokenType.IllegalToken
                          line   = line
                          column = column + head.lexeme.Length
                      }
                      let tail: TokenStream = scan remaining line (illegal.column + 1)
                      head :: illegal :: tail
                  // integer component
                  | remaining ->
                      let head: Token = {
                          lexeme = integerComponent |> Transformers.charsToString
                          id     = integerComponent |> (Transformers.parseNumber >> TokenType.Number)
                          line   = line
                          column = column
                      }
                      let tail: TokenStream = scan remaining line (column + head.lexeme.Length - 1)
                      head :: tail

             // variables, keywords, and constants
             | c :: _ when Predicates.isLetter c ->
                 match consume Predicates.isLetter src with
                  // variable (+ optional subscript)
                  | [character], variableTail ->
                      match variableTail with
                       // with subscript
                       | subscript :: remaining when Predicates.isDigit subscript ->
                           let encodedSubscript: int = subscript |> (Transformers.parseDigit >> (fun s -> s + 1))
                           let head: Token = {
                               lexeme = $"{character}{encodedSubscript}"
                               id     = (character, encodedSubscript) |> TokenType.Variable
                               line   = line
                               column = column
                           }
                           let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                           head :: tail
                       // no subscript
                       | remaining ->
                           let head: Token = {
                               lexeme = $"{character}"
                               id     = (character, 0) |> TokenType.Variable
                               line   = line
                               column = column
                           }
                           let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                           head :: tail

                  // keywords & constants
                  | chars, remaining ->
                      let word: string = chars |> Transformers.charsToString
                      let id: TokenType = match word with
                                           | "if"               -> TokenType.If
                                           | "otherwise"        -> TokenType.Otherwise
                                           | "undefined"        -> TokenType.Undefined
                                           | "infinity" | "inf" -> TokenType.Infinity
                                           | "pi"               -> TokenType.Pi
                                           | "tau"              -> TokenType.Tau
                                           | "euler"            -> TokenType.Euler
                                           | _                  -> TokenType.Symbol
                      let head: Token = {
                          lexeme = word
                          id     = id
                          line   = line
                          column = column
                      }
                      let tail: TokenStream = scan remaining line (column + word.Length)
                      head :: tail

             // args & params
             | ',' :: remaining ->
                 let head: Token       = {lexeme=","; id=TokenType.Comma; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // set notation
             | '-' :: '>' :: remaining ->
                 let head: Token       = {lexeme="->"; id=TokenType.Arrow; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | ':'        :: remaining ->
                 let head: Token       = {lexeme=":"; id=TokenType.Colon; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // comparison operators
             | '<' :: '=' :: remaining ->
                 let head: Token       = {lexeme="<="; id=TokenType.LessThan; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | '>' :: '=' :: remaining ->
                 let head: Token       = {lexeme=">="; id=TokenType.LessThanOrEqual; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | '!' :: '=' :: remaining ->
                 let head: Token       = {lexeme="!="; id=TokenType.NotEqual; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | '='        :: remaining ->
                 let head: Token       = {lexeme="="; id=TokenType.Equals; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '<'        :: remaining ->
                 let head: Token       = {lexeme="<"; id=TokenType.LessThan; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '>'        :: remaining ->
                 let head: Token       = {lexeme="<"; id=TokenType.GreaterThan; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // arithmetic operators
             | '^'        :: remaining ->
                 let head: Token       = {lexeme="^"; id=TokenType.Hat; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '!'        :: remaining ->
                 let head: Token       = {lexeme="!"; id=TokenType.Exclamation; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '*'        :: remaining ->
                 let head: Token       = {lexeme="*"; id=TokenType.Asterisk; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '/' :: '/' :: remaining ->
                 let head: Token       = {lexeme="//"; id=TokenType.DoubleForwardSlash; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | '/'        :: remaining ->
                 let head: Token       = {lexeme="/"; id=TokenType.ForwardSlash; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '%'        :: remaining ->
                 let head: Token       = {lexeme="%"; id=TokenType.Percentage; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '+'        :: remaining ->
                 let head: Token       = {lexeme="+"; id=TokenType.Plus; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '-'        :: remaining ->
                 let head: Token       = {lexeme="-"; id=TokenType.Hyphen; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '|'        :: remaining ->
                 let head: Token       = {lexeme="|"; id=TokenType.Bar; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // brackets, curly braces & square brackets
             | '('        :: remaining ->
                 let head: Token       = {lexeme="("; id=TokenType.LeftParenthesis; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | ')'        :: remaining ->
                 let head: Token       = {lexeme=")"; id=TokenType.RightParenthesis; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '{'        :: remaining ->
                 let head: Token       = {lexeme="{"; id=TokenType.LeftBrace; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '}'        :: remaining ->
                 let head: Token       = {lexeme="}"; id=TokenType.RightBrace; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '['        :: remaining ->
                 let head: Token       = {lexeme="["; id=TokenType.LeftBracket; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | ']'        :: remaining ->
                 let head: Token       = {lexeme="]"; id=TokenType.RightBracket; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // end of statement
             | ';' :: remaining ->
                 let head: Token       = {lexeme=";"; id=TokenType.SemiColon; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // comment (ignores everything until newline)
             | '#' :: _ ->
                 let _, (remaining: char list) = consume Predicates.notNewline src
                 scan remaining line (column + remaining.Length)

             // newline (increment line)
             | '\n' :: remaining ->
                 scan remaining (line + 1) 0

             // skip whitespace
             | c :: _ when Predicates.isBlank c ->
                 let (consumed: char list), (remaining: char list) = consume Predicates.isBlank src
                 scan remaining line (column + consumed.Length)

             // illegal tokens
             | _ ->
                 let (lexeme: char list), (remaining: char list) = consume Predicates.nonBlank src
                 let head: Token = {
                     lexeme = Transformers.charsToString lexeme
                     id     = TokenType.IllegalToken
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                 head :: tail

        let chars = source |> Transformers.stringToChars
        (scan chars) 0 0

    [<EntryPoint>]
    let main (_: string array): int =
        let repr: string = "f(x) = 2*x; # this is a comment\n" |> (tokenize >> strTokens)
        printf $"{repr}\n"
        0
