// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Lexer.fs
// Summary: The tokenising logic for the Diorite language
// Author:  Arsngrobg, Borngle
// Version: v1.9
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The <c>Lexer</c> module groups up related logic for tokenising <b>Diorite</b> source code into a sequential
///        stream of <c>Token</c>s. It is the first stage of language processing, and offers many functions for
///        obtaining descriptive error messages from failed tokenisation.
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
module Lexer =
    /// <summary>
    ///     <p>A <c>TokenStream</c> is a sequence of ordered <c>Token</c>s.</p>
    ///     <p>As of now, it is the type definition for a <c>Token list</c>.</p>
    /// </summary>
    type TokenStream = Token list

    /// <summary>
    ///     <p>Reads the current statement, as defined by the head of the <c>TokenStream</c>.</p>
    ///     <p>A statement, in the context of a <c>TokenStream</c>, is any number of tokens until a <c>SemiColon</c>
    ///        token is found.
    ///     </p>
    ///     <p><i>Base Case: this function returns <c>false</c> for an empty <c>TokenStream</c>.</i></p>
    /// </summary>
    /// <param name="stream"> the <c>TokenStream</c> </param>
    /// <param name="id"> the <c>TokenType</c> to search for </param>
    /// <returns> <c>true</c> if the current statement contains the <c>TokenType</c>; <c>false</c> otherwise </returns>
    let rec StatementContainsToken (stream: TokenStream) (id: TokenType): bool =
        match stream with
         | head                     :: _    when head.id = id     -> true
         | _                        :: tail                       -> (tail |> StatementContainsToken) id
         | []
         | {id=TokenType.SemiColon} :: _                          -> false

    /// <summary>
    ///     <p>Filters all <c>IllegalToken</c> tokens from the supplied <c>TokenStream</c>, and maps them all to
    ///        <c>SyntaxError</c>s.
    ///     </p>
    /// </summary>
    /// <param name="stream"> the <c>TokenStream</c> </param>
    /// <returns> a list of <c>SyntaxError</c>s - may be empty </returns>
    let rec GetErrors (stream: TokenStream): DioriteError list =
        // helper function for generating a descriptive message for the IllegalToken
        let AsSyntaxError (token: Token): DioriteError =
            let message: string = $"IllegalToken: \"{token.lexeme}\" at line {token.line}, column {token.column}"
            DioriteError.SyntaxError(message)

        match stream with
         | []                                       -> []
         | {id=TokenType.IllegalToken} as t :: tail -> (t |> AsSyntaxError) :: (GetErrors tail)
         | _                                :: tail -> GetErrors tail

    /// <summary>
    ///     <p>Recursively consumes the characters that satisfy the <c>predicate</c> until it no longer can do so.</p>
    ///     <p>It returns the consumed characters and the remaining characters as a result of this operation.</p>
    /// </summary>
    /// <param name="predicate"> the predicate which determines if the characters consumed </param>
    /// <param name="source"> the source characters </param>
    /// <returns> the consumed characters and the remaining characters </returns>
    let rec Consume (predicate: char -> bool) (source: char list): char list * char list =
        match source with
         | c :: tail when predicate c ->
            let (consumed: char list), (remaining: char list) = Consume predicate tail
            (c :: consumed, remaining)
         | source                     -> ([], source)

    /// <summary>
    ///     <p>A <c>Consumer</c> is a curried application of the <c>consume</c> function that has been preloaded with
    ///        a <c>predicate</c>. It can be simplified to a function that splits a list into two chunks: the consumed
    ///        characters, and the remaining characters.
    ///     </p>
    /// </summary>
    type Consumer = char list -> char list * char list

    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are letters.</p>
    /// </summary>
    let ConsumeLetters:      Consumer = Consume System.Char.IsLetter
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are digits.</p>
    /// </summary>
    let ConsumeDigits:       Consumer = Consume System.Char.IsDigit
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are blank.</p>
    /// </summary>
    let ConsumeBlanks:       Consumer = Consume System.Char.IsWhiteSpace
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are non-newline.</p>
    /// </summary>
    let ConsumeNonNewlines:  Consumer = Consume (fun c -> c <> '\n')
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are newline.</p>
    /// </summary>
    let ConsumeOnlyNewlines: Consumer = Consume (fun c -> c = '\n')
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are not blank.</p>
    /// </summary>
    let ConsumeUntilBlank:   Consumer = Consume (System.Char.IsWhiteSpace >> not)
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that it is not a double quote.</p>
    /// </summary>
    let ConsumeUntilQuotes:  Consumer = Consume (fun c -> c <> '"')

    /// <summary>
    ///     <p>Tokenises the string in the local context of the string.</p>
    ///     <p><i>This just means that line and column position information is local to the string that <c>tokenise</c>
    ///        is called with.</i>
    ///     </p>
    /// </summary>
    /// <param name="source"> the <b>Diorite</b> source code </param>
    /// <returns> the <c>source</c> code broken up into a <c>TokenStream</c> </returns>
    let Tokenise (source: string): TokenStream =
        let rec ReadFrom (source: char list) (line: uint, column: uint): TokenStream =
            match source with
             // empty string
             | [] -> []

             // numbers
             | c :: tail when c |> System.Char.IsDigit ->
                 let (integer: char list), (tail: char list) = ConsumeDigits (c::tail)
                 match tail with
                  | '.' :: tail ->
                      match (ConsumeDigits tail) with
                       // mark the '.' as illegal and then proceed
                       | [], remaining ->
                          let lexeme: string = integer |> System.String.Concat
                          let head: Token = {
                              lexeme = lexeme
                              id     = TokenType.Number
                              value  = TokenValue.Number (lexeme |> System.Double.Parse)
                              line   = line
                              column = column
                          }
                          let illegal: Token = {
                              lexeme = "."
                              id     = TokenType.IllegalToken
                              value  = TokenValue.None
                              line   = line
                              column = column + (uint head.lexeme.Length)
                          }
                          let tail: TokenStream = (remaining |> ReadFrom) (line, column + (uint head.lexeme.Length))
                          head :: illegal :: tail
                       // x.y
                       | decimal, remaining ->
                          let lexeme: string = (integer @ ['.'] @ decimal) |> System.String.Concat
                          let head: Token = {
                              lexeme = lexeme
                              id     = TokenType.Number
                              value  = TokenValue.Number (lexeme |> System.Double.Parse)
                              line   = line
                              column = column
                          }
                          let tail: TokenStream = (remaining |> ReadFrom) (line, column + (uint head.lexeme.Length))
                          head :: tail
                  // x
                  | remaining ->
                      let lexeme: string = integer |> System.String.Concat
                      let head: Token = {
                          lexeme = lexeme
                          id     = TokenType.Number
                          value  = TokenValue.Number (lexeme |> System.Double.Parse)
                          line   = line
                          column = column
                      }
                      let tail: TokenStream = (remaining |> ReadFrom) (line, column + (uint head.lexeme.Length))
                      head :: tail

             // variables, constants, & symbols
             | c :: tail when c |> System.Char.IsLetter ->
                 match (ConsumeLetters (c::tail)) with
                  // variable + subscript
                  | [character], d :: remaining when d |> System.Char.IsDigit ->
                      let subscript: uint8 = (uint8 d) - (uint8 '0')
                      let head: Token = {
                          lexeme = $"{character}{subscript}"
                          id     = TokenType.Variable
                          value  = TokenValue.Variable (character, subscript + 1uy)
                          line   = line
                          column = column
                      }
                      let tail: TokenStream = (remaining |> ReadFrom) (line, column + (uint head.lexeme.Length))
                      head :: tail

                  // variable
                  | [character], remaining ->
                      let head: Token = {
                          lexeme = $"{character}"
                          id     = TokenType.Variable
                          value  = TokenValue.Variable (character, 0uy)
                          line   = line
                          column = column
                      }
                      let tail: TokenStream = (remaining |> ReadFrom) (line, column + (uint head.lexeme.Length))
                      head :: tail

                  // keywords
                  | characters, remaining ->
                      let word: string = characters |> System.String.Concat
                      let id: TokenType = match word with
                                           | "if"               -> TokenType.If
                                           | "otherwise"        -> TokenType.Otherwise
                                           | "undefined"        -> TokenType.Undefined
                                           | "infinity" | "inf" -> TokenType.Infinity
                                           | "plot"             -> TokenType.Plot
                                           | "error"            -> TokenType.Error
                                           | "pi"               -> TokenType.Pi
                                           | "tau"              -> TokenType.Tau
                                           | "euler"            -> TokenType.Euler
                                           | _                  -> TokenType.Symbol
                      let head: Token = {
                          lexeme  = word
                          id      = id
                          value   = TokenValue.None
                          line    = line
                          column  = column
                      }
                      let tail: TokenStream = (remaining |> ReadFrom) (line, column + (uint word.Length))
                      head :: tail

             // args & params
             | ',' :: tail ->
                 let head: Token = {
                     lexeme = ","
                     id     = TokenType.Comma
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail

             // set notation
             | '-' :: '>' :: tail ->
                 let head: Token = {
                     lexeme = "->"
                     id     = TokenType.Arrow
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | ':' :: tail ->
                 let head: Token = {
                     lexeme = ":"
                     id     = TokenType.Colon
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail

             // comparison operators
             | '=' :: tail ->
                 let head: Token = {
                     lexeme = "="
                     id     = TokenType.Equals
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '!' :: '=' :: tail ->
                 let head: Token = {
                     lexeme = "!="
                     id     = TokenType.NotEqual
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '<' :: '=' :: tail ->
                 let head: Token = {
                     lexeme = "<="
                     id     = TokenType.LessThanOrEqual
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '>' :: '=' :: tail ->
                 let head: Token = {
                     lexeme = ">="
                     id     = TokenType.GreaterThanOrEqual
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '<' :: tail ->
                 let head: Token = {
                     lexeme = "<"
                     id     = TokenType.LessThan
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '>' :: tail ->
                 let head: Token = {
                     lexeme = ">"
                     id     = TokenType.GreaterThan
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail

             // arithmetic operators
             | '+' :: tail ->
                 let head: Token = {
                     lexeme = "+"
                     id     = TokenType.Plus
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '-' :: tail ->
                 let head: Token = {
                     lexeme = "-"
                     id     = TokenType.Hyphen
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '*' :: tail ->
                 let head: Token = {
                     lexeme = "*"
                     id     = TokenType.Asterisk
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '/' :: '/' :: tail ->
                 let head: Token = {
                     lexeme = "//"
                     id     = TokenType.DoubleForwardSlash
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '/' :: tail ->
                 let head: Token = {
                     lexeme = "/"
                     id     = TokenType.ForwardSlash
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '%' :: tail ->
                 let head: Token = {
                     lexeme = "%"
                     id     = TokenType.Percentage
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '^' :: tail ->
                 let head: Token = {
                     lexeme = "^"
                     id     = TokenType.Hat
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '!' :: tail ->
                 let head: Token = {
                     lexeme = "!"
                     id     = TokenType.Exclamation
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail

             // wrapping characters
             | '(' :: tail ->
                 let head: Token = {
                     lexeme = "("
                     id     = TokenType.LeftParenthesis
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | ')' :: tail ->
                 let head: Token = {
                     lexeme = ")"
                     id     = TokenType.RightParenthesis
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '|' :: tail ->
                 let head: Token = {
                     lexeme = "|"
                     id     = TokenType.Bar
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '{' :: tail ->
                 let head: Token = {
                     lexeme = "{"
                     id     = TokenType.LeftBrace
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '}' :: tail ->
                 let head: Token = {
                     lexeme = "}"
                     id     = TokenType.RightBrace
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | '[' :: tail ->
                 let head: Token = {
                     lexeme = "["
                     id     = TokenType.LeftBracket
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail
             | ']' :: tail ->
                 let head: Token = {
                     lexeme = "]"
                     id     = TokenType.RightBracket
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail

             // string literal
             | '"' :: tail ->
                 let (consumed: char list), (tail: char list) = ConsumeUntilQuotes tail
                 let lexeme: string = $"{consumed |> System.String.Concat}"
                 let (id: TokenType), (remaining: char list) =
                     match tail with
                      | '"' :: remaining -> (TokenType.StringLiteral, remaining)
                      | remaining        -> (TokenType.IllegalToken,  remaining)
                 let head: Token = {
                     lexeme = lexeme
                     id     = id
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (remaining |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail

             // end-of-statement
             | ';' :: tail ->
                 let head: Token = {
                     lexeme = ";"
                     id     = TokenType.SemiColon
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (tail |> ReadFrom) (line, column + (uint head.lexeme.Length))
                 head :: tail

             // comments
             | c :: tail when c = '#' ->
                 let (consumed: char list), (remaining: char list) = ConsumeNonNewlines (tail)
                 (remaining |> ReadFrom) (line, column + (uint consumed.Length))

             // newline
             | c :: tail when c = '\n' ->
                 let (consumed: char list), (remaining: char list) = ConsumeOnlyNewlines (c::tail)
                 (remaining |> ReadFrom) (line + (uint consumed.Length), 0u)

             // whitespace
             | c :: tail when c |> System.Char.IsWhiteSpace ->
                 let (consumed: char list), (remaining: char list) = ConsumeBlanks (c::tail)
                 (remaining |> ReadFrom) (line, column + (uint consumed.Length))

             // unrecognised token
             | trailing ->
                 let (lexeme: char list), (remaining: char list) = ConsumeUntilBlank trailing
                 let illegal: Token = {
                     lexeme = lexeme |> System.String.Concat
                     id     = TokenType.IllegalToken
                     value  = TokenValue.None
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = (remaining |> ReadFrom) (line, illegal.column + (uint illegal.lexeme.Length))
                 illegal :: tail
            
        let sourceCharacters: char list = source.ToCharArray() |> List.ofArray
        (sourceCharacters |> ReadFrom) (0u, 0u)
