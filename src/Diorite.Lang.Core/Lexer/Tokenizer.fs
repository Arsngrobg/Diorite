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

namespace Diorite.Lang.Core.Lexer

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors
open Diorite.Lang.Core.Lexer.Consumers

/// <summary>
///     <p>The <c>Tokenizer</c> module contains logic for tokenising <b>Diorite</b> source code.</p>
/// </summary>
[<AutoOpen>]
module Tokenizer =
    /// <summary>
    ///     <p>Attempts to produce the next token from the supplied character list.</p>
    ///     <p>This function will recursively search until it has either: found a token, or has reached the end of the
    ///        character list.
    ///     </p>
    /// </summary>
    /// <param name="line"> the current line number </param>
    /// <param name="column"> the current column number </param>
    /// <param name="substr"> the sub string character sequence of the original string </param>
    /// <returns> a <c>Token</c> wrapped in an <c>option</c> type, as it may not be able to return one </returns>
    let rec GetNextToken (line: uint, column: uint) (substr: char list): LexerState =
        match substr with
         // empty string
         | [] -> None

         // numbers
         | c :: tail when c |> System.Char.IsDigit ->
             let (integer: char list), (tail: char list) = ConsumeDigits (c::tail)
             match tail with
              | '.' :: tail ->
                  match (ConsumeDigits tail) with
                   // ignore the '.' lexeme - which will get marked as illegal on next call
                   | [], remaining ->
                      let lexeme: string = integer |> System.String.Concat
                      let token: Token = {
                          lexeme = lexeme
                          id     = TokenType.Number
                          value  = TokenValue.Number (lexeme |> System.Double.Parse)
                          line   = line
                          column = column
                      }
                      Some (token, remaining)
                   // x.y
                   | decimal, remaining ->
                      let lexeme: string = (integer @ ['.'] @ decimal) |> System.String.Concat
                      let token: Token = {
                          lexeme = lexeme
                          id     = TokenType.Number
                          value  = TokenValue.Number (lexeme |> System.Double.Parse)
                          line   = line
                          column = column
                      }
                      Some (token, remaining)
              // x
              | remaining ->
                  let lexeme: string = integer |> System.String.Concat
                  let token: Token = {
                      lexeme = lexeme
                      id     = TokenType.Number
                      value  = TokenValue.Number (lexeme |> System.Double.Parse)
                      line   = line
                      column = column
                  }
                  Some (token, remaining)

         // variables, constants, & symbols
         | c :: tail when (c |> System.Char.IsLetter) || (c = '_') ->
             match (ConsumeSymbol (c::tail)) with
              // variable + subscript
              | [character], d :: remaining when d |> System.Char.IsDigit ->
                  let subscript: uint8 = (uint8 d) - (uint8 '0')
                  let token: Token = {
                      lexeme = $"{character}{subscript}"
                      id     = TokenType.Variable
                      value  = TokenValue.Variable (character, subscript + 1uy)
                      line   = line
                      column = column
                  }
                  Some (token, remaining)

              // variable
              | [character], remaining ->
                  let token: Token = {
                      lexeme = $"{character}"
                      id     = TokenType.Variable
                      value  = TokenValue.Variable (character, 0uy)
                      line   = line
                      column = column
                  }
                  Some (token, remaining)

              // keywords
              | characters, remaining ->
                  let word: string = characters |> System.String.Concat
                  let id: TokenType = match word with
                                       | "complex"          -> TokenType.Complex
                                       | "im"               -> TokenType.Im
                                       | "re"               -> TokenType.Re
                                       | "if"               -> TokenType.If
                                       | "otherwise"        -> TokenType.Otherwise
                                       | "undefined"        -> TokenType.Undefined
                                       | "infinity" | "inf" -> TokenType.Infinity
                                       | "plot"             -> TokenType.Plot
                                       | "using"            -> TokenType.Using
                                       | "error"            -> TokenType.Error
                                       | "pi"               -> TokenType.Pi
                                       | "tau"              -> TokenType.Tau
                                       | "euler"            -> TokenType.Euler
                                       | _                  -> TokenType.Symbol
                  let token: Token = {
                      lexeme  = word
                      id      = id
                      value   = TokenValue.None
                      line    = line
                      column  = column
                  }
                  Some (token, remaining)

         // args & params
         | ',' :: remaining ->
             let token: Token = {
                 lexeme = ","
                 id     = TokenType.Comma
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)

         // set notation
         | '-' :: '>' :: remaining ->
             let token: Token = {
                 lexeme = "->"
                 id     = TokenType.Arrow
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | ':' :: remaining ->
             let token: Token = {
                 lexeme = ":"
                 id     = TokenType.Colon
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)

         // comparison operators
         | '=' :: remaining ->
             let token: Token = {
                 lexeme = "="
                 id     = TokenType.Equals
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '!' :: '=' :: remaining ->
             let token: Token = {
                 lexeme = "!="
                 id     = TokenType.NotEqual
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '<' :: '=' :: remaining ->
             let token: Token = {
                 lexeme = "<="
                 id     = TokenType.LessThanOrEqual
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '>' :: '=' :: remaining ->
             let token: Token = {
                 lexeme = ">="
                 id     = TokenType.GreaterThanOrEqual
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '<' :: remaining ->
             let token: Token = {
                 lexeme = "<"
                 id     = TokenType.LessThan
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '>' :: remaining ->
             let token: Token = {
                 lexeme = ">"
                 id     = TokenType.GreaterThan
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)

         // arithmetic operators
         | '+' :: remaining ->
             let token: Token = {
                 lexeme = "+"
                 id     = TokenType.Plus
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '-' :: remaining ->
             let token: Token = {
                 lexeme = "-"
                 id     = TokenType.Hyphen
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '*' :: remaining ->
             let token: Token = {
                 lexeme = "*"
                 id     = TokenType.Asterisk
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '/' :: '/' :: remaining ->
             let token: Token = {
                 lexeme = "//"
                 id     = TokenType.DoubleForwardSlash
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '/' :: remaining ->
             let token: Token = {
                 lexeme = "/"
                 id     = TokenType.ForwardSlash
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '%' :: remaining ->
             let token: Token = {
                 lexeme = "%"
                 id     = TokenType.Percentage
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '^' :: remaining ->
             let token: Token = {
                 lexeme = "^"
                 id     = TokenType.Hat
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '!' :: remaining ->
             let token: Token = {
                 lexeme = "!"
                 id     = TokenType.Exclamation
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)

         // wrapping characters
         | '(' :: remaining ->
             let token: Token = {
                 lexeme = "("
                 id     = TokenType.LeftParenthesis
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | ')' :: remaining ->
             let token: Token = {
                 lexeme = ")"
                 id     = TokenType.RightParenthesis
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '|' :: remaining ->
             let token: Token = {
                 lexeme = "|"
                 id     = TokenType.Bar
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '{' :: remaining ->
             let token: Token = {
                 lexeme = "{"
                 id     = TokenType.LeftBrace
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '}' :: remaining ->
             let token: Token = {
                 lexeme = "}"
                 id     = TokenType.RightBrace
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | '[' :: remaining ->
             let token: Token = {
                 lexeme = "["
                 id     = TokenType.LeftBracket
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)
         | ']' :: remaining ->
             let token: Token = {
                 lexeme = "]"
                 id     = TokenType.RightBracket
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)

         // string literal
         | '"' :: tail ->
             let (consumed: char list), (tail: char list) = ConsumeUntilQuotes tail
             let lexeme: string = $"{consumed |> System.String.Concat}"
             let (id: TokenType), (remaining: char list) =
                 match tail with
                  | '"' :: remaining -> (TokenType.StringLiteral, remaining)
                  | remaining        -> (TokenType.IllegalToken,  remaining)

             let token: Token = {
                 lexeme = lexeme
                 id     = id
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)

         // end-of-statement
         | ';' :: remaining ->
             let token: Token = {
                 lexeme = ";"
                 id     = TokenType.SemiColon
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (token, remaining)

         // comments
         | '#' :: tail ->
             let (consumed: char list), (remaining: char list) = ConsumeNonNewlines tail
             ((line, column + (uint consumed.Length) + 1u), remaining) ||> GetNextToken // +1 for '#'

         | '\n' :: tail ->
             let (consumed: char list), (remaining: char list) = ConsumeOnlyNewlines tail
             ((line + (uint consumed.Length) + 1u, 1u), remaining) ||> GetNextToken // +1 for '\n'

         // whitespace
         | c :: tail when c |> System.Char.IsWhiteSpace ->
             let (consumed: char list), (remaining: char list) = ConsumeBlanks tail
             ((line, column + (uint consumed.Length) + 1u), remaining) ||> GetNextToken // +1 for ' '

         // unrecognised token
         | unknown ->
             let (lexeme: char list), (remaining: char list) = ConsumeUntilBlank unknown
             let illegal: Token = {
                 lexeme = lexeme |> System.String.Concat
                 id     = TokenType.IllegalToken
                 value  = TokenValue.None
                 line   = line
                 column = column
             }
             Some (illegal, remaining)

    /// <summary>
    ///     <p>Tokenises the string in the local context of the string.</p>
    ///     <p><i>This just means that line and column position information is local to the string that <c>tokenise</c>
    ///        is called with.</i>
    ///     </p>
    /// </summary>
    /// <param name="source"> the <b>Diorite</b> source code </param>
    /// <returns> the <c>source</c> code broken up into a <c>TokenStream</c> </returns>
    let Tokenise (source: string): TokenStream =
        let sourceCharacters: char list = source.ToCharArray() |> List.ofArray

        let rec Accumulate (substr: char list) (accumulator: TokenStream): TokenStream =
            let pos: uint * uint =
                if accumulator.Length = 0 then
                    (1u, 1u)
                else
                    let head: Token = accumulator.Head
                    (head.line, head.column + (uint head.lexeme.Length))

            match (GetNextToken pos substr) with
             | Some (token, tail) -> (Accumulate tail) (token :: accumulator)
             | None               -> List.rev accumulator

        (Accumulate sourceCharacters) []

    /// <summary>
    ///     <p>Filters all <c>IllegalToken</c> tokens from the supplied <c>TokenStream</c>, and maps it to a
    ///        <c>SyntaxError</c>.
    ///     </p>
    /// </summary>
    /// <param name="stream"> the <c>TokenStream</c> </param>
    /// <returns> a list of <c>SyntaxError</c>s - may be empty </returns>
    let GetTokenizerError (stream: TokenStream): DioriteError option =
        // helper function
        let rec CreateParagraph (stream: TokenStream): string list =
            match stream with
             | []                                       -> []
             | {id=TokenType.IllegalToken} as t :: tail -> $"\n\tIllegalToken: \"{t.lexeme}\"" :: (CreateParagraph tail)
             | _                                :: tail -> CreateParagraph tail

        match (CreateParagraph stream) with
         | []   -> None
         | errs -> (String.concat "" errs, None) |> (DioriteError.SyntaxError >> Some)
