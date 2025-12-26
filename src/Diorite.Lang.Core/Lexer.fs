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
    ///     <p>The state returned by the <c>Tokenise</c> function.</p>
    ///     <p><b>1.</b> The first value (<c>Token option</c>), which is the potential token.</p>
    ///     <p><b>2.</b> The second value (<c>char list</c>), which are the remaining characters.</p>
    /// </summary>
    type LexerState = (Token * char list) option

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
    let ConsumeBlanks:       Consumer = Consume (fun c -> c <> '\n' && System.Char.IsWhiteSpace c)
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
         | c :: tail when c |> System.Char.IsLetter ->
             match (ConsumeLetters (c::tail)) with
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
                                       | "im"               -> TokenType.Im
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
        
