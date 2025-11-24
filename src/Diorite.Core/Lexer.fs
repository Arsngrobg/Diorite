// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Lexer.fs
// Summary: The lexer for the Diorite mathematics language
// Author:  Arsngrobg, Borngle
// Version: v1.7
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The <c>Lexer</c> module contains bindings related to producing tokens from raw strings into lexical
///        tokens.
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
module Lexer =
    // the signature for a function that transforms a type into another type
    type private Transformer<'a, 'b> = 'a -> 'b

    [<RequireQualifiedAccess>]
    module private Transformers =
        let stringToCharList: Transformer<string, char list> = Seq.toList
        let charToInt:        Transformer<char, int>         = (fun c -> int c - int '0')
        let charListToString: Transformer<char list, string> = (Array.ofList >> System.String)
        let charListToFloat:  Transformer<char list, float>  = (charListToString >> System.Double.Parse)

    // the signature for a predicate that the consumer uses to determine whether a character should be consumed
    type private ConsumerPredicate = char -> bool

    module private ConsumerPredicates =
        let isLetter:   ConsumerPredicate = System.Char.IsLetter
        let isDigit:    ConsumerPredicate = System.Char.IsDigit
        let isBlank:    ConsumerPredicate = System.Char.IsWhiteSpace
        let notNewline: ConsumerPredicate = (fun c -> c <> '\n')
        let nonBlank:   ConsumerPredicate = (isBlank >> not)

    // recursively consume character given that they satisfy the given predicate
    // returns the consumed characters and the remaining characters
    let rec private consume (predicate: ConsumerPredicate) (src: char list): char list * char list =
        match src with
         | c :: tail when predicate c ->
            let (consumed: char list), (remaining: char list) = consume predicate tail
            (c :: consumed, remaining)
         | _                          -> ([], src)

    /// <summary>
    ///     <p>The discriminated union type that identifies the <c>Token</c> in a <c>TokenStream</c>.</p>
    ///     <p>Some <c>TokenType</c>s may store some metadata about it like <c>TokenType.Number</c>, which stores the
    ///        numerical representation of the token consisting of a numbered string.
    ///     </p>
    /// </summary>
    type TokenType =
        | IllegalToken

        // value types
        | Number   of float
        | Variable of VariableType
        | Symbol

        // reserved words
        | Undefined
        | Infinity
        | Plot

        // symbolic constants
        | Pi
        | Tau
        | Euler

        // set definitions
        | Colon
        | Arrow

        // args & params
        | Comma

        // comparison operators
        | Equals
        | LessThan
        | GreaterThan
        | LessThanOrEqual
        | GreaterThanOrEqual
        | NotEqual

        // arithmetic operators
        | Hat
        | Exclamation
        | Asterisk
        | ForwardSlash
        | DoubleForwardSlash
        | Percentage
        | Plus
        | Hyphen

        // control flow
        | If
        | Otherwise

        // wrappers
        | LeftParenthesis
        | RightParenthesis
        | LeftBracket
        | RightBracket
        | LeftBrace
        | RightBrace
        | Bar

        // end of statement
        | SemiColon

    /// <summary>
    ///     <p>The <c>Token</c> type represents a lexical unit in the <b>Diorite</b> mathematics language.</p>
    ///     <p>It is composed of:
    ///        <ul>
    ///            <li>the <c>lexeme</c>, which is the <c>string</c> slice that this <c>Token</c> represents</li>
    ///            <li>the <c>id</c>, which denotes the type of <c>Token</c></li>
    ///            <li>the <c>line</c>, the line of the respective context in which this token is located</li>
    ///            <li>the <c>column</c>, the column of the respective context in which this token is located</li>
    ///        </ul>
    ///     </p>
    /// </summary>
    type Token = {
        lexeme: string
        id:     TokenType
        line:   int
        column: int
    }

    /// <summary>
    ///     <p>The <c>TokenStream</c> is a sequence of tokens.</p>
    ///     <p>As of right now, it is a typedef for a <c>Token list</c>.</p>
    /// </summary>
    type TokenStream = Token list

    let rec statementContainsToken (tokens: TokenStream) (id: TokenType): bool =
        match tokens with
         | []                               -> false
         | token :: _    when token.id = id -> true
         | _     :: tail                    -> (statementContainsToken tail) id

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>Token</c>.</p>
    /// </summary>
    /// <param name='token'> the <c>Token</c> </param>
    /// <returns> the <c>string</c> representation of the supplied <c>Token</c> </returns>
    let strToken (token: Token): string =
        $"Token['{token.lexeme}', {token.id}, [{token.line}:{token.column}]]"

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>TokenStream</c>.</p>
    /// </summary>
    /// <param name='tokens'> the <c>TokenStream</c> </param>
    /// <returns> the <c>string</c> representation of the supplied <c>TokenStream</c> </returns>
    let rec strTokens (tokens: TokenStream): string =
        match tokens with
         | []                                      -> ""
         | [t] when t.id = TokenType.SemiColon     -> $"({strToken t})"
         | {id = TokenType.SemiColon} as t :: tail -> $"({strToken t})\n{strTokens tail}"
         | t                               :: tail -> $"({strToken t}) {strTokens tail}"

    /// <summary>
    ///     <p>Finds the first instance of an <c>IllegalToken</c> in the <c>TokenStream</c>.</p>
    ///     <p></p>
    /// </summary>
    /// <param name='tokens'> the tokens to search for an <c>IllegalToken</c> </param>
    /// <returns> an <c>option</c>, that may contain the <c>DioriteError</c> (<c>SyntaxError</c>) </returns>
    let rec getError (tokens: TokenStream): DioriteError option =
        let getErrorMsg (illegal: Token): string =
            $"Unexpected token: '{illegal.lexeme}' at [{illegal.line}:{illegal.column}]"

        match tokens with
         | []                                         -> None
         | {id = TokenType.IllegalToken} as head :: _ -> (getErrorMsg >> DioriteError.SyntaxError >> Some) head
         | _    :: tail                               -> getError tail

    /// <summary>
    ///     <p>Produces tokens from the supplied <c>string</c> into a sequence of <c>Token</c>s
    ///        (<c>TokenStream</c>).
    ///     </p>
    ///     <p>Typical usage:
    ///        <code>
    ///           let tokens: Lexer.TokenStream = Lexer.tokenize "2 + 2.5"
    ///           match (Lexer.getError tokens) with
    ///            | Some err -> printf $"{err}\n"
    ///            | None     -> printf $"{tokens}\n"
    ///        </code>
    ///     </p>
    /// </summary>
    /// <param name='source'> the source string to tokenise </param>
    /// <returns> a <c>TokenStream</c> that consists of the tokens derived from the source string </returns>
    let tokenise (source: string): TokenStream =
        // recursive scanner that tracks the line and column local to the supplied string
        let rec scan (src: char list) (line: int) (column: int): TokenStream =
            match src with
             // empty string
             | [] -> []

             // numbers
             | c :: _ when ConsumerPredicates.isDigit c ->
                 let (integerComponent: char list), (numberTail: char list) = consume ConsumerPredicates.isDigit src
                 match numberTail with
                  // integer component + decimal component
                  | '.' :: c :: tail when (ConsumerPredicates.isDigit c) ->
                      let consumerState: char list * char list = consume ConsumerPredicates.isDigit (c :: tail)
                      let (decimalComponent: char list), (remaining: char list) = consumerState

                      let raw: char list = integerComponent @ ['.'] @ decimalComponent
                      let head: Token = {
                          lexeme = raw |> Transformers.charListToString
                          id     = raw |> (Transformers.charListToFloat >> TokenType.Number)
                          line   = line
                          column = column
                      }
                      let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                      head :: tail
                  // integer component + '.' (but no decimal component)
                  | '.' :: remaining ->
                      let head: Token = {
                          lexeme = integerComponent |> Transformers.charListToString
                          id     = integerComponent |> (Transformers.charListToFloat >> TokenType.Number)
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
                          lexeme = integerComponent |> Transformers.charListToString
                          id     = integerComponent |> (Transformers.charListToFloat >> TokenType.Number)
                          line   = line
                          column = column
                      }
                      let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                      head :: tail

             // variables, keywords, and constants
             | c :: _ when ConsumerPredicates.isLetter c ->
                 match (consume ConsumerPredicates.isLetter src) with
                  // variable (+ optional subscript)
                  | [character], variableTail ->
                      match variableTail with
                       // with subscript
                       | subscript :: remaining when (ConsumerPredicates.isDigit subscript) ->
                           let encodedSubscript: int = subscript |> (Transformers.charToInt >> (fun s -> s + 1))
                           let head: Token = {
                               lexeme = $"{character}{encodedSubscript}"
                               id     = (character, encodedSubscript |> uint8) |> TokenType.Variable
                               line   = line
                               column = column
                           }
                           let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                           head :: tail
                       // no subscript
                       | remaining ->
                           let head: Token = {
                               lexeme = $"{character}"
                               id     = (character, 0uy) |> TokenType.Variable
                               line   = line
                               column = column
                           }
                           let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                           head :: tail

                  // keywords & constants
                  | chars, remaining ->
                      let word: string = chars |> Transformers.charListToString
                      let id: TokenType = match word with
                                           | "if"               -> TokenType.If
                                           | "otherwise"        -> TokenType.Otherwise
                                           | "undefined"        -> TokenType.Undefined
                                           | "infinity" | "inf" -> TokenType.Infinity
                                           | "plot"             -> TokenType.Plot
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
                 let _, (remaining: char list) = consume ConsumerPredicates.notNewline src
                 scan remaining line (column + remaining.Length)

             // newline (increment line)
             | '\n' :: remaining ->
                 scan remaining (line + 1) 0

             // skip whitespace
             | c :: _ when (ConsumerPredicates.isBlank c) ->
                 let (consumed: char list), (remaining: char list) = consume ConsumerPredicates.isBlank src
                 scan remaining line (column + consumed.Length)

             // illegal tokens
             | _ ->
                 let (lexeme: char list), (remaining: char list) = consume ConsumerPredicates.nonBlank src
                 let head: Token = {
                     lexeme = lexeme |> Transformers.charListToString
                     id     = TokenType.IllegalToken
                     line   = line
                     column = column
                 }
                 let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                 head :: tail

        let chars: char list = source |> Transformers.stringToCharList
        (scan chars) 0 0
