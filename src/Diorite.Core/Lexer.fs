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
    ///     <p>These are simply identifier types for a <c>Token</c>, they hold no metadata, this makes it easier for the
    ///        parser to consume tokens.
    ///     </p>
    /// </summary>
    type TokenType =
        | IllegalToken

        // value types
        | Number
        | Variable
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
    ///     <p>The <c>TokenPayload</c> is the metadata for a particular <c>Token</c>.</p>
    ///     <p>There are only <i>two</i> types of payloads that the tokeniser recognises:
    ///        <list type='number'>
    ///            <item><description>a number (<c>float</c>)</description></item>
    ///            <item><description>a variable (<c>VariableType</c>)</description></item>
    ///        </list>
    ///        This feature replaces the original implementation, where the <c>TokenType</c> DU type retained that data,
    ///        however, it made parsing more tedious.
    ///     </p>
    /// </summary>
    type TokenPayload =
        | NoPayload
        | NumberPayload   of float
        | VariablePayload of VariableType

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
        lexeme:  string
        id:      TokenType
        payload: TokenPayload
        line:    int
        column:  int
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
    ///           let tokens: Lexer.TokenStream = Lexer.tokenise "2 + 2.5"
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
                          lexeme  = raw |> Transformers.charListToString
                          id      = TokenType.Number
                          payload = raw |> (Transformers.charListToFloat >> TokenPayload.NumberPayload)
                          line    = line
                          column  = column
                      }
                      let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                      head :: tail
                  // integer component + '.' (but no decimal component)
                  | '.' :: remaining ->
                      let head: Token = {
                          lexeme  = integerComponent |> Transformers.charListToString
                          id      = TokenType.Number
                          payload = integerComponent |> (Transformers.charListToFloat >> TokenPayload.NumberPayload)
                          line    = line
                          column  = column
                      }
                      let illegal: Token = {
                          lexeme  = "."
                          id      = TokenType.IllegalToken
                          payload = NoPayload
                          line    = line
                          column  = column + head.lexeme.Length
                      }
                      let tail: TokenStream = scan remaining line (illegal.column + 1)
                      head :: illegal :: tail
                  // integer component
                  | remaining ->
                      let head: Token = {
                          lexeme  = integerComponent |> Transformers.charListToString
                          id      = TokenType.Number
                          payload = integerComponent |> (Transformers.charListToFloat >> TokenPayload.NumberPayload)
                          line    = line
                          column  = column
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
                               lexeme  = $"{character}{encodedSubscript}"
                               id      = TokenType.Variable
                               payload = (character, encodedSubscript |> uint8) |> TokenPayload.VariablePayload
                               line    = line
                               column  = column
                           }
                           let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                           head :: tail
                       // no subscript
                       | remaining ->
                           let head: Token = {
                               lexeme  = $"{character}"
                               id      = TokenType.Variable
                               payload = (character, 0uy) |> TokenPayload.VariablePayload
                               line    = line
                               column  = column
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
                          lexeme  = word
                          id      = id
                          payload = NoPayload
                          line    = line
                          column  = column
                      }
                      let tail: TokenStream = scan remaining line (column + word.Length)
                      head :: tail

             // args & params
             | ',' :: remaining ->
                 let head: Token       = {lexeme=","; id=TokenType.Comma; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // set notation
             | '-' :: '>' :: remaining ->
                 let head: Token       = {lexeme="->"; id=TokenType.Arrow; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | ':'        :: remaining ->
                 let head: Token       = {lexeme=":"; id=TokenType.Colon; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // comparison operators
             | '<' :: '=' :: remaining ->
                 let head: Token       = {lexeme="<="; id=TokenType.LessThan; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | '>' :: '=' :: remaining ->
                 let head: Token       = {lexeme=">="; id=TokenType.LessThanOrEqual; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | '!' :: '=' :: remaining ->
                 let head: Token       = {lexeme="!="; id=TokenType.NotEqual; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | '='        :: remaining ->
                 let head: Token       = {lexeme="="; id=TokenType.Equals; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '<'        :: remaining ->
                 let head: Token       = {lexeme="<"; id=TokenType.LessThan; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '>'        :: remaining ->
                 let head: Token       = {lexeme="<"; id=TokenType.GreaterThan; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // arithmetic operators
             | '^'        :: remaining ->
                 let head: Token       = {lexeme="^"; id=TokenType.Hat; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '!'        :: remaining ->
                 let head: Token       = {lexeme="!"; id=TokenType.Exclamation; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '*'        :: remaining ->
                 let head: Token       = {lexeme="*"; id=TokenType.Asterisk; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '/' :: '/' :: remaining ->
                 let head: Token       = {lexeme="//"; id=TokenType.DoubleForwardSlash; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 2)
                 head :: tail
             | '/'        :: remaining ->
                 let head: Token       = {lexeme="/"; id=TokenType.ForwardSlash; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '%'        :: remaining ->
                 let head: Token       = {lexeme="%"; id=TokenType.Percentage; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '+'        :: remaining ->
                 let head: Token       = {lexeme="+"; id=TokenType.Plus; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '-'        :: remaining ->
                 let head: Token       = {lexeme="-"; id=TokenType.Hyphen; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '|'        :: remaining ->
                 let head: Token       = {lexeme="|"; id=TokenType.Bar; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // brackets, curly braces & square brackets
             | '('        :: remaining ->
                 let head: Token       = {lexeme="("; id=TokenType.LeftParenthesis; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | ')'        :: remaining ->
                 let head: Token       = {lexeme=")"; id=TokenType.RightParenthesis; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '{'        :: remaining ->
                 let head: Token       = {lexeme="{"; id=TokenType.LeftBrace; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '}'        :: remaining ->
                 let head: Token       = {lexeme="}"; id=TokenType.RightBrace; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | '['        :: remaining ->
                 let head: Token       = {lexeme="["; id=TokenType.LeftBracket; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail
             | ']'        :: remaining ->
                 let head: Token       = {lexeme="]"; id=TokenType.RightBracket; payload=NoPayload; line=line; column=column}
                 let tail: TokenStream = scan remaining line (column + 1)
                 head :: tail

             // end of statement
             | ';' :: remaining ->
                 let head: Token       = {lexeme=";"; id=TokenType.SemiColon; payload=NoPayload; line=line; column=column}
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
                     lexeme  = lexeme |> Transformers.charListToString
                     id      = TokenType.IllegalToken
                     payload = NoPayload
                     line    = line
                     column  = column
                 }
                 let tail: TokenStream = scan remaining line (column + head.lexeme.Length)
                 head :: tail

        let chars: char list = source |> Transformers.stringToCharList
        (scan chars) 0 0
