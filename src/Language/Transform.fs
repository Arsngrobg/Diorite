// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Transform.fs
// Summary: The functions for transforming diorite source files into token streams and subsequently ASTs
// Author:  Arsngrobg, Borngle
// Version: v1.5
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>Lexer</c> module groups up related bindings that represent the tokenization stage of the code
///     transformation. The tokens are then passed to the <c>Parser</c> module to extract meaning from the token
///     stream.
///     <code>
///         let tokens = Lexer.lex("x = 2")
///         printf $"{tokens}" // output: "[VARIABLE "x", EQUALS, NUMBER 2]"
///     </code>
/// </summary>
module Lexer =
    /// <summary>
    ///     The <c>Token</c> discriminated union type represents a type of token.
    /// </summary>
    type Token =
        // new lines are abstracted so we need to differentiate between a new statement or just separation via newline
        | NewStatement
        // for safe escape from the lexer function and display an error whilst continuing execution
        | IllegalToken      of string
        // value types
        | Number            of float
        | Variable          of string
        // constants
        | Undefined
        | Infinity
        | Pi
        | Tau
        // operators & operator priority
        | Equals
        | LeftParenthesis
        | RightParenthesis
        | Exponent
        | Multiply
        | Divide
        | Percentage
        | Plus
        | Subtract
        | Bar
        // pre-defined functions / symbolic-named functions
        | Symbol of string
        // set hints
        | SetHint
        | NumberSet

    /// <summary>
    ///     A debug function for outputting the tokens in a structured manner from a supplied token stream.
    /// </summary>
    /// <param name="tokens"> the <c>Lexer.Token</c> stream </param>
    let rec tokens2str (tokens: Token list): string =
        match tokens with
         | []                    -> ""
         | NewStatement :: tail -> $"\nNewStatement\n{tokens2str tail}"
         | t            :: tail -> $"({t}) {tokens2str tail}"

    /// <summary>
    ///     Helper function to convert a <c>string</c> into a list of <c>char</c>s.
    /// </summary>
    /// <param name='str'> the string to convert into a list of characters </param>
    /// <returns> the original <c>string</c>, transformed into a list of characters </returns>
    let stringToChars (str: string): char list =
        [ for c in str do c ]
        
    /// <summary>
    ///     Helper function to evaluate if a character is alphabetical.
    /// </summary>
    /// <param name='c'> the character </param>
    /// <returns> <c>true</c> if the character is a letter, <c>false</c> if otherwise </returns>
    let isAlpha (c: char): bool =
        System.Char.IsLetter c
    
    /// <summary>
    ///     Helper function to evaluate if a character is a digit.
    /// </summary>
    /// <param name='c'> the character </param>
    /// <returns> <c>true</c> if the character is a digit, <c>false</c> if otherwise </returns>
    let isDigit (c: char): bool =
        System.Char.IsDigit c

    /// <summary>
    ///     Helper function to evaluate if a character is whitespace.
    /// </summary>
    /// <param name='c'> the character </param>
    /// <returns> <c>true</c> if the character is a whitespace, <c>false</c> if otherwise </returns>
    let isWhitespace (c: char): bool =
        System.Char.IsWhiteSpace c

    /// <summary>
    ///     Helper function to evaluate any character except from whitespace characters.
    /// </summary>
    /// <param name='c'> the character </param>
    /// <returns> <c>true</c> if the character is not a whitespace character, <c>false</c> if otherwise </returns>
    let any (c: char): bool =
        not(isWhitespace c)

    /// <summary>
    ///     Helper function for parsing a string as a numeric value.
    /// </summary>
    /// <param name='str'> the string to parse </param>
    /// <returns> the <c>string</c> transformed into its <c>float</c> representation </returns>
    let parseNumber (str: string): float =
        System.Double.Parse str

    /// <summary>
    ///     Helper function for collapsing a list of characters into a <c>string</c>.
    /// </summary>
    /// <param name='chars'> list of characters </param>
    /// <returns> the <c>char list</c> formatted as a <c>string</c> </returns>
    let charsToString (chars: char list): string =
        System.String.Concat chars
    
    /// <summary>
    ///     Recursively consumes characters if they satisfy a given predicate.
    /// </summary>
    /// <param name='predicate'> function that is supplied a <c>char</c> which wraps a <b>bool</b> expression </param>
    /// <param name='src'> list of characters being checked </param>
    /// <returns> a tuple: consisting of the consumed characters, and the remaining characers </returns>
    let rec consume (predicate: char -> bool) (src: char list): char list * char list =
        match src with
         | c :: tail when predicate c -> // match if true
            let (consumed: char list ), (remaining: char list) = consume predicate tail
            ( c:: consumed, remaining )   // c is prepended to consumed, and remaining is the rest of the list that does not match
         | _ -> ( [], src )

    /// <summary>
    ///     Converts the supplied <c>src</c> string into a stream of tokens.
    /// </summary>
    /// <param name='src'> the raw string to be tokenized </param>
    /// <returns> a <c>IO.Result</c> that may contain the list of tokens or an error </returns>
    let lex (src: string): Token list IO.Result =
        let rec scan (src: char list): Token list =
            match src with
             | [] -> []

             // newlines and any other whitespace after it should be recognised as a NewStatement
             | c :: tail when c = '\n' ->
                 let _, (remaining: char list) = consume isWhitespace ( c :: tail )
                 match ( remaining |> List.forall isWhitespace ) with
                  | true  -> []
                  | false -> NewStatement :: scan remaining

             // sets
             | ':' :: tail | '-' :: '>' :: tail ->
                let _, (remaining: char list) = consume isWhitespace tail
                match remaining with
                 | ( 'N' | 'Z' | 'Q' | 'I' | 'R' | 'C' ) :: tail -> SetHint :: NumberSet :: scan tail
                 | remaining                                     ->
                     let (consumed: char list), _ = consume any remaining
                     [ consumed |> charsToString |> IllegalToken ]

             // variables, built-in functions, and constants
             | c :: tail when isAlpha c ->
                let (letters: char list), (remaining: char list) = consume isAlpha ( c :: tail )
                let name: string = charsToString letters

                // any 1-length string of alpha characters is defined as a variable, symbolic name otherwise
                let tokenType: Token =
                    match letters with
                     | [ _ ]   -> Variable name
                     |   _     -> Symbol   name

                match name with
                 // constants
                 | "undefined"        -> Undefined :: scan remaining
                 | "infinity" | "inf" -> Infinity  :: scan remaining
                 | "pi"               -> Pi        :: scan remaining
                 | "tau"              -> Tau       :: scan remaining
                 | _ ->
                    match remaining with
                     | '(' :: tail -> tokenType :: LeftParenthesis :: scan tail
                       // parenthesis token separated for better parser context
                     | _           -> tokenType :: scan remaining
                    
             // numbers
             | c :: tail when isDigit c ->
                let digits, remaining = consume isDigit ( c :: tail )
                match remaining with
                 | '.' :: tail ->
                    let (fractionalDigits: char list), (remaining: char list) = consume isDigit ( tail )
                    let number = charsToString ( digits @ ['.'] @ fractionalDigits )
                    let result = parseNumber number
                    Number result :: scan remaining
                 | _ ->
                    let number = charsToString digits
                    let result = parseNumber number
                    Number result :: scan remaining
            
             // operators and symbols
             | '=' :: tail -> Equals           :: scan tail
             | '(' :: tail -> LeftParenthesis  :: scan tail
             | ')' :: tail -> RightParenthesis :: scan tail
             | '^' :: tail -> Exponent         :: scan tail
             | '*' :: tail -> Multiply         :: scan tail
             | '/' :: tail -> Divide           :: scan tail
             | '%' :: tail -> Percentage       :: scan tail
             | '+' :: tail -> Plus             :: scan tail
             | '-' :: tail -> Subtract         :: scan tail
             | '|' :: tail -> Bar              :: scan tail
                
             // Other cases
             | c :: tail when isWhitespace c -> scan tail
             | remaining                     ->
                 let (consumed: char list), _ = consume any remaining
                 [ consumed |> charsToString |> IllegalToken ]

        let tokens: Token list = src |> stringToChars |> scan

        // we need to know if the lexer failed and find the string that caused it
        let rec getFailingToken (tokens: Token list): string option =
            match tokens with
             | [ IllegalToken lexeme ] -> Some lexeme
             | _ :: tail               -> getFailingToken tail
             | _                       -> None

        // fail if IllegalToken was found
        match tokens |> getFailingToken with
         | Some lexeme -> IO.Failure(lexeme |> IO.LexerError)
         | None        -> IO.Success tokens

/// <summary>
///     The <c>Parser</c> module.
///     All related functionality for parsing a provided token stream.
/// </summary>
module Parser =
    /// <summary>
    ///     The node types for the AST.
    /// </summary>
    type NodeType =
        // value types
        | NUMBER     of float
        | VARIABLE   of string
        // constants
        | UNDEFINED
        | INFINITY
        // operators
        | EQUALS
        | EXPONENT
        | MULTIPLY
        | DIVIDE
        | PERCENTAGE
        | ADD
        | SUBTRACT
        // builtin functions
        | BUILTIN

    /// <summary>
    ///     A node in an Abstract Syntax Tree (AST) of the grammar.
    /// </summary>
    /// <param name='nodeType'> the type of <c>ASTNode</c> </param>
    /// <param name='children'> the children of this <c>ASTNode</c> - can be zero </param>
    [<Struct>]
    type ASTNode = {
        nodeType: NodeType
        // 0 children = leaf node (number, variable)
        // 1 child    = unary operation
        // 2 children = binary operation
        // x children = function arguments
        children: ASTNode list
    }

    /// <summary>
    ///     Parses the supplied list of <c>tokens</c> into a formatted AST.
    /// </summary>
    /// <param name='tokens'> the token stream to parse into an AST </param>
    /// <returns> a root node of the parsed sequence of tokens </returns>
    let parse(tokens: Lexer.Token list): ASTNode =
        {nodeType=NUMBER 0; children=[]}

