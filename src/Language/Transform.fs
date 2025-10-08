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
// Version: v1.4
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
        NEW_STATEMENT
        // value types
        | NUMBER            of float
        | VARIABLE          of string
        // constants
        | UNDEFINED
        | INFINITY
        | PI
        | TAU
        // operators & operator priority
        | EQUALS
        | L_PARENTHESIS
        | R_PARENTHESIS
        | EXPONENT
        | MULTIPLY
        | DIVIDE
        | PERCENTAGE
        | PLUS
        | SUBTRACT
        | BAR
        // pre-defined functions / symbolic-named functions
        | SYMBOL of string
        // set hints
        | SET_HINT
        | NUMBER_SET

    /// <summary>
    ///     A debug function for outputting the tokens in a structured manner from a supplied token stream.
    /// </summary>
    /// <param name="tokens"> the <c>Lexer.Token</c> stream </param>
    let rec tokens2str (tokens: Token list): string =
        match tokens with
         | []                    -> "\n"
         | NEW_STATEMENT :: tail -> $"\nNEW_STATEMENT\n{tokens2str tail}"
         | t             :: tail -> $"({t}) {tokens2str tail}"

    /// <summary>
    ///     Helper function to convert a <c>string</c> into a list of <c>char</c>s.
    /// </summary>
    /// <param name='str'></param>
    let stringToChars (str: string): char list =
        Seq.toList str
        
    /// <summary>
    ///     Helper function to evaluate if a character is alphabetical.
    /// </summary>
    /// <param name='c'> the character </param>
    let isAlpha (c: char): bool =
        System.Char.IsLetter c
    
    /// <summary>
    ///     Helper function to evaluate if a character is a digit.
    /// </summary>
    /// <param name='c'> the character </param>
    let isDigit (c: char): bool =
        System.Char.IsDigit c

    /// <summary>
    ///     Helper function to evaluate if a character is whitespace.
    /// </summary>
    /// <param name='c'> the character </param>
    let isWhitespace<'T> (c: char): bool =
        System.Char.IsWhiteSpace c

    /// <summary>
    ///     Helper function for parsing a string as a numeric value.
    /// </summary>
    /// <param name='str'> the string to parse </param>
    let parseNumber (str: string): float =
        System.Double.Parse str

    /// <summary>
    ///     Helper function for collapsing a list of characters into a <c>string</c>.
    /// </summary>
    /// <param name='chars'> list of characters </param>
    let charsToString (chars: char list): string =
        System.String.Concat chars
    
    /// <summary>
    ///     Recursively consumes characters if they satisfy a given predicate.
    /// </summary>
    /// <param name='predicate'> function that is supplied a <c>char</c> which wraps a <b>bool</b> expression </param>
    /// <param name='src'> list of characters being checked </param>
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
    let lex (src: string): Token list =
        let rec scan (src: char list): Token list =
            match src with
             | [] -> []
             | c :: tail when c = '\n' ->
                 let _, (remaining: char list) = consume isWhitespace ( c :: tail )
                 match ( remaining |> List.forall isWhitespace ) with
                  | true  -> []
                  | false -> NEW_STATEMENT :: scan remaining

             // sets
             | ':' :: tail | '-' :: '>' :: tail ->
                let _, (remaining: char list) = consume isWhitespace tail
                match remaining with
                 | ( 'N' | 'Z' | 'Q' | 'I' | 'R' | 'C' ) :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                 | _                                             -> raise (System.Exception("Lexer error"))

             // variables, built-in functions, and constants
             | c :: tail when isAlpha c ->
                let (letters: char list), (remaining: char list) = consume isAlpha ( c :: tail )
                let name: string = charsToString letters

                // any 1-length string of alpha characters is defined as a variable, symbolic name otherwise
                let tokenType: Token =
                    match letters with
                     | _ when letters.Length = 1 -> VARIABLE name
                     | _                         -> SYMBOL name

                match name with
                 // constants
                 | "undefined"        -> UNDEFINED :: scan remaining
                 | "infinity" | "inf" -> INFINITY  :: scan remaining
                 | "pi"               -> PI        :: scan remaining
                 | "tau"              -> TAU       :: scan remaining
                 | _ ->
                    match remaining with
                     | '(' :: tail -> tokenType :: L_PARENTHESIS :: scan tail // parenthesis token separated for better parser context
                     | _           -> tokenType :: scan remaining
                    
             // numbers
             | c :: tail when isDigit c ->
                let digits, remaining = consume isDigit ( c :: tail )
                match remaining with
                 | '.' :: tail ->
                    let (fractionalDigits: char list), (remaining: char list) = consume isDigit ( c :: tail )
                    let number = charsToString ( digits @ ['.'] @ fractionalDigits )
                    let result = parseNumber number
                    NUMBER result :: scan remaining
                 | _ ->
                    let number = charsToString digits
                    let result = parseNumber number
                    NUMBER result :: scan remaining
            
             // operators and symbols
             | '=' :: tail -> EQUALS        :: scan tail
             | '(' :: tail -> L_PARENTHESIS :: scan tail
             | ')' :: tail -> R_PARENTHESIS :: scan tail
             | '^' :: tail -> EXPONENT      :: scan tail
             | '*' :: tail -> MULTIPLY      :: scan tail
             | '/' :: tail -> DIVIDE        :: scan tail
             | '%' :: tail -> PERCENTAGE    :: scan tail
             | '+' :: tail -> PLUS          :: scan tail
             | '-' :: tail -> SUBTRACT      :: scan tail
             | '|' :: tail -> BAR           :: scan tail
                
             // Other cases
             | c :: tail when isWhitespace c -> scan tail
             | _ -> raise ( System.Exception("Lexer error") )
            
        scan ( Seq.toList src )

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
    let parse(tokens: Lexer.Token list): ASTNode =
        {nodeType=NUMBER 0; children=[]}

