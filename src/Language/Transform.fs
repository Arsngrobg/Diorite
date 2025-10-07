// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Transform.fs
// Summary: The functions for transforming diorite source files into ASTs and/or token streams
// Author:  Arsngrobg, Borngle
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Language.Transform

/// <summary>
///     The <c>Lexer</c> module.
///     All related functionality for tokenizing <c>Diorite</c> source code.
/// </summary>
module Lexer =
    /// <summary>The token types that are recognised by this lexer.</summary>
    type Token =
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
        // builtin functions
        | BUILTIN of string
        // set hints
        | SET_HINT
        | NUMBER_SET
        
    /// <summary> Helper function to evaluate if a character is alphabetical </summary>
    /// <param name="c"> Input character </param>
    let isAlpha c = System.Char.IsLetter c
    
    /// <summary> Helper function to evaluate if a character is a digit </summary>
    /// <param name="c"> Input character </param>
    let isDigit c = System.Char.IsDigit c
    
    /// <summary> Recursively consumes characters if they satisfy a given predicate </summary>
    /// <param name="predicate"> Function that returns true or false </param>
    /// <param name="src"> List of characters being checked </param>
    let rec consume predicate src =
        match src with
        | c::tail when predicate(c) -> // Match if true
            let consumed, remaining = consume predicate tail
            (c::consumed, remaining) // c is prepended to consumed, and remaining is the rest of the list that does not match
        | _ -> ([], src)

    /// <summary>
    ///     The <c>lexer</c> module.
    ///     All related functionality for converting a raw string into a stream of tokens.
    /// </summary>
    /// <param name="src">the raw string to be tokenized</param>
    let lex(src: string): list<Token> =
        let rec scan src =
            match src with
            | [] -> []
            
            // Sets
            | ':':: tail ->
                let consumed, remaining = consume System.Char.IsWhiteSpace tail
                match remaining with
                | 'N' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'Z' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'Q' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'I' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'R' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'C' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | _ -> raise (System.Exception("Lexer error"))
            | '-' :: '>':: tail ->
                let consumed, remaining = consume System.Char.IsWhiteSpace tail
                match remaining with
                | 'N' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'Z' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'Q' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'I' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'R' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | 'C' :: tail -> SET_HINT :: NUMBER_SET :: scan tail
                | _ -> raise (System.Exception("Lexer error"))

            // Variables, built-in functions, and constants
            | c::tail when isAlpha(c) ->
                let letters, remaining = consume isAlpha (c::tail)
                let name = System.String.Concat letters
                match name with
                | "undefined" -> UNDEFINED :: scan remaining
                | "infinity" -> INFINITY :: scan remaining
                | "pi" -> PI :: scan remaining
                | "tau" -> TAU :: scan remaining
                | _ ->
                    match remaining with
                    | '('::tail -> BUILTIN(name) :: L_PARENTHESIS :: scan tail // Parenthesis token separated for better parser context 
                    | _ -> VARIABLE(name) :: scan remaining
                    
            // Numbers
            | c::tail when isDigit(c) ->
                let digits, remaining = consume isDigit (c::tail)
                match remaining with
                | '.'::tail ->
                    let fractionalDigits, remaining = consume isDigit (c::tail)
                    let number = System.String.Concat(digits @ ['.'] @ fractionalDigits)
                    let result = System.Double.Parse(number)
                    NUMBER(result) :: scan remaining
                | _ ->
                    let number = System.String.Concat(digits)
                    let result = System.Double.Parse(number)
                    NUMBER(result) :: scan remaining
            
            // Operators and symbols
            | '='::tail -> EQUALS :: scan tail
            | '('::tail -> L_PARENTHESIS :: scan tail
            | ')'::tail -> R_PARENTHESIS :: scan tail
            | '^'::tail -> EXPONENT :: scan tail
            | '*'::tail -> MULTIPLY :: scan tail
            | '/'::tail -> DIVIDE :: scan tail
            | '%'::tail -> PERCENTAGE :: scan tail
            | '+'::tail -> PLUS :: scan tail
            | '-'::tail -> SUBTRACT :: scan tail
            | '|'::tail -> BAR :: scan tail
                
            // Other cases
            | c :: tail when System.Char.IsWhiteSpace c -> scan tail
            | _ -> raise (System.Exception("Lexer error"))
            
        scan(Seq.toList src)

/// <summary>
///     The <c>Parser</c> module.
///     All related functionality for parsing a provided token stream.
/// </summary>
module Parser =
    /// <summary>The node types for the AST.</summary>
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

    /// <summary>A node in an Abstract Syntax Tree (AST) of the grammar.</summary>
    /// <param name="nodeType">the type of <c>ASTNode</c></param>
    /// <param name="children">the children of this <c>ASTNode</c> - can be zero</param>
    [<Struct>]
    type ASTNode = {
        nodeType: NodeType
        // 0 children = leaf node (number, variable)
        // 1 child    = unary operation
        // 2 children = binary operation
        // x children = function arguments
        children: list<Lexer.Token>
    }

    /// <summary>Parses the supplied list of <c>tokens</c> into a formatted AST.</summary>
    /// <param name="tokens">the token stream to parse into an AST</param>
    let parse(tokens: list<Lexer.Token>): ASTNode =
        {nodeType=NUMBER 0; children=[]}
        