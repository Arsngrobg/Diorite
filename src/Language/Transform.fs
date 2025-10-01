// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Transform.fs
// Summary: The functions for transforming diorite source files into ASTs and/or token streams
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Language.Transform

/// <summary>
///     The <c>Lexer</c> module.
///     All related functionality for tokenizing
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
        // builtin functions
        | BUILTIN
        // set hints
        | SET_HINT
        | NUMBER_SET

    /// <summary>
    ///     The <c>lexer</c> module.
    ///     All related functionality for converting a raw string into a stream of tokens.
    /// </summary>
    /// <param name="src">the raw string to be tokenized</param>
    let lex(src: string): list<Token> =
        []

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
