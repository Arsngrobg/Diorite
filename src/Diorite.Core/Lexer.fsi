// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Lexer.fsi
// Summary: Public interface definitions for the Diorite language lexer
// Author:  Arsngrobg, Borngle
// Version: v1.6
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The <c>Lexer</c> module contains bindings related to tokenizing raw strings into <b>Diorite</b> tokens.</p>
/// </summary>
module Lexer =
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
    ///     <p>It is composed of: the <c>lexeme</c>, which is the reference to the <c>string</c> that this
    ///        <c>Token</c> represents; the <c>id</c>, which denotes the union type of this <c>Token</c>; the
    ///        <c>line</c>, the line of the respective source file this token is located; the <c>column</c>, the column
    ///        of the respective source file this token is located.
    ///     </p>
    /// </summary>
    [<Struct>]
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

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>Token</c>.</p>
    /// </summary>
    /// <typeparam name='Token'> the token </typeparam>
    /// <returns> the <c>string</c> representation of the supplied <c>Token</c> </returns>
    val strToken: Token -> string

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>TokenStream</c>.</p>
    /// </summary>
    /// <typeparam name='TokenStream'> the token stream </typeparam>
    /// <returns> the <c>string</c> representation of the supplied <c>TokenStream</c> </returns>
    val strTokens: TokenStream -> string

    /// <summary>
    ///     <p>Searches through the supplied <c>TokenStream</c> in order until it reaches an <c>IllegalToken</c>.</p>
    ///     <p>If it does reach an <c>IllegalToken</c>, the function will return a <c>SyntaxError</c> containing a
    ///        message which states what the illegal token is.
    ///     </p>
    /// </summary>
    /// <typeparam name='TokenStream'> the tokens to search for an <c>IllegalToken</c> </typeparam>
    /// <returns> maybe a <c>DioriteError</c> (<c>SyntaxError</c>) </returns>
    val getError: TokenStream -> DioriteError option

    /// <summary>
    ///     <p>Tokenizes the supplied <c>string</c> into a sequence of <c>Token</c>s (<c>TokenStream</c>).</p>
    /// </summary>
    /// <typeparam name='string'> the raw string to tokenize </typeparam>
    val tokenize: string -> TokenStream
