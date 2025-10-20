// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    LexerTypes.fs
// Summary: The types used by the Lexer - in global namespace for the project
// Author:  Arsngrobg, Borngle
// Version: v1.4
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     All the valid tokens that can be accepted in the <b>Diorite</b> language.
///     <c>IllegalToken</c> is used to determine errors in source files / input.
/// </summary>
type Token =
    // lexing continues upon discovering an IllegalToken as it helps with finding all illegal tokens
    | IllegalToken      of string            // contains the offending lexeme

    // value types
    | Number            of float             // contains the number literal
    | Identifier        of char * int option // contains the character + optional subscript
    | Symbol            of string            // contains the symbol name

    // reserved words
    | Undefined
    | Infinity

    // symbolic constants
    | Pi
    | Tau
    | Euler

    // integral operator
    | Tick

    // boundary operators
    | Colon
    | Arrow

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
    | Percentage
    | Plus
    | Hyphen
    | Bar

    // control flow
    | If
    | Otherwise

    // parenthesis, brackets, and braces
    | LeftParenthesis
    | RightParenthesis
    | LeftBracket
    | RightBracket
    | LeftBrace
    | RightBrace

/// <summary>
///     A descriptive wrapper for a <c>Token</c> list.
/// </summary>
type TokenStream = Token list
