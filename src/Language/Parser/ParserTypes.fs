// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    ParserTypes.fs
// Summary: The types used by the parser - in global namespace for the project
// Author:  Arsngrobg, Borngle
// Version: v1.8
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The number sets supported by the <c>SetHint</c> feature.
/// </summary>
type NumberSet =
    | Natural    // N = {1, ..., ∞}
    | Integer    // Z = {-∞, ..., 0, ..., ∞}
    | Real       // R = {Q & I}
    | Rational   // Q = {x where x = a/b & b != 0}
    | Irrational // I = {x where x != a/b}
    | Complex    // C = {x where x = a + bi}

/// <summary>
///     The linkage type of functions.
///     <c>Internal</c> linkage means that it is locally defined within the source file.
///     <c>External</c> linkage means that it is defined elsewhere.
/// </summary>
type Linkage =
    | Internal of string
    | External of string

/// <summary>
///     The metadata for a function.
///     <c>symbol</c> is the optional string value that also represents this function.
///     <c>inlined</c> is a tuple of <c>bool</c>s where it is of the pattern: <c>enabled * forced</c>.
///     <c>memoized</c> is a tuple of <c>bool</c>s where it is of the pattern: <c>enabled * forced</c>.
/// </summary>
type FunctionMetadata = {
    symbol:   string      option // optional, meaningful name that persists throughout the entire program
    inlined:  bool * bool        // (enabled, forced)
    memoized: bool * bool        // (enabled, forced)
}

/// <summary>
///     The attributes of a function.
///     If no <c>NumberSet</c> is provided to a parameter or the return type - it defaults to <c>Real</c>.
/// </summary>
type FunctionAttributes = {
    identifier: char * int option                      // the variable name
    parameters: ((char * int option) * NumberSet) list // parameter name    - defaults: Real
    returns:    NumberSet                              // return 'type'     - default:  Real
    metadata:   FunctionMetadata
}

/// <summary>
///     The <c>ASTNode</c> type is a discriminated union which describes the structure of the AST of the <b>Diorite</b>
///     language.
/// </summary>
type ASTNode =
    // values
    | Number          of float
    | Identifier      of id: char * subscript: int option

    // reserved words
    | Undefined
    | Infinity

    // unary operations
    | Integration
    | Differentiation
    | Percentage
    | Positive
    | Negative

    // binary operations
    | Multiplication
    | Division
    | Modulo
    | Addition
    | Subtraction

    // comparison operations
    | Equals
    | NotEquals
    | GreaterThan
    | LessThan
    | GreaterThanOrEqual
    | LessThanOrEqual

    // structure
    | Begin            of ASTNode list
    | BinaryOperation  of left:       ASTNode            * operator:    ASTNode      * right: ASTNode
    | UnaryOperation   of operand:    ASTNode            * operator:    ASTNode
    | Comparison       of left:       ASTNode            * operator:    ASTNode      * right: ASTNode
    | Conditions       of cases:      ASTNode list       * defaultCase: ASTNode
    | FunctionDef      of data:       FunctionAttributes * body:        ASTNode
    | FunctionCall     of identifier: ASTNode            * arguments:   ASTNode list
