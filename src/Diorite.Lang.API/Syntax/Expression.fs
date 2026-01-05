// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Expression.fs
// Summary: The type definition for the Expression type, which is a recursive structure for a mathematical expression
//          in Diorite
// Author:  Arsngrobg
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax

/// <summary>
///     <p>The structured representation of an expression in the <b>Diorite</b> language.</p>
///     <p>It is a smaller component of the <c>AST</c> and is defined as such since it narrows the grammar through
///        the F# type system.
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
type Expression =
    /// <summary>
    ///     <p>An atomic unit for an expression.</p>
    ///     <p>Represents a <c>ValueType</c>.</p>
    /// </summary>
    | Value           of ValueType
    /// <summary>
    ///     <p>An atomic unit for an expression.</p>
    ///     <p>Represents a reference to <c>ValueType</c> which the value is obtained during evaluation.</p>
    /// </summary>
    | Variable        of VariableType
    /// <summary>
    ///     <p>A structured representation of a binary operation in <b>Diorite</b>.</p>
    ///     <p>It is a tuple which has the left and right sub expressions and its <c>BinaryOperator</c>.</p>
    /// </summary>
    | BinaryOperation of Expression * BinaryOperator * Expression
    /// <summary>
    ///     <p>A structured representation of a unary operation in <b>Diorite</b>.</p>
    ///     <p>It is a tuple which has the operand and its <c>UnaryOperator</c>.</p>
    /// </summary>
    | UnaryOperation  of Expression * UnaryOperator
    /// <summary>
    ///     <p>A structured representation of a function call in <b>Diorite</b>.</p>
    ///     <p>It is a tuple which has the <c>FunctionReferenceType</c>, and its argument list.</p>
    /// </summary>
    | FunctionCall    of FunctionReferenceType * Expression list