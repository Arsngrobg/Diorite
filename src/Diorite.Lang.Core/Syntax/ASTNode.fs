// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    ASTNode.fs
// Summary: The type definition for the ASTNode type, which is the sub-root of the top-most tree structure of parsed
//          Diorite source code
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>The <c>AST</c> is the tree structure of a top-level <b>Diorite</b> statement.</p>
/// </summary>
[<RequireQualifiedAccess>]
type ASTNode =
    /// <summary>
    ///     <p>A statement that is only an expression.</p>
    ///     <p><i>Example</i>: <c>2 + 3;</c></p>
    /// </summary>
    | Expression         of Expression
    /// <summary>
    ///     <p>A statement that requests the plot of a function (via a <c>FunctionReferenceType</c>), or an
    ///        anonymous function.
    ///     </p>
    ///     <p><i>Example: <c>plot f; # f(x)=2*x</c></i> OR <c>plot (2*x)</c></p>
    /// </summary>
    | PlotFunction       of Expression
    /// <summary>
    ///     <p>A statement that assigns an <c>Expression</c> on the right-hand side to a variable on the
    ///        left-hand side.
    ///     </p>
    ///     <p><i>Example: <c>y = 100;</c></i></p>
    /// </summary>
    | Assignment         of VariableType * Expression
    /// <summary>
    ///     <p>A function definition.
    ///        Either composed of a single <c>Expression</c>, or a series of <c>PiecewiseOperation</c>s.
    ///     </p>
    ///     <p><i>Example: <c>f(x) = 2*x;</c> OR <c>f(x) = { -x if x &lt; 0; x otherwise; }</c></i></p>
    /// </summary>
    | FunctionDefinition of FunctionType
