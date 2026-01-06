// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    DioriteUnaryOperator.cs
// Summary: The type definition for the UnaryOperator in Diorite, an operation applied on a single ValueType - it
//          maps to UnaryOperator in the core layer
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax;

/// <summary>
///     <p>A <c>DioriteUnaryOperator</c> is an operator that executes on a single operand on either side, depending on
///        which direction it binds.
///     </p>
///     <p><i>Literal pattern: <c>[UnaryOperator] a</c> | <c>a [UnaryOperator]</c></i></p>
/// </summary>
public enum DioriteUnaryOperator
{
    /// <summary>
    ///     <p>The unary operator for positive (<c>+a</c>) - the identity operator.</p>
    ///     <p><i>binds: to the right</i></p>
    /// </summary>
    Positive,
    /// <summary>
    ///     <p>The unary operator for negation (<c>-a</c>).</p>
    ///     <p><i>binds: to the right</i></p>
    /// </summary>
    Negative,
    /// <summary>
    ///     <p>The unary operator for factorial (<c>a!</c>).</p>
    ///     <p><i>binds: to the left</i></p>
    /// </summary>
    Factorial,
    /// <summary>
    ///     <p>The unary operator for absolute (<c>|a|</c>).</p>
    ///     <p><i>binds: N/A</i></p>
    /// </summary>
    Absolute,
    /// <summary>
    ///     <p>The unary operator for obtaining the imaginary component of a <c>Complex</c> value (<c>im b</c>).</p>
    ///     <p><i>binds: N/A</i></p>
    /// </summary>
    GetImaginary,
    /// <summary>
    ///     <p>The unary operator for obtaining the real component of a <c>Complex</c> value (<c>re a</c>).</p>
    ///     <p><i>binds: N/A</i></p>
    /// </summary>
    GetReal
}