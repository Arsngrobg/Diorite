// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    BinaryOperator.fs
// Summary: The type definition for the BinaryOperator in Diorite, an operation applied on a pair ValueTypes 
// Author:  Arsngrobg
// Version: v1.4
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax

/// <summary>
///     <p>A <c>BinaryOperator</c> is an operator that executes on two operands on either side (hence binary).</p>
///     <p><i>Literal pattern: <c>a [BinaryOperator] b</c></i></p>
/// </summary>
[<RequireQualifiedAccess>]
type BinaryOperator =
    /// <summary>
    ///     <p>The binary operator for addition (<c>a + b</c>).</p>
    ///     <p><i>associativity: left associative</i></p>
    /// </summary>
    | Addition
    /// <summary>
    ///     <p>The binary operator for subtraction (<c>a - b</c>).</p>
    ///     <p><i>associativity: left associative</i></p>
    /// </summary>
    | Subtraction
    /// <summary>
    ///     <p>The binary operator for multiplication (<c>a * b</c>).</p>
    ///     <p><i>associativity: left associative</i></p>
    /// </summary>
    | Multiplication
    /// <summary>
    ///     <p>The binary operator for division (<c>a / b</c>).</p>
    ///     <p><i>associativity: left associative</i></p>
    /// </summary>
    | Division
    /// <summary>
    ///     <p>The binary operator for modulo (<c>a % b</c>).</p>
    ///     <p><i>associativity: left associative</i></p>
    /// </summary>
    | Modulo
    /// <summary>
    ///     <p>The binary operator for floor division (<c>a // b</c>).</p>
    ///     <p><i>associativity: left associative</i></p>
    /// </summary>
    | FloorDivision
    /// <summary>
    ///     <p>The binary operator for exponent (<c>a ^ b</c>).</p>
    ///     <p><i>associativity: right associative</i></p>
    /// </summary>
    | Exponent
    /// <summary>
    ///     <p>The binary operator for declaring a complex value (<c>a + bi</c>).</p>
    ///     <p><i>binds: N/A</i></p>
    /// </summary>
    | OfComplex
