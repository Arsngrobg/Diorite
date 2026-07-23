// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    BinaryOperationRules.fs
// Summary: The implementations of the UnaryOperationRule
// Author:  Arsngrobg
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors

open Diorite.Lang.Core.Runtime.BinaryOperationRules // for BinMul

/// <summary>
///     <p>The <c>UnaryOperationRules</c> module groups up the implementations of <c>UnaryOperationRule</c>s.</p>
/// </summary>
module UnaryOperationRules =
    /// <summary>
    ///     <p>This is the hard limit on the application of the <c>Factorial</c> operation in <b>Diorite</b>.
    ///        This is the value that, beyond this input argument, the value of <c>x!</c> is too large to be
    ///        represented by a 64-bit integer. Hence, it optimises the call as a value of
    ///        <c>inf</c>.
    ///     </p>
    ///     <p><i>This also prevents the <c>factorial</c> operation from easily exploding, which causes
    ///           <c>StackOverflowError</c>s.
    ///     </i></p>
    /// </summary>
    let FactorialLimit: int = 20

    /// <summary>
    ///     <p>Produces a <c>MathError</c> that has the appropriate error message for the given
    ///        <c>UnaryOperator</c>, and <c>ValueType</c>.
    ///     </p>
    /// </summary>
    /// <param name="operator"> the operator the failure was caused by </param>
    /// <param name="a"> the value that may have caused the error </param>
    /// <returns> a <c>MathError</c> with the appropriate error message, given the supplied data </returns>
    let UnsupportedUnaryOperation (operator: UnaryOperator) (a: ValueType): ValueType Result =
        (Some $"Unsupported unary {operator} operation for {a}", [a]) ||> MathError

    /// <summary>
    ///     <p>The rule for unary positive.</p>
    /// </summary>
    /// <param name="a"> the operand </param>
    let UnaryPositiveRule: UnaryOperationRule = fun a ->
        let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Positive
        match a with
         | ValueType.Complex (a, b) -> ValueType.Complex (a, b) |> Ok
         | ValueType.Float   a      -> ValueType.Float    a     |> Ok
         | ValueType.Integer   a    -> ValueType.Integer  a     |> Ok
         | a                        -> unsupported a

    /// <summary>
    ///     <p>The rule for unary negative.</p>
    /// </summary>
    /// <param name="a"> the operand </param>
    let UnaryNegativeRule: UnaryOperationRule = fun a ->
        let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Negative
        match a with
         | ValueType.Complex (a, b) -> ValueType.Complex (-a, -b) |> Ok
         | ValueType.Float   a      -> ValueType.Float    -a      |> Ok
         | ValueType.Integer   a    -> ValueType.Integer  -a      |> Ok
         | a                        -> unsupported a

    /// <summary>
    ///     <p>The rule for unary factorial.</p>
    /// </summary>
    /// <param name="a"> the operand </param>
    let rec UnaryFactorialRule: UnaryOperationRule = fun a ->
        let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Factorial
        match a with
         | ValueType.Integer   a     ->
             if    a > FactorialLimit              then ConstantInfinity    |> Ok // x! > Double.Max
             elif  a < 0                           then ValueType.Undefined |> Ok
             elif  a < 2                           then ValueType.Integer 1 |> Ok
             else (ValueType.Integer (a - 1L) |> UnaryFactorialRule) |> Result.bind (fun b ->
                      (ValueType.Integer a, b) |> BinaryMultiplicationRule
                  )
         | a                        -> unsupported a

    /// <summary>
    ///     <p>The rule for unary absolute.</p>
    /// </summary>
    /// <param name="a"> the operand </param>
    let UnaryAbsoluteRule: UnaryOperationRule = fun a ->
        let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Absolute
        match a with
         | ValueType.Complex (a, b) -> ValueType.Float ((a**2 + b**2) |> System.Math.Sqrt) |> Ok
         | ValueType.Integer a      -> ValueType.Integer (a           |> System.Math.Abs ) |> Ok
         | ValueType.Float   a      -> ValueType.Float   (a           |> System.Math.Abs ) |> Ok
         | a                        -> unsupported a

    /// <summary>
    ///     <p>The rule for unary complex constructor.</p>
    /// </summary>
    /// <param name="a"> the operand </param>
    let UnaryGetImaginaryRule: UnaryOperationRule = fun a ->
        let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.GetImaginary
        match a with
         | ValueType.Complex (_, b) -> ValueType.Float b |> Ok
         | ValueType.Float    _     -> ValueType.Float 0 |> Ok
         | a                        -> unsupported a

    /// <summary>
    ///     <p>The rule for unary <c>re(z)</c>.</p>
    /// </summary>
    /// <param name="a"> the operand </param>
    let UnaryGetRealRule: UnaryOperationRule = fun a ->
        let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.GetReal
        match a with
         | ValueType.Complex (a, _) -> ValueType.Float a |> Ok
         | ValueType.Float    a     -> ValueType.Float a |> Ok
         | a                        -> unsupported a

