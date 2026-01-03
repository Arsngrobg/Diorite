// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    BinaryOperationRules.fs
// Summary: The implementations of the BinaryOperationRule
// Author:  Arsngrobg
// Version: v1.4
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors
open Diorite.Lang.Core.Runtime.OperationRuleHelpers

/// <summary>
///     <p>The <c>BinaryOperationRules</c> module groups up the implementations of <c>BinaryOperationRule</c>s.</p>
/// </summary>
module BinaryOperationRules =
    /// <summary>
    ///     <p>Produces a <c>MathError</c> that has the appropriate error message for the given
    ///        <c>BinaryOperator</c>, and pair of <c>ValueType</c>s.
    ///     </p>
    /// </summary>
    /// <param name="operator"> the operator the failure was caused by </param>
    /// <param name="a"> the first value in the pair that may have caused the error </param>
    /// <param name="b"> the second value in the pair that may have caused the error </param>
    /// <returns> a <c>MathError</c> with the appropriate error message, given the supplied data </returns>
    let UnsupportedBinaryOperation (operator: BinaryOperator) (a: ValueType, b: ValueType): ValueType Result =
        (Some $"Unsupported binary {operator} operation between {a} & {b}", [a; b]) ||> MathError

    /// <summary>
    ///     <p>The rule for binary addition.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let BinaryAdditionRule: BinaryOperationRule = fun ab ->
        let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Addition
        match (Upcast ab) with
         | ValueType.Complex (a, b), ValueType.Complex (c, d) -> ValueType.Complex (a + c,  b + d) |> Ok
         | ValueType.Number   a,     ValueType.Number   b     -> MaybeNaN          (  a   +   b  ) |> Ok
         | a,                        b                        -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for binary subtraction.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let BinarySubtractionRule: BinaryOperationRule = fun ab ->
        let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Subtraction
        match (Upcast ab) with
         | ValueType.Complex (a, b), ValueType.Complex (c, d) -> ValueType.Complex (a - c,  b - d) |> Ok
         | ValueType.Number   a,     ValueType.Number   b     -> MaybeNaN          (  a   -   b  ) |> Ok
         | a,                        b                        -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for binary multiplication.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let BinaryMultiplicationRule: BinaryOperationRule = fun ab ->
        let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Multiplication
        match (Upcast ab) with
         | ValueType.Complex (a, b), ValueType.Complex (c, d) -> ValueType.Complex (a*c - b*d,  a*d + b*c) |> Ok
         | ValueType.Number   a,     ValueType.Number   b     -> MaybeNaN          (    a     *     b    ) |> Ok
         | a,                        b                        -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for binary division.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let BinaryDivisionRule: BinaryOperationRule = fun ab ->
        let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Division
        match (Upcast ab) with
         | ValueType.Complex (a, b), ValueType.Complex (c, d) ->
             let denominator: float = c**2 + d**2
             if denominator = 0 then
                 (Some "Division by zero", [ValueType.Complex (a, b); ValueType.Complex (c, d)])
                 ||> MathError
             else
                 let a: float = (a*c + b*d) / denominator
                 let b: float = (b*c - a*d) / denominator
                 ValueType.Complex (a, b) |> Ok
         | ValueType.Number   a,     ValueType.Number   b     ->
             if   b = 0 then (Some "Division by zero", [ValueType.Number a; ValueType.Number b]) ||> MathError
             else MaybeNaN (a / b) |> Ok
         | a,                        b                        -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for binary modulo.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let BinaryModuloRule: BinaryOperationRule = fun ab ->
        let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Modulo
        match ab with
         | ValueType.Number a, ValueType.Number b -> MaybeNaN (a % b) |> Ok
         | a,                  b                  -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for binary floor division.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let BinaryFloorDivisionRule: BinaryOperationRule = fun ab ->
        let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.FloorDivision
        match ab with
         | ValueType.Number a, ValueType.Number b ->
             if b = 0 then (Some "Division by zero", [ValueType.Number a; ValueType.Number b]) ||> MathError
             else          (a / b) |> (System.Math.Floor >> MaybeNaN >> Ok)
         | a,                  b                  -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for binary exponent.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let BinaryExponentRule: BinaryOperationRule = fun ab ->
        let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Exponent
        match (Upcast ab) with
         | ValueType.Complex (a, b), ValueType.Complex (c, d) ->
             // Diorite uses the principle value of exponents between two complex numbers
             //   z^w    = (r^c)(e^-d*theta) * (cos(c*theta + d*ln(r)) + i*sin(c*theta + d*ln(r)))
             //  theta   = arctan(b/a)
             //    r     = sqrt(a^2 + b^2)
             //  |z^w|   = (r^c)(e^-d*theta)
             // arg(z^w) = c*theta + d*ln(r)
             //   z^w    = |z^w| * (cos(arg(z^w)) + i*sin(arg(z^w))

             let theta:    float = System.Math.Atan2 (b, a) // ensures between [-pi ... pi] (principle)
             let r:        float = System.Math.Sqrt (a**2 + b**2)
             let magZPwrW: float = (r**c) * (System.Math.E**(-d*theta))
             let argZPwrW: float = c*theta + d*(System.Math.Log r)

             let reZW: float = magZPwrW * (System.Math.Cos argZPwrW)
             let imZW: float = magZPwrW * (System.Math.Sin argZPwrW)
             ValueType.Complex (reZW, imZW) |> Ok
         | ValueType.Number   a,     ValueType.Number   b     -> MaybeNaN (a ** b) |> Ok
         | a,                        b                        -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for binary complex constructor.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let BinaryOfComplexRule: BinaryOperationRule = fun ab ->
        let unsupported (offender: ValueType) (first: bool): ValueType Result =
            let meta: string = if first then "a term" else "b coefficient"
            let msg:  string = $"Complex constructor requires a pair of reals - offending argument is the {meta}"
            (Some msg, [offender]) ||> MathError

        match ab with
         | ValueType.Number a, ValueType.Number b -> ValueType.Complex (a, b) |> Ok
         | ValueType.Number _, b                  -> unsupported b false
         | a,                  _                  -> unsupported a true
