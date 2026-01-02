// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    ComparisonOperationRules.fs
// Summary: The implementations of the ComparisonOperationRule
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors
open Diorite.Lang.Core.Runtime.OperationRuleHelpers

/// <summary>
///     <p>The <c>ComparisonOperationRules</c> module groups up the implementations of
///        <c>ComparisonOperationRule</c>s.
///     </p>
/// </summary>
module ComparisonOperationRules =
    /// <summary>
    ///     <p>Produces a <c>MathError</c> that has the appropriate error message for the given
    ///        <c>ComparisonOperator</c>, and pair of <c>ValueType</c>s.
    ///     </p>
    /// </summary>
    /// <param name="operator"> the operator the failure was caused by </param>
    /// <param name="a"> the first value in the pair that may have caused the error </param>
    /// <param name="b"> the second value in the pair that may have caused the error </param>
    /// <returns> a <c>MathError</c> with the appropriate error message, given the supplied data </returns>
    let UnsupportedComparison (operator: ComparisonOperator) (a: ValueType, b: ValueType): Result<bool> =
        (Some $"Unsupported {operator} comparison operation between {a} & {b}", [a; b]) ||> MathError

    /// <summary>
    ///     <p>The rule for equality comparison.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let EqualityComparisonRule: ComparisonOperationRule = fun ab ->
        match (Upcast ab) with
         | ValueType.Complex (a, b), ValueType.Complex (c, d) -> (a = c && b = d) |> Ok
         | ValueType.Number   a,     ValueType.Number   b     -> (a = b)          |> Ok
         | ValueType.Undefined,      ValueType.Undefined      -> true             |> Ok
         | _,                        _                        -> false            |> Ok

    /// <summary>
    ///     <p>The rule for inequality comparison.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let InequalityComparisonRule: ComparisonOperationRule = fun ab ->
        (ab |> EqualityComparisonRule) |> Result.bind (not >> Ok)

    /// <summary>
    ///     <p>The rule for strict-less-than comparison.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let StrictLessThanComparisonRule: ComparisonOperationRule = fun ab ->
        let unsupported: ComparisonOperationRule = UnsupportedComparison ComparisonOperator.StrictLessThan
        match ab with
         | ValueType.Number a, ValueType.Number b -> (a < b) |> Ok
         | a,                  b                  -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for strict-greater-than comparison.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let StrictGreaterThanComparisonRule: ComparisonOperationRule = fun ab ->
        let unsupported: ComparisonOperationRule = UnsupportedComparison ComparisonOperator.StrictGreaterThan
        match ab with
         | ValueType.Number a, ValueType.Number b -> (a > b) |> Ok
         | a,                  b                  -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for non-strict-less-than comparison.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let NonStrictLessThanComparisonRule: ComparisonOperationRule = fun ab ->
        let unsupported: ComparisonOperationRule = UnsupportedComparison ComparisonOperator.NonStrictLessThan
        match ab with
         | ValueType.Number a, ValueType.Number b -> (a >= b) |> Ok
         | a,                  b                  -> unsupported (a, b)

    /// <summary>
    ///     <p>The rule for non-strict-greater-than comparison.</p>
    /// </summary>
    /// <param name="ab"> the operands </param>
    let NonStrictGreaterThanComparisonRule: ComparisonOperationRule = fun ab ->
        let unsupported: ComparisonOperationRule = UnsupportedComparison ComparisonOperator.NonStrictGreaterThan
        match ab with
         | ValueType.Number a, ValueType.Number b -> (a >= b) |> Ok
         | a,                  b                  -> unsupported (a, b)
