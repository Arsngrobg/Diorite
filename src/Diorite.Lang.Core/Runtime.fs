// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Runtime.fs
// Summary: Contains the live interpreter, and the virtual memory manager for the Diorite language
// Author:  Arsngrobg, Borngle
// Version: v1.11
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The <c>Runtime</c> module contains the live interpreter, and the virtual memory manager for the
///        <b>Diorite</b> mathematics processing language.
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
module Runtime =
    /// <summary>
    ///     <p>Defines a function that accepts a pair of values that may produce a <c>ValueType</c> or an error.</p>
    /// </summary>
    type BinaryOperationRule     = ValueType * ValueType -> ValueType Result
    /// <summary>
    ///     <p>Defines a function that accepts a single value that may produce a <c>ValueType</c> or an error.</p>
    /// </summary>
    type UnaryOperationRule      = ValueType             -> ValueType Result
    /// <summary>
    ///     <p>Defines a function that accepts a pair of values that may produce a <c>bool</c> or an error.</p>
    /// </summary>
    type ComparisonOperationRule = ValueType * ValueType -> bool      Result

    /// <summary>
    ///     <p>Upcasts the pair of values into equivalent types for relevant operations on equal types.</p>
    /// </summary>
    /// <param name="a"> the left-hand-side value </param>
    /// <param name="b"> the right-hand-side value </param>
    /// <returns> <c>a</c> &amp; <c>b</c>, as equal types, if possible </returns>
    let Upcast (a: ValueType, b: ValueType): ValueType * ValueType =
        match (a, b) with
         | ValueType.Complex (a, b), ValueType.Number  c      -> (ValueType.Complex (a, b), ValueType.Complex (c, 0))
         | ValueType.Number  a,      ValueType.Complex (b, c) -> (ValueType.Complex (a, 0), ValueType.Complex (b, c))
         | a,                        b                        -> (a,                        b                       )

    /// <summary>
    ///     <p>The <c>BinaryOperationRules</c> module groups up the implementations of <c>BinaryOperationRule</c>s.</p>
    /// </summary>
    [<AutoOpen>]
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
            (Some $"Unsupported binary {operator} operation between {a} & {b}", a) ||> MathError

        /// <summary>
        ///     <p>The rule for binary addition.</p>
        /// </summary>
        /// <param name="ab"> the operands </param>
        let BinaryAdditionRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Addition
            match (Upcast ab) with
             | ValueType.Complex (a, b), ValueType.Complex (c, d) -> ValueType.Complex (a + b,  c + d) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> ValueType.Number  (  a   +   b  ) |> Ok
             | a,                        b                        -> unsupported (a, b)

        /// <summary>
        ///     <p>The rule for binary subtraction.</p>
        /// </summary>
        /// <param name="ab"> the operands </param>
        let BinarySubtractionRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Subtraction
            match (Upcast ab) with
             | ValueType.Complex (a, b), ValueType.Complex (c, d) -> ValueType.Complex (a - b,  c - d) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> ValueType.Number  (  a   +   b  ) |> Ok
             | a,                        b                        -> unsupported (a, b)

        /// <summary>
        ///     <p>The rule for binary multiplication.</p>
        /// </summary>
        /// <param name="ab"> the operands </param>
        let BinaryMultiplicationRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Multiplication
            match (Upcast ab) with
             | ValueType.Complex (a, b), ValueType.Complex (c, d) -> ValueType.Complex (a*c - b*d,  a*d + b*c) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> ValueType.Number  (    a     *     b    ) |> Ok
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
                 if denominator = 0 then (Some "Division by zero", ValueType.Number denominator) ||> MathError
                 else
                     let a: float = (a*c + b*d) / denominator
                     let b: float = (b*c - a*d) / denominator
                     ValueType.Complex (a, b) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     ->
                 if   b = 0 then (Some "Division by zero", ValueType.Number b) ||> MathError
                 else ValueType.Number (a / b) |> Ok
             | a,                        b                        -> unsupported (a, b)

        /// <summary>
        ///     <p>The rule for binary modulo.</p>
        /// </summary>
        /// <param name="ab"> the operands </param>
        let BinaryModuloRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Modulo
            match ab with
             | ValueType.Number a, ValueType.Number b -> ValueType.Number (a % b) |> Ok
             | a,                  b                  -> unsupported (a, b)

        /// <summary>
        ///     <p>The rule for binary floor division.</p>
        /// </summary>
        /// <param name="ab"> the operands </param>
        let BinaryFloorDivisionRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.FloorDivision
            match ab with
             | ValueType.Number a, ValueType.Number b ->
                 if b = 0 then (Some "Division by zero", ValueType.Number b) ||> MathError
                 else          (a / b) |> (System.Math.Floor >> ValueType.Number >> Ok)
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

                 let theta:    float = System.Math.Atan2 (b, a)
                 let r:        float = System.Math.Sqrt (a**2 + b**2)
                 let magZPwrW: float = (r**c) * (System.Math.E**(-d*theta))
                 let argZPwrW: float = c*theta + d*(System.Math.Log r)

                 let reZW: float = magZPwrW * (System.Math.Cos argZPwrW)
                 let imZW: float = magZPwrW * (System.Math.Sin argZPwrW)
                 ValueType.Complex (reZW, imZW) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> ValueType.Number (a ** b) |> Ok
             | a,                        b                        -> unsupported (a, b)

        /// <summary>
        ///     <p>The rule for binary complex constructor.</p>
        /// </summary>
        /// <param name="ab"> the operands </param>
        let BinaryOfComplexRule: BinaryOperationRule = fun ab ->
            let unsupported (offender: ValueType) (first: bool): ValueType Result =
                let meta: string = if first then "a term" else "b coefficient"
                let msg:  string = $"Complex constructor requires a pair of reals - offending argument is the {meta}"
                (Some msg, offender) ||> MathError

            match ab with
             | ValueType.Number a, ValueType.Number b -> ValueType.Complex (a, b) |> Ok
             | ValueType.Number _, b                  -> unsupported b false
             | a,                  _                  -> unsupported a true

    /// <summary>
    ///     <p>The <c>UnaryOperationRules</c> module groups up the implementations of <c>UnaryOperationRule</c>s.</p>
    /// </summary>
    [<AutoOpen>]
    module UnaryOperationRules =
        /// <summary>
        ///     <p>Produces a <c>MathError</c> that has the appropriate error message for the given
        ///        <c>UnaryOperator</c>, and <c>ValueType</c>.
        ///     </p>
        /// </summary>
        /// <param name="operator"> the operator the failure was caused by </param>
        /// <param name="a"> the value that may have caused the error </param>
        /// <returns> a <c>MathError</c> with the appropriate error message, given the supplied data </returns>
        let UnsupportedUnaryOperation (operator: UnaryOperator) (a: ValueType): ValueType Result =
            (Some $"Unsupported unary {operator} operation for {a}", a) ||> MathError

        /// <summary>
        ///     <p>The rule for unary positive.</p>
        /// </summary>
        /// <param name="a"> the operand </param>
        let UnaryPositiveRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Positive
            match a with
             | ValueType.Complex (a, b) -> ValueType.Complex (a, b) |> Ok
             | ValueType.Number   a     -> ValueType.Number  a      |> Ok
             | a                        -> unsupported a

        /// <summary>
        ///     <p>The rule for unary negative.</p>
        /// </summary>
        /// <param name="a"> the operand </param>
        let UnaryNegativeRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Negative
            match a with
             | ValueType.Complex (a, b) -> ValueType.Complex (-a, -b) |> Ok
             | ValueType.Number   a     -> ValueType.Number   -a      |> Ok
             | a                        -> unsupported a

        /// <summary>
        ///     <p>The rule for unary factorial.</p>
        /// </summary>
        /// <param name="a"> the operand </param>
        let rec UnaryFactorialRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Factorial
            match a with
             | ValueType.Number   a     ->
                 if   (a |> System.Math.Truncate) <> a then ValueType.Undefined |> Ok
                 elif  a < 0                           then ValueType.Undefined |> Ok
                 elif  a < 2                           then ValueType.Number 1  |> Ok
                 else (ValueType.Number (a - 1.0) |> UnaryFactorialRule) ?=> (fun b ->
                          (ValueType.Number a, b) |> BinaryMultiplicationRule
                      )
             | a                        -> unsupported a

        /// <summary>
        ///     <p>The rule for unary absolute.</p>
        /// </summary>
        /// <param name="a"> the operand </param>
        let UnaryAbsoluteRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Absolute
            match a with
             | ValueType.Complex (a, b) -> ValueType.Number ((a**2 + b**2) |> System.Math.Sqrt) |> Ok
             | ValueType.Number   a     -> ValueType.Number (a             |> System.Math.Abs ) |> Ok
             | a                        -> unsupported a

        /// <summary>
        ///     <p>The rule for unary complex constructor.</p>
        /// </summary>
        /// <param name="a"> the operand </param>
        let UnaryGetImaginaryRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.GetImaginary
            match a with
             | ValueType.Number   _     -> ValueType.Number 0 |> Ok
             | a                        -> unsupported a

        /// <summary>
        ///     <p>The rule for unary <c>re(z)</c>.</p>
        /// </summary>
        /// <param name="a"> the operand </param>
        let UnaryGetRealRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.GetReal
            match a with
             | ValueType.Complex (a, _) -> ValueType.Number a |> Ok
             | ValueType.Number   a     -> ValueType.Number a |> Ok
             | a                        -> unsupported a

    /// <summary>
    ///     <p>The <c>ComparisonOperationRules</c> module groups up the implementations of
    ///        <c>ComparisonOperationRule</c>s.
    ///     </p>
    /// </summary>
    [<AutoOpen>]
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
            (Some $"Unsupported {operator} comparison operation between {a} & {b}", a) ||> MathError

        /// <summary>
        ///     <p>The rule for equality comparison.</p>
        /// </summary>
        /// <param name="ab"> the operands </param>
        let EqualityComparisonRule: ComparisonOperationRule = fun ab ->
            match ab with
             | ValueType.Complex (a, b), ValueType.Complex (c, d) -> (a = c && b = d) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> (a = b)          |> Ok
             | ValueType.Undefined,      ValueType.Undefined      -> true             |> Ok
             | _,                        _                        -> false            |> Ok

        /// <summary>
        ///     <p>The rule for inequality comparison.</p>
        /// </summary>
        /// <param name="ab"> the operands </param>
        let InequalityComparisonRule: ComparisonOperationRule = fun ab ->
            (ab |> EqualityComparisonRule) ?=> (not >> Ok)

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
