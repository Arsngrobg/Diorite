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
    type BinaryOperationRule     = ValueType * ValueType -> Result<ValueType>
    /// <summary>
    ///     <p>Defines a function that accepts a single value that may produce a <c>ValueType</c> or an error.</p>
    /// </summary>
    type UnaryOperationRule      = ValueType             -> Result<ValueType>
    /// <summary>
    ///     <p>Defines a function that accepts a pair of values that may produce a <c>bool</c> or an error.</p>
    /// </summary>
    type ComparisonOperationRule = ValueType * ValueType -> Result<bool>

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

    [<AutoOpen>]
    module BinaryOperationRules =
        let UnsupportedBinaryOperation (operator: BinaryOperator) (a: ValueType, b: ValueType): Result<ValueType> =
            (Some $"Unsupported binary {operator} operation between {a} & {b}", a) ||> MathError

        let BinaryAdditionRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Addition
            match (Upcast ab) with
             | ValueType.Complex (a, b), ValueType.Complex (c, d) -> ValueType.Complex (a + b,  c + d) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> ValueType.Number  (  a   +   b  ) |> Ok
             | a,                        b                        -> unsupported (a, b)

        let BinarySubtractionRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Subtraction
            match (Upcast ab) with
             | ValueType.Complex (a, b), ValueType.Complex (c, d) -> ValueType.Complex (a - b,  c - d) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> ValueType.Number  (  a   +   b  ) |> Ok
             | a,                        b                        -> unsupported (a, b)

        let BinaryMultiplicationRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Multiplication
            match (Upcast ab) with
             | ValueType.Complex (a, b), ValueType.Complex (c, d) -> ValueType.Complex (a*c - b*d,  a*d + b*c) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> ValueType.Number  (    a     *     b    ) |> Ok
             | a,                        b                        -> unsupported (a, b)

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

        let BinaryModuloRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.Modulo
            match ab with
             | ValueType.Number a, ValueType.Number b -> ValueType.Number (a % b) |> Ok
             | a,                  b                  -> unsupported (a, b)

        let BinaryFloorDivisionRule: BinaryOperationRule = fun ab ->
            let unsupported: BinaryOperationRule = UnsupportedBinaryOperation BinaryOperator.FloorDivision
            match ab with
             | ValueType.Number a, ValueType.Number b ->
                 if b = 0 then (Some "Division by zero", ValueType.Number b) ||> MathError
                 else          (a / b) |> (System.Math.Floor >> ValueType.Number >> Ok)
             | a,                  b                  -> unsupported (a, b)

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

                 let theta:    float = if   a = 0 && b > 0 then   System.Math.PI / 2.0
                                       elif a = 0 && b < 0 then -(System.Math.PI / 2.0)
                                       else                       System.Math.Asin (b / a)
                 let r:        float = System.Math.Sqrt (a**2 + b**2)
                 let magZPwrW: float = (r**c) * (System.Math.E**(-d*theta))
                 let argZPwrW: float = c*theta + d*(System.Math.Log r)

                 let reZW: float = magZPwrW * (System.Math.Cos argZPwrW)
                 let imZW: float = magZPwrW * (System.Math.Sin argZPwrW)
                 ValueType.Complex (reZW, imZW) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> ValueType.Number (a ** b) |> Ok
             | a,                        b                        -> unsupported (a, b)

    [<AutoOpen>]
    module UnaryOperationRules =
        let UnsupportedUnaryOperation (operator: UnaryOperator) (a: ValueType): Result<ValueType> =
            (Some $"Unsupported unary {operator} operation for {a}", a) ||> MathError

        let UnaryPositiveRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Positive
            match a with
             | ValueType.Complex (a, b) -> ValueType.Complex (a, b) |> Ok
             | ValueType.Number   a     -> ValueType.Number  a      |> Ok
             | a                        -> unsupported a

        let UnaryNegativeRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Negative
            match a with
             | ValueType.Complex (a, b) -> ValueType.Complex (-a, -b) |> Ok
             | ValueType.Number   a     -> ValueType.Number   -a      |> Ok
             | a                        -> unsupported a

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

        let UnaryAbsoluteRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.Absolute
            match a with
             | ValueType.Complex (a, b) -> ValueType.Number ((a**2 + b**2) |> System.Math.Sqrt) |> Ok
             | ValueType.Number   a     -> ValueType.Number (a             |> System.Math.Abs ) |> Ok
             | a                        -> unsupported a

        let UnaryGetImaginaryRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.GetImaginary
            match a with
             | ValueType.Complex (_, b) -> ValueType.Number      b  |> Ok
             | ValueType.Number   b     -> ValueType.Complex (0, b) |> Ok
             | a                        -> unsupported a

        let UnaryGetRealRule: UnaryOperationRule = fun a ->
            let unsupported: UnaryOperationRule = UnsupportedUnaryOperation UnaryOperator.GetReal
            match a with
             | ValueType.Complex (a, _) -> ValueType.Number a |> Ok
             | ValueType.Number   a     -> ValueType.Number a |> Ok
             | a                        -> unsupported a

    [<AutoOpen>]
    module ComparisonOperationRules =
        let UnsupportedComparison (operator: ComparisonOperator) (a: ValueType, b: ValueType): Result<bool> =
            (Some $"Unsupported {operator} comparison operation between {a} & {b}", a) ||> MathError

        let EqualityComparisonRule: ComparisonOperationRule = fun ab ->
            match ab with
             | ValueType.Complex (a, b), ValueType.Complex (c, d) -> (a = c && b = d) |> Ok
             | ValueType.Number   a,     ValueType.Number   b     -> (a = b)          |> Ok
             | ValueType.Undefined,      ValueType.Undefined      -> true             |> Ok
             | _,                        _                        -> false            |> Ok

        let InequalityComparisonRule: ComparisonOperationRule = fun ab ->
            (ab |> EqualityComparisonRule) ?=> (not >> Ok)

        let StrictLessThanComparisonRule: ComparisonOperationRule = fun ab ->
            let unsupported: ComparisonOperationRule = UnsupportedComparison ComparisonOperator.StrictLessThan
            match ab with
             | ValueType.Number a, ValueType.Number b -> (a < b) |> Ok
             | a,                  b                  -> unsupported (a, b)

        let StrictGreaterThanComparisonRule: ComparisonOperationRule = fun ab ->
            let unsupported: ComparisonOperationRule = UnsupportedComparison ComparisonOperator.StrictGreaterThan
            match ab with
             | ValueType.Number a, ValueType.Number b -> (a > b) |> Ok
             | a,                  b                  -> unsupported (a, b)

        let NonStrictLessThanComparisonRule: ComparisonOperationRule = fun ab ->
            let unsupported: ComparisonOperationRule = UnsupportedComparison ComparisonOperator.NonStrictLessThan
            match ab with
             | ValueType.Number a, ValueType.Number b -> (a >= b) |> Ok
             | a,                  b                  -> unsupported (a, b)

        let NonStrictGreaterThanComparisonRule: ComparisonOperationRule = fun ab ->
            let unsupported: ComparisonOperationRule = UnsupportedComparison ComparisonOperator.NonStrictGreaterThan
            match ab with
             | ValueType.Number a, ValueType.Number b -> (a >= b) |> Ok
             | a,                  b                  -> unsupported (a, b)
