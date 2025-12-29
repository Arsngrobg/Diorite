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

    /// <summary>
    ///     <p>The <c>Memory</c> module contains related bindings for managing the <b>Diorite</b> virtual memory.</p>
    ///     <p><b>Diorite</b> manages <b>two</b> forms memory:
    ///        <p><b>1.</b> variable table - where in each cell, they are either storing a <c>ValueType</c> or a
    ///           <c>FunctionType</c>. Initially, all cells are declared as <c>Undefined</c>.
    ///        </p>
    ///        <p><b>2.</b> symbol register - which maintains copies of function declarations with symbolic names using
    ///           the <c>[symbol:[SYMBOL]]</c> syntax. This means a redefined function can still be called, provided
    ///           they have been declared to have a symbolic alias.
    ///        </p>
    ///     </p>
    /// </summary>
    [<AutoOpen>]
    module Memory =
        /// <summary>
        ///     <p>The <c>CellData</c> type defines the types of data that can be stored in a variable table cell in
        ///        <b>Diorite</b>. Either, it can be of a value (<c>Undefined</c>, <c>Number</c>, <c>Complex</c>). Or
        ///        it can be of a function declaration (e.g. <c>f(x) = 2*x</c>)
        ///     </p>
        /// </summary>
        type CellData =
            /// <summary>
            ///     <p>The <c>ValueType</c> branch of a <c>CellData</c> in the variable table.</p>
            ///     <p><i>e.g. <c>x = 2</c>/<c>x = undefined</c>/<c>x = complex(0, 1)</c></i></p>
            /// </summary>
            | OfValue    of ValueType
            /// <summary>
            ///     <p>The <c>FunctionType</c> branch of a <c>CellData</c> in the variable table.</p>
            ///     <p><i>e.g. <c>f(x) = 2*x</c></i></p>
            /// </summary>
            | OfFunction of FunctionType

        /// <summary>
        ///     <p>A <c>VariableTable</c> is a region of contiguous memory that maps to a table of data where each row
        ///        is mapped to a prefix of an alphabetical character denoting a variable, and the columns are divided
        ///        into two groups: where an internal subscript of <c>0</c> is the variable without the additional
        ///        subscript character, every subscript from <c>1</c> to <c>10</c> is the encoded subscript.
        ///     </p>
        ///     <p><i>Each character group is offset by <c>11</c> cells, as <c>11</c> different variations of the same
        ///        character.</i>
        ///     </p>
        /// </summary>
        type VariableTable = CellData array

        /// <summary>
        ///     <p>A <c>SymbolRegister</c> is a <c>Map</c> that holds the aliases for a defined <c>FunctionType</c>s in
        ///        <b>Diorite</b>. Each <c>FunctionType</c> in the register is uniquely defined, but that does not
        ///        restrict functions of equal tree signature. That means no function can have at least <b>one</b>
        ///        aliases per definition.
        ///     </p>
        /// </summary>
        type SymbolRegister = Map<string, FunctionType>

        /// <summary>
        ///     <p>The <c>Memory</c> type is the primary storage type for <b>Diorite</b>.</p>
        ///     <p>It holds <b>two</b> sections of data:
        ///        <p><b>1.</b> <c>VariableTable</c> - where in each cell, they are either storing a <c>ValueType</c> or
        ///           a <c>FunctionType</c>. Initially, all cells are declared as <c>Undefined</c>.
        ///        </p>
        ///        <p><b>2.</b> <c>SymbolRegister</c> - which maintains copies of function declarations with symbolic
        ///           names using the <c>[symbol:[SYMBOL]]</c> syntax. This means a redefined function can still be
        ///           called, provided they have been declared to have a symbolic alias.
        ///        </p>
        ///     </p>
        ///     <p>Each 'instance' of <c>Memory</c> should remain immutable.</p>
        /// </summary>
        type Memory = {
            /// <summary>
            ///     <p>The table that maps to each possible <c>VariableType</c> in <b>Diorite</b>.</p>
            /// </summary>
            variables: VariableTable
            /// <summary>
            ///     <p>The register (<c>Map</c>) that contains the aliases and their mapped <c>FunctionType</c>.</p>
            /// </summary>
            symbols:   SymbolRegister
        }

        /// <summary>
        ///     <p>The default state of <c>Memory</c>.</p>
        ///     <p>All variables in the <c>VariableTable</c> are <c>Undefined</c>.</p>
        ///     <p>The <c>SymbolRegister</c> is empty.</p>
        ///     <p><i>This constant value should remain 100% immutable.</i></p>
        /// </summary>
        let Defaults: Memory = {
            variables = (ValueType.Undefined |> CellData.OfValue) |> (Array.create MaxVariables)
            symbols   = Map.empty<string, FunctionType>
        }

        /// <summary>
        ///     <p>Calculates the index of the supplied <c>VariableType</c> for its respective position in the
        ///        <c>VariableTable</c>.
        ///     </p>
        /// </summary>
        /// <param name="var"> the <c>VariableType</c> to derive the position from </param>
        /// <returns> the position of the <c>VariableType</c> in the <c>VariableTable</c> </returns>
        let IndexOf (var: VariableType): int =
            let (c: char), (s: uint8) = var
            assert (c |> System.Char.IsLetter)
            let charIdx: int = if c |> System.Char.IsUpper then ((int 'Z') - (int c)) + 26 else (int 'Z') - (int c)
            let regionStart: int = charIdx * 11
            regionStart + (int s)

        /// <summary>
        ///     <p>Sets the variable in the <c>VariableTable</c> of the supplied <c>Memory</c>.</p>
        ///     <p>It returns a new copy of the <c>Memory</c> that includes the updated <c>VariableTable</c>.</p>
        /// </summary>
        /// <param name="memory"> the <c>Memory</c> struct </param>
        /// <param name="slot"> the <c>VariableType</c> that points to an index in the <c>VariableTable</c> </param>
        /// <param name="value"> <c>CellData</c> that either contains a function or value </param>
        /// <returns> a new <c>Memory</c> struct that carries the new variable table </returns>
        let SetVariable (memory: Memory) (slot: VariableType, value: CellData): Memory =
            let position: int = IndexOf slot
            let tableCopy: VariableTable = memory.variables |> (Array.updateAt position value)
            {
                variables = tableCopy
                symbols   = memory.symbols
            }

        /// <summary>
        ///     <p>Applies the sequence of variable-value pairs and returns the copy of <c>Memory</c> which reflects
        ///        this change.
        ///     </p>
        /// </summary>
        /// <param name="memory"> the <c>Memory</c> struct </param>
        /// <param name="pairs"> a list of <c>VariableType</c>-<c>CellData</c> pairs </param>
        /// <returns> a new <c>Memory</c> struct that carries the new variable table </returns>
        let SetVariables (memory: Memory) (pairs: (VariableType * CellData) list): Memory =
            let rec MutateTable (table: VariableTable) (pairs: (VariableType * CellData) list): unit =
                match pairs with
                 | []              -> ()
                 | (v, cd) :: tail ->
                     let position: int = IndexOf v 
                     table[position] <- cd
                     MutateTable table tail

            let tableCopy: VariableTable = Array.copy memory.variables
            MutateTable tableCopy pairs
            {
                variables = tableCopy
                symbols   = memory.symbols
            }

        /// <summary>
        ///     <p>Gets the variable in the <c>VariableTable</c> of the supplied <c>Memory</c>.</p>
        /// </summary>
        /// <param name="memory"> the <c>Memory</c> struct </param>
        /// <param name="slot"> the <c>VariableType</c> that points to an index in the <c>VariableTable</c> </param>
        /// <returns> the <c>CellData</c> for that <c>VariableType</c> </returns>
        let GetVariable (memory: Memory) (slot: VariableType): CellData =
            let position: int = IndexOf slot
            memory.variables[position]

        /// <summary>
        ///     <p>Updates the <c>SymbolRegister</c> for the supplied <c>Memory</c>.</p>
        ///     <p>This copies the <c>FunctionType</c> into <b>Diorite</b>'s virtual memory so even after the variables
        ///        is redefined, it can still be referenced.
        ///     </p>
        /// </summary>
        /// <param name="memory"> the <c>Memory</c> struct </param>
        /// <param name="alias"> the <c>string</c> alias for the function </param>
        /// <param name="fn"> the <c>FunctionType</c> to copy into the register </param>
        /// <returns> the updated <c>Memory</c> struct wih the updated <c>SymbolRegister</c> </returns>
        let UpdateSymbol (memory: Memory) (alias: string, fn: FunctionType): Memory =
            let updatedSymbols: SymbolRegister = memory.symbols.Add (alias, fn)
            {
                variables = memory.variables // no need to copy as any modifications will be applied to copies later on
                symbols   = updatedSymbols
            }

        /// <summary>
        ///     <p>Tries to obtain the <c>FunctionType</c> from the supplied <c>string</c> alias for the <b>Diorite</b>
        ///        function. It returns a wrapper <c>option</c> type as the <c>alias</c> may not point to a valid
        ///        <b>Diorite</b> function at the time of this function call.
        ///     </p>
        /// </summary>
        /// <param name="memory"> the <c>Memory</c> struct </param>
        /// <param name="alias"> the <c>string</c> alias for the <b>Diorite</b> function that may exist </param>
        /// <returns> the <b>Diorite</b> function wrapped in an <c>option</c> type </returns>
        let GetFunctionFromSymbol (memory: Memory) (alias: string): FunctionType option =
            memory.symbols.TryFind alias
