// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Syntax.fs
// Summary: The syntax definitions for the Diorite mathematics language
// Author:  Arsngrobg, Borngle
// Version: v1.16
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The <c>Syntax</c> module contains all the definitions for the structure of the <b>Diorite</b> language.</p>
///     <p>Ranging from the <c>Token</c>s or <c>TokenType</c>s to the Abstract Syntax Tree and <c>Expression</c>s.</p>
/// </summary>
[<AutoOpen>]
module Syntax =
    /// <summary>
    ///     <p>The structured representation of a <c>Variable</c> in the <b>Diorite</b> mathematics language.</p>
    ///     <p><b>1.</b> The first value (<c>char</c>) is the character which is the variable name (e.g. 'x').</p>
    ///     <p><b>2.</b> The second value (<c>int</c>) is the encoded subscript of the variable - this value is
    ///        optional, where a subscript of <c>0</c> internally represents the plain character (e.g. <c>'x'</c>) and
    ///        <c>10</c> internally represents the subscript-ed variable <c>"x9"</c>, which is the maximum amount of
    ///        subscript-ed permutations of the character.
    ///        <i>The encoded subscript is declared as an unsigned 8-bit integer.</i>
    ///     </p>
    ///     <p>For all characters of the alphabet (including lowercase &amp; uppercase), each with 11 unique
    ///        permutations, that means <b>Diorite</b> supports a total of <c>572</c> variables.
    ///     </p>
    /// </summary>
    type VariableType = char * uint8

    /// <summary>
    ///     <p>The number sets supported in the <b>Diorite</b> language.</p>
    ///     <p>These sets define the domain and/or range of a function.</p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type NumberSet =
        /// <summary>
        ///     <p>The set of all positive integers, including zero.</p>
        ///     <p><c>N = {0, ..., ∞}</c></p>
        /// </summary>
        | Natural
        /// <summary>
        ///     <p>The set of all whole numbers, including zero.</p>
        ///     <p><c>Z = {-∞, ..., 0, ..., ∞}</c></p>
        /// </summary>
        | Integer
        /// <summary>
        ///     <p>The set of all numbers that can be represented as points on an infinitely long number line.</p>
        ///     <p><c>R = {Q & I}</c></p>
        /// </summary>
        | Real
        /// <summary>
        ///     <p>The set of all numbers that can be represented as a ratio of two integers that are not equal.</p>
        ///     <p><c>Q = {x | x = a/b & (b != 0 OR b != a)}</c></p>
        /// </summary>
        | Rational
        /// <summary>
        ///     <p>The set of all numbers that cannot be represented as a ratio of two integers that are not equal.</p>
        ///     <p><c>{x | x != a/b & (a != b OR b != a)}</c></p>
        /// </summary>
        | Irrational
        /// <summary>
        ///     <p>The set of all numbers in the form <c>a + bi</c>, where <c>i</c> is the imaginary unit (sqrt(-1)),
        ///        and <c>i^2 = -1</c>. <c>a</c> and <c>b</c> are <c>Real</c> numbers.
        ///     </p>
        ///     <p><c>C = {a + bi | a, b are Real numbers}</c></p>
        /// </summary>
        | Complex

    /// <summary>
    ///     <p>The default number set if none is provided.</p>
    ///     <p>Any parameter without the domain definition operator defaults to <c>Real</c>.</p>
    ///     <p>Any function without the range definition operator defaults to <c>Real</c> also.</p>
    /// </summary>
    let defaultNumberSet: NumberSet = NumberSet.Real

    /// <summary>
    ///     <p>The union type which describe the cases in which a <c>Value</c> is represented as in <b>Diorite</b>.</p>
    ///     <p>This union type captures the different ways a <c>Value</c> may be expressed, ranging from concrete
    ///        numeric data to conceptual placeholders such as <c>PInfinity</c>/<c>NInfinity</c> or the absence of any
    ///        value (<c>Undefined</c>).
    ///     </p> 
    /// </summary>
    type ValueType =
        /// <summary>
        ///     <p>A 64-bit, floating-point decimal.</p>
        /// </summary>
        | Number    of float
        /// <summary>
        ///     <p>A complex number of the form <c>a + im(b)</c>.</p>
        /// </summary>
        | Complex   of float * float
        /// <summary>
        ///     <p>Positive infinity.</p>
        ///     <p><c>∞</c></p>
        /// </summary>
        | PInfinity
        /// <summary>
        ///     <p>Negative infinity.</p>
        ///     <p><c>-∞</c></p>
        /// </summary>
        | NInfinity
        /// <summary>
        ///     <p>An undetermined value.</p>
        ///     <p><i>Can also be seen as the absence of a value - like <c>null</c>.</i></p>
        /// </summary>
        | Undefined
    
    /// <summary>
    ///     <p>A <c>TokenType</c> is an identifier type for a lexical <c>Token</c> in the <b>Diorite</b>.</p>
    ///     <p>These union types do not store metadata is it makes it easier to consume every type of token.</p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type TokenType =
        /// <summary>
        ///     <p>Any string literal that is not recognised by the <b>Diorite</b> language.</p>
        ///     <p>This indicates the lexer failed to recognise the lexeme, hence an error has occurred.</p>
        /// </summary>
        | IllegalToken
        /// <summary>
        ///     <p>A floating-point decimal number.</p>
        ///     <p>Any integer or decimal representation.</p>
        /// </summary>
        | Number
        /// <summary>
        ///     <p>A character followed by an optional encoded subscript.</p>
        ///     <p>A variable is a tuple consisting of the character that represents it, and the encoded subscript.
        ///        The encoded subscript is an unsigned, 8-bit integer that should be within the range of <c>0</c> to
        ///        <c>11</c>. Where an internal subscript of <c>0</c> is just the plain-text character, and a subscript
        ///        of <c>1</c> through <c>10</c> are the characters with the number minus one.
        ///     </p>
        ///     <p>A variable represents <i>some</i> value (<c>ValueType</c>) in memory, whether that be the
        ///        interpreter's virtual memory, or the device's hardware memory when compiled.
        ///     </p>
        /// </summary>
        | Variable
        /// <summary>
        ///     <p>Any string literal that is not already reserved by the <b>Diorite</b> language.</p>
        ///     <p>It is an alias for a <b>Diorite</b> function that </p>
        /// </summary>
        | Symbol
        /// <summary>
        ///     <p>The string literal <c>"undefined"</c></p>
        ///     <p>The equivalent representation of the absence of a value.</p>
        /// </summary>
        | Undefined
        /// <summary>
        ///     <p>The string literal <c>"infinity"</c>/<c>"inf"</c></p>
        ///     <p>The representation of an extremely large, positive-bound value.</p>
        /// </summary>
        | Infinity
        /// <summary>
        ///     <p>The string literal <c>"im"</c>.</p>
        ///     <p>A constructor for an imaginary number, consumes subexpression on right-hand side.</p>
        /// </summary>
        | Im
        /// <summary>
        ///     <p>The string literal <c>"plot"</c>.</p>
        ///     <p>Plots the function that is on the right-hand side of this token.</p>
        /// </summary>
        | Plot
        /// <summary>
        ///     <p>the string literal <c>"error"</c>.</p>
        ///     <p>Throws a <c>MathError</c> when encountered with the optional error message given to it.</p>
        /// </summary>
        | Error
        /// <summary>
        ///     <p>The string literal <c>"pi"</c>.</p>
        ///     <p>The constant value for pi (π).</p>
        /// </summary>
        | Pi
        /// <summary>
        ///     <p>The string literal <c>"tau"</c></p>
        ///     <p>The constant value for tau (τ).</p>
        /// </summary>
        | Tau
        /// <summary>
        ///     <p>The string literal <c>"euler"</c>.</p>
        ///     <p>The constant value for Euler's constant (e).</p>
        /// </summary>
        | Euler
        /// <summary>
        ///     <p>The character literal <c>':'</c></p>
        ///     <p>The delimiter for defining the domain for a function in <b>Diorite</b>.</p>
        /// </summary>
        | Colon
        /// <summary>
        ///     <p>The string literal <c>"->"</c>.</p>
        ///     <p>The delimiter for defining the range for a function in <b>Diorite</b>.</p>
        /// </summary>
        | Arrow
        /// <summary>
        ///     <p>The character literal <c>','</c>.</p>
        ///     <p>The delimiter for sequencing parameters or arguments for a function in <b>Diorite</b>.</p>
        /// </summary>
        | Comma
        /// <summary>
        ///     <p>The character literal <c>'^'</c>.</p>
        ///     <p>The binary exponent operator.</p>
        /// </summary>
        | Hat
        /// <summary>
        ///     <p>The character literal <c>'!'</c>.</p>
        ///     <p>The unary factorial operator.</p>
        /// </summary>
        | Exclamation
        /// <summary>
        ///     <p>The character literal <c>'*'</c>.</p>
        ///     <p>The binary multiplication operator.</p>
        /// </summary>
        | Asterisk
        /// <summary>
        ///     <p>The character literal <c>'/'</c>.</p>
        ///     <p>The binary division operator.</p>
        /// </summary>
        | ForwardSlash
        /// <summary>
        ///     <p>The string literal <c>"//"</c>.</p>
        ///     <p>The binary floor division operator.</p>
        /// </summary>
        | DoubleForwardSlash
        /// <summary>
        ///     <p>The character literal <c>'%'</c>.</p>
        ///     <p>The binary modulo operator.</p>
        /// </summary>
        | Percentage
        /// <summary>
        ///     <p>The character literal <c>'+'</c>.</p>
        ///     <p>The binary addition operator.</p>
        /// </summary>
        | Plus
        /// <summary>
        ///     <p>The character literal <c>'-'</c>.</p>
        ///     <p>The binary subtraction operator.</p>
        /// </summary>
        | Hyphen
        /// <summary>
        ///     <p>The character literal <c>'='</c>.</p>
        ///     <p>The assignment/equality operator.</p>
        /// </summary>
        | Equals
        /// <summary>
        ///     <p>The string literal <c>"!="</c>.</p>
        ///     <p>The inequality operator.</p>
        /// </summary>
        | NotEqual
        /// <summary>
        ///     <p>The character literal <c>'&lt;'</c>.</p>
        ///     <p>The strict less-than inequality operator.</p>
        /// </summary>
        | LessThan
        /// <summary>
        ///     <p>The character literal <c>'&gt;'</c>.</p>
        ///     <p>The strict greater-than inequality operator.</p>
        /// </summary>
        | GreaterThan
        /// <summary>
        ///     <p>The string literal <c>'&lt;='</c>.</p>
        ///     <p>The non-strict less-than-or-equal inequality operator.</p>
        /// </summary>
        | LessThanOrEqual
        /// <summary>
        ///     <p>The string literal <c>'&gt;='</c>.</p>
        ///     <p>The non-strict greater-than-or-equal inequality operator.</p>
        /// </summary>
        | GreaterThanOrEqual
        /// <summary>
        ///     <p>The string literal <c>"if"</c>.</p>
        ///     <p>Represents a guard condition in a piecewise expression.#
        ///        It selects the correct branch if the associated boolean expression evaluates to <c>true</c>.
        ///     </p>
        /// </summary>
        | If
        /// <summary>
        ///     <p>The string literal <c>"otherwise"</c>.</p>
        ///     <p>Represents the fallback branch in a piecewise operation.
        ///        This branch is selected only ig all previous conditions evaluate to <c>false</c>.
        ///     </p>
        /// </summary>
        | Otherwise
        /// <summary>
        ///     <p>The character literal <c>'('</c>.</p>
        ///     <p>Denotes the beginning of a subexpression, a sequence of parameters or arguments.</p>
        /// </summary>
        | LeftParenthesis
        /// <summary>
        ///     <p>The character literal <c>')'</c>.</p>
        ///     <p>Denotes the end of a subexpression, a sequence of parameters or arguments.</p>
        /// </summary>
        | RightParenthesis
        /// <summary>
        ///     <p>The character literal <c>'['</c>.</p>
        ///     <p>Denotes the beginning of a function metadata attribute.</p>
        /// </summary>
        | LeftBracket
        /// <summary>
        ///     <p>The character literal <c>']'</c>.</p>
        ///     <p>Denotes the end of a function metadata attribute.</p>
        /// </summary>
        | RightBracket
        /// <summary>
        ///     <p>The character literal <c>'{'</c>.</p>
        ///     <p>Denotes the beginning of a function body.</p>
        /// </summary>
        | LeftBrace
        /// <summary>
        ///     <p>The character literal <c>'}'</c>.</p>
        ///     <p>Denotes the end of a function body.</p>
        /// </summary>
        | RightBrace
        /// <summary>
        ///     <p>The character literal <c>'|'</c>.</p>
        ///     <p>States that the result of the expression they wrap should have the absolute operation.</p>
        /// </summary>
        | Bar
        /// <summary>
        ///     <p>The character literal <c>';'</c>.</p>
        ///     <p>Denotes the end of a statement in <b>Diorite</b>.</p>
        /// </summary>
        | SemiColon

    /// <summary>
    ///     <p>This is the payload type for a <c>Token</c>.</p>
    ///     <p>The only data currently required for parsing:
    ///        <p><b>1.</b> the 64-bit floating-point representation of the number <c>Token</c> lexeme.</p>
    ///        <p><b>2.</b> the <c>VariableType</c> that correlates to the variable <c>Token</c> lexeme.</p>
    ///     </p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type TokenValue =
        /// <summary>
        ///     <p>No payload value is stored by the <c>Token</c>.</p>
        /// </summary>
        | None
        /// <summary>
        ///     <p>A 64-bit, floating-point number is the payload for the <c>Token</c>.</p>
        /// </summary>
        | Number   of float
        /// <summary>
        ///     <p>The <c>VariableType</c> representation of the variable lexeme of the <c>Token</c>.</p>
        /// </summary>
        | Variable of VariableType

    /// <summary>
    ///     <p>A <c>Token</c> is an atomic lexical unit in the <b>Diorite</b> language.</p>
    /// </summary>
    type Token = {
        /// <summary>
        ///     <p>The string literal this <c>Token</c> represents.</p>
        /// </summary>
        lexeme: string
        /// <summary>
        ///     <p>The identifier for this <c>Token</c>.</p>
        /// </summary>
        id:     TokenType
        /// <summary>
        ///     <p>The payload value this <c>Token</c> stores.</p>
        /// </summary>
        value:  TokenValue
        /// <summary>
        ///     <p>The line this <c>Token</c> is found on.</p>
        ///     <p>This is the relative line number from the lexer context.</p>
        /// </summary>
        line:   uint
        /// <summary>
        ///     <p>The column this <c>Token</c> is found on.</p>
        ///     <p>This is the relative line number from the lexer context.</p>
        /// </summary>
        column: uint
    }

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
    ///     <p>A <c>UnaryOperator</c> is an operator that executes on a single operand on either side, depending on
    ///        which direction it binds.
    ///     </p>
    ///     <p><i>Literal pattern: <c>[UnaryOperator] a</c> | <c>a [UnaryOperator]</c></i></p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type UnaryOperator =
        /// <summary>
        ///     <p>The unary operator for positive (<c>+a</c>) - the identity operator.</p>
        ///     <p><i>binds: to the right</i></p>
        /// </summary>
        | Positive
        /// <summary>
        ///     <p>The unary operator for negation (<c>-a</c>).</p>
        ///     <p><i>binds: to the right</i></p>
        /// </summary>
        | Negative
        /// <summary>
        ///     <p>The unary operator for factorial (<c>a!</c>).</p>
        ///     <p><i>binds: to the left</i></p>
        /// </summary>
        | Factorial
        /// <summary>
        ///     <p>The unary operator for absolute (<c>|a|</c>).</p>
        ///     <p><i>binds: N/A</i></p>
        /// </summary>
        | Absolute

    /// <summary>
    ///     <p>A <c>ComparisonOperator</c> is an operator that is used to compare to values either side of it.</p>
    ///     <p><i>Literal pattern: <c>a [ComparisonOperator] b</c></i></p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type ComparisonOperator =
        /// <summary>
        ///     <p>The comparison operator for checking the equality of two values (<c>a = b</c>).</p>
        /// </summary>
        | Equality
        /// <summary>
        ///     <p>The comparison operator for checking the inequality of two values (<c>a != b</c>).</p>
        /// </summary>
        | Inequality
        /// <summary>
        ///     <p>The comparison operator for checking if the left-hand value is less than, but not equal-to the
        ///        right-hand value (<c>a &lt; b</c>).
        ///     </p>
        /// </summary>
        | StrictLessThan
        /// <summary>
        ///     <p>The comparison operator for checking if the left-hand value is greater than, but not equal-to the
        ///        right-hand value (<c>a &gt; b</c>).
        ///     </p>
        /// </summary>
        | StrictGreaterThan
        /// <summary>
        ///     <p>The comparison operator for checking if the left-hand value is less than, or equal-to the right-hand
        ///        value (<c>a &lt;= b</c>).
        ///     </p>
        /// </summary>
        | NonStrictLessThan
        /// <summary>
        ///     <p>The comparison operator for checking if the left-hand value is greater than, or equal-to the
        ///        right-hand value (<c>a &lt;= b</c>).
        ///     </p>
        /// </summary>
        | NonStrictGreaterThan

    /// <summary>
    ///     <p>A <c>FunctionReferenceType</c> is all cases in which a <b>Diorite</b> function can be referenced with.
    ///        All functions can be referenced via the variable (e.g. <c>f(...)</c>), or its alias (symbol).
    ///     </p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type FunctionReferenceType =
        /// <summary>
        ///     <p>A variable reference to a function.</p>
        ///     <p>This is a reference to the function via the variable table.</p>
        /// </summary>
        | OfVariable of VariableType
        /// <summary>
        ///     <p>A reserved string symbol reference to a function.</p>
        ///     <p>This is a reference to the function via the symbol dictionary.</p>
        /// </summary>
        | OfSymbol   of string

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
        ///     <p>Represents a reference to <c>ValueType</c> which the value is obtained when evaluating.</p>
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

    /// <summary>
    ///     <p>The structured representation of a plottable expression in the <b>Diorite</b> language.</p>
    ///     <p>It is a subset of <c>Expression</c></p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type PlottableExpression =
        /// <summary>
        ///     <p>An atomic unit for a plottable expression.</p>
        ///     <p>Represents a <c>ValueType</c>.</p>
        /// </summary>
        | Value             of ValueType
        /// <summary>
        ///     <p>An atomic unit for a plottable expression.</p>
        ///     <p>Represents a reference to function to be plotted.</p>
        /// </summary>
        | FunctionReference of FunctionReferenceType
        /// <summary>
        ///     <p>A structured representation of a binary operation in <b>Diorite</b>.</p>
        ///     <p>It is a tuple which has the left and right sub expressions and its <c>BinaryOperator</c>.</p>
        /// </summary>
        | BinaryOperation   of Expression * BinaryOperator * Expression
        /// <summary>
        ///     <p>A structured representation of a unary operation in <b>Diorite</b>.</p>
        ///     <p>It is a tuple which has the operand and its <c>UnaryOperator</c>.</p>
        /// </summary>
        | UnaryOperation    of Expression * UnaryOperator

    /// <summary>
    ///     <p>A <c>FunctionResult</c> is exactly that, a result from a function that may be an <c>Expression</c>, or
    ///        an error with an optional error message.
    ///     </p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type FunctionResult =
        /// <summary>
        ///     <p>An <c>Expression</c> returned by the function.</p>
        /// </summary>
        | Expression of Expression
        /// <summary>
        ///     <p>An error thrown by the function.</p>
        /// </summary>
        | Error      of string option

    /// <summary>
    ///     <p>A tuple which represents the structure of a comparison operation in <b>Diorite</b>.</p>
    ///     <p>It consist of:
    ///        <p><b>1.</b> the left-hand side <c>Expression</c>.</p>
    ///        <p><b>2.</b> the <c>ComparisonOperator</c>.</p>
    ///        <p><b>3.</b> the right-hand side <c>Expression</c>.</p>
    ///     </p>
    /// </summary>
    type ComparisonOperation = Expression * ComparisonOperator * Expression

    /// <summary>
    ///     <p>The structured representation of a piecewise condition in a <b>Diorite</b> function.</p>
    ///     <p>It's a tuple which holds:
    ///        <p><b>1.</b> the <c>Expression</c> to be evaluated if the <c>ComparisonOperation</c> evaluates to
    ///           <c>true</c>.
    ///        </p>
    ///        <p><b>2.</b> the <c>ComparisonOperation</c> which is the predicate to be tested.
    ///           This decides whether the function should return the <c>Expression</c> to the left of it, or move down
    ///           the conditional chain.
    ///        </p>
    ///     </p>
    /// </summary>
    type PiecewiseCondition = Expression * ComparisonOperation

    /// <summary>
    ///     <p>A subtype of the <c>PiecewiseCondition</c> type where its <c>ComparisonOperation</c> will always evaluate
    ///        to <c>true</c>.
    ///     </p>
    ///     <p>This models the <c>Expression otherwise</c> syntax using the <c>PiecewiseCondition</c> type.</p>
    /// </summary>
    /// <param name='defaultExpression'> the default <c>Expression</c> to return </param>
    /// <returns> a <c>PiecewiseCondition</c> that will always evaluate to <c>true</c> </returns>
    let PiecewiseBaseCase (defaultExpression: Expression): PiecewiseCondition = (
        defaultExpression,
        (
            ValueType.Undefined |> Expression.Value,
            ComparisonOperator.Equality,
            ValueType.Undefined |> Expression.Value
        )
    )

    /// <summary>
    ///     <p>The <c>FunctionMetadata</c> type are options relating to a function definition in <b>Diorite</b>.</p>
    /// </summary>
    type FunctionMetadata = {
        /// <summary>
        ///     <p>The optional alias for the function.</p>
        /// </summary>
        symbol:   string option
        /// <summary>
        ///     <p>Whether function calls should be inlined by the compiler.</p>
        ///     <p><i>This is a compile-time feature only.</i></p>
        /// </summary>
        inlined:  bool
        /// <summary>
        ///     <p>Whether previous function calls should be cached by</p>
        /// </summary>
        memoized: bool
    }

    /// <summary>
    ///     <p>The <c>FunctionParameter</c> is a parameter in a function in <b>Diorite</b>.</p>
    ///     <p><b>1.</b> The first value (<c>VariableType</c>), which is the identifier for the parameter.</p>
    ///     <p><b>2.</b> The second value (<c>NumberSet</c>), which denotes the number set which the parameter must
    ///        comply with in order for the function to accept it.
    ///     </p>
    /// </summary>
    type FunctionParameter = VariableType * NumberSet

    /// <summary>
    ///     <p>The <c>FunctionMetadata</c> record type contains type information about a function definition.</p>
    ///     <p>It also maintains the identifier which this function is bound to, and its <c>FunctionMetadata</c>.</p>
    ///     <p><i>This is a concrete field, meaning it cannot be modified after the function definition.</i></p>
    /// </summary>
    type FunctionAttributes = {
        /// <summary>
        ///     <p>The <c>VariableType</c> that identifies this function.</p>
        ///     <p>This is the identifier that was used to originally define the function, however is not always
        ///        accurate to what is in memory at a given moment.
        ///     </p>
        /// </summary>
        identifier: VariableType
        /// <summary>
        ///     <p>The list of parameters that the function requires to execute.</p>
        /// </summary>
        parameters: FunctionParameter list
        /// <summary>
        ///     <p>The return <i>type</i> of the function.</p>
        ///     <p>This is used to check for set membership on function return.</p>
        /// </summary>
        range:      NumberSet
        /// <summary>
        ///     <p>The metadata for the function.</p>
        /// </summary>
        metadata:   FunctionMetadata
    }

    /// <summary>
    ///     <p>The default <c>FunctionMetadata</c> for a <b>Diorite</b> function.</p>
    ///     <p>If no metadata attributes are given, by default, all values are 'empty'.</p>
    /// </summary>
    let defaultFunctionMetadata: FunctionMetadata = {
        symbol   = None
        inlined  = false
        memoized = false
    }

    /// <summary>
    ///     <p>The structured representation of a function body in <b>Diorite</b>.</p>
    ///     <p>A function body in <b>Diorite</b> either contains:
    ///        <p><b>1.</b> a single <c>Expression</c></p>
    ///        <p><b>2.</b> a chain of piecewise conditions</p>
    ///     </p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type FunctionBody =
        /// <summary>
        ///     <p>Describes a function with a single <c>Expression</c>.</p>
        /// </summary>
        | Expression          of Expression
        /// <summary>
        ///     <p>Describes a function consisting of a chain of piecewise conditions.</p>
        /// </summary>
        | PiecewiseConditions of PiecewiseCondition list

    /// <summary>
    ///     <p>The <c>AST</c> is the tree structure of a <b>Diorite</b> statement.</p>
    /// </summary>
    [<RequireQualifiedAccess>]
    type AST =
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
        | PlotFunction       of PlottableExpression
        /// <summary>
        ///     <p>A statement that assigns an <c>Expression</c> on the right-hand side to a variable on the
        ///        left-hand side.
        ///     </p>
        ///     <p><i>Example: y = 100;</i></p>
        /// </summary>
        | Assignment         of VariableType * Expression
        /// <summary>
        ///     <p>A function definition.
        ///        Either composed of a single <c>Expression</c>, or a series of <c>PiecewiseOperation</c>s.
        ///     </p>
        ///     <p><i>Example: f(x) = 2*x;</i></p>
        /// </summary>
        | FunctionDefinition of FunctionAttributes * FunctionBody
