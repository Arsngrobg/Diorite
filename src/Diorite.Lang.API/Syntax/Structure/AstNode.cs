// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    AstNode.cs
// Summary: The type definition for the ASTNode type, which is a node in the Abstract Syntax Tree (AST) of a Diorite
//          program
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using System.Runtime;
using Diorite.Lang.API.Syntax.Data;
using Diorite.Lang.API.Syntax.Views;
using Microsoft.FSharp.Core;

namespace Diorite.Lang.API.Syntax.Structure;

/// <summary>
///     <p>The <c>AstNode</c> type is a structured representation of <b>Diorite</b> code.</p>
///     <p>In short, it is the Abstract Syntax Tree (AST) of the provided source code.</p>
///     <p>It provides a rich set of methods for querying the structure of the node itself or the subtree.</p>
/// </summary>
/// <typeparam name="T"> the type of the payload value this <c>AstNode</c> carries </typeparam>
public sealed class AstNode<T>
{
    internal static AstNode<object?> OfCoreType(Core.Syntax.ASTNode coreNode)
    {
        switch (coreNode)
        {
            case Core.Syntax.ASTNode.Expression exp:
                return OfCoreExpression(exp.Item);
            case Core.Syntax.ASTNode.PlotFunction pltFn:
                var anonFn  = OfCoreExpression(pltFn.Item.expression);
                var anonArg = new AstNode<object?>(
                    AstNode<object?>.Kind.FunctionArguments,
                    Variable.OfCoreType(pltFn.Item.parameter),
                    []
                );
                return new AstNode<object?>(
                    AstNode<object?>.Kind.PlotFunction,
                    null,
                    [anonFn, anonArg]
                );
            case Core.Syntax.ASTNode.Assignment assign:
                var lValue = new AstNode<object?>(
                    AstNode<object?>.Kind.Variable,
                    Variable.OfCoreType(assign.Item1),
                    []
                );
                var rValue = OfCoreExpression(assign.Item2);
                return new AstNode<object?>(
                    AstNode<object?>.Kind.Assignment,
                    null,
                    [lValue, rValue]
                );
            case Core.Syntax.ASTNode.FunctionDefinition fnDef:
                var fnAttrs = FunctionAttributes.OfCoreType(fnDef.Item.Item1);
                var fnBody  = OfCoreFunctionBody(fnDef.Item.Item2);
                return new AstNode<object?>(
                    AstNode<object?>.Kind.FunctionDefinition,
                    fnAttrs,
                    [fnBody]
                );
            default:
                throw new AmbiguousImplementationException($"Missing mapping for {coreNode.GetType()}");
        }
    }

    internal static AstNode<object?> OfCoreFunctionBody(Core.Syntax.FunctionBody coreBody)
    {
        switch (coreBody)
        {
            case Core.Syntax.FunctionBody.Expression exp:
                return OfCoreExpression(exp.Item);
            case Core.Syntax.FunctionBody.PiecewiseConditions piecewiseConditions:
                var conditions = piecewiseConditions.Item.Select(pwc =>
                {
                    var resultNode = pwc.Item1 switch
                    {
                        Core.Syntax.FunctionResult.Error err =>
                            new AstNode<object?>(
                                AstNode<object?>.Kind.FunctionError,
                                FSharpOption<string>.get_IsNone(err.Item) ? null : err.Item.Value,
                                []
                            ),
                        Core.Syntax.FunctionResult.Expression exp =>
                            OfCoreExpression(exp.Item),
                        _ => throw new AmbiguousImplementationException($"Missing mapping for {pwc.Item1.GetType()}")
                    };
                    
                    var left  = OfCoreExpression(pwc.Item2.Item1);
                    var right = OfCoreExpression(pwc.Item2.Item3);
                    var cmpOp = pwc.Item2.Item2;

                    AstNode<object?>.Kind type;
                    if      (cmpOp.IsEquality)             type = AstNode<object?>.Kind.EqualityComparison;
                    else if (cmpOp.IsInequality)           type = AstNode<object?>.Kind.InequalityComparison;
                    else if (cmpOp.IsStrictLessThan)       type = AstNode<object?>.Kind.StrictLessThanComparison;
                    else if (cmpOp.IsStrictGreaterThan)    type = AstNode<object?>.Kind.StrictGreaterThanComparison;
                    else if (cmpOp.IsNonStrictLessThan)    type = AstNode<object?>.Kind.NonStrictLessThanComparison;
                    else if (cmpOp.IsNonStrictGreaterThan) type = AstNode<object?>.Kind.NonStrictGreaterThanComparison;
                    else throw new AmbiguousImplementationException($"Missing {cmpOp} mapping");
                    var comparisonNode = new AstNode<object?>(type, null, [left, right]);

                    return new AstNode<object?>(
                        AstNode<object?>.Kind.PiecewiseCondition,
                        null,
                        [resultNode, comparisonNode]
                    );
                }).ToArray();

                return new AstNode<object?>(
                    AstNode<object?>.Kind.PiecewiseConditions,
                    null,
                    conditions
                );
            default:
                throw new AmbiguousImplementationException($"Missing mapping for {coreBody.GetType()}");
        }
    }

    internal static AstNode<object?> OfCoreExpression(Core.Syntax.Expression coreExpression)
    {
        AstNode<object?>.Kind type;
        switch (coreExpression)
        {
            case Core.Syntax.Expression.Value value:
                return new AstNode<object?>(
                    AstNode<object?>.Kind.Value,
                    Views.Value.OfCoreType(value.Item),
                    []
                );
            case Core.Syntax.Expression.Variable variable:
                return new AstNode<object?>(
                    AstNode<object?>.Kind.Variable,
                    Variable.OfCoreType(variable.Item),
                    []
                );
            case Core.Syntax.Expression.BinaryOperation binOp:
                var left  = OfCoreExpression(binOp.Item1);
                var right = OfCoreExpression(binOp.Item3);
                var bop   = binOp.Item2;
                
                if      (bop.IsAddition)       type = AstNode<object?>.Kind.BinaryAddition;
                else if (bop.IsSubtraction)    type = AstNode<object?>.Kind.BinarySubtraction;
                else if (bop.IsMultiplication) type = AstNode<object?>.Kind.BinaryMultiplication;
                else if (bop.IsDivision)       type = AstNode<object?>.Kind.BinaryDivision;
                else if (bop.IsModulo)         type = AstNode<object?>.Kind.BinaryModulo;
                else if (bop.IsFloorDivision)  type = AstNode<object?>.Kind.BinaryFloorDivision;
                else if (bop.IsExponent)       type = AstNode<object?>.Kind.BinaryExponent;
                else if (bop.IsOfComplex)      type = AstNode<object?>.Kind.BinaryOfComplex;
                else throw new AmbiguousImplementationException($"Missing {bop} mapping");

                return new AstNode<object?>(type, null, [left, right]);
            case Core.Syntax.Expression.UnaryOperation unOp:
                var operand = OfCoreExpression(unOp.Item1);
                var uop     = unOp.Item2;
                
                if      (uop.IsPositive)     type = AstNode<object?>.Kind.UnaryPositive;
                else if (uop.IsNegative)     type = AstNode<object?>.Kind.UnaryNegative;
                else if (uop.IsFactorial)    type = AstNode<object?>.Kind.UnaryFactorial;
                else if (uop.IsGetReal)      type = AstNode<object?>.Kind.UnaryGetReal;
                else if (uop.IsGetImaginary) type = AstNode<object?>.Kind.UnaryGetImaginary;
                else throw new AmbiguousImplementationException($"Missing {uop} mapping");

                return new AstNode<object?>(type, null, [operand]);
            case Core.Syntax.Expression.FunctionCall fnCall:
                var fnRef  = fnCall.Item1;
                var fnArgs = new AstNode<object?>(
                    AstNode<object?>.Kind.FunctionArguments,
                    fnCall.Item2.Select(OfCoreExpression).ToArray(),
                    []
                );

                var fnId = fnRef.IsOfSymbol
                    ? ((Core.Syntax.FunctionReferenceType.OfSymbol) fnRef).Item
                    : Variable.OfCoreType(((Core.Syntax.FunctionReferenceType.OfVariable) fnRef).Item).ToString();

                return new AstNode<object?>(
                    AstNode<object?>.Kind.FunctionCall,
                    fnId,
                    [fnArgs]
                );
            default:
                throw new AmbiguousImplementationException($"Missing Expression case {coreExpression}");
        }
    }

    /// <summary>
    ///     <p>A tagging enum for the <c>AstNode</c> type, which denotes the type of <c>AstNode</c> it is, and whether
    ///        it has children or not.
    ///     </p>
    /// </summary>
    public enum Kind
    {
        /// <summary>
        ///     <p>An atomic unit for an expression.</p>
        ///     <p>Represents a <c>Value</c>, and has no child nodes (leaf node).</p>
        /// </summary>
        Value,
        /// <summary>
        ///     <p>An atomic unit for an expression.</p>
        ///     <p>Represents a reference to <c>Value</c> where the true value is obtained during evaluation.</p>
        ///     <p>It has no child nodes (leaf node).</p>
        /// </summary>
        Variable,
        /// <summary>
        ///     <p>A structured representation of a function call in <b>Diorite</b>.</p>
        ///     <p>It is a <c>FunctionReference</c> and a sequence of arguments.</p>
        /// </summary>
        FunctionCall,
        /// <summary>
        ///     <p>A structured representation of function arguments in <b>Diorite</b>.</p>
        ///     <p>It is a sequence of expressions plugged into a function call.</p>
        /// </summary>
        FunctionArguments,
        /// <summary>
        ///     <p>A statement that requests the plot of an anonymous function.</p>
        ///     <p><i>Example: <c>plot f(x); # f(x)=2*x</c></i> OR <c>plot (2*x)</c></p>
        /// </summary>
        PlotFunction,
        /// <summary>
        ///     <p>A statement that assigns an <c>Expression</c> on the right-hand side to a <c>Variable</c> on the
        ///        left-hand side.
        ///     </p>
        ///     <p><i>Example: <c>y = 100;</c></i></p>
        /// </summary>
        Assignment,
        /// <summary>
        ///     <p>A function definition.
        ///        Either composed of a single <c>Expression</c>, or a series of <c>PiecewiseOperation</c>s.
        ///     </p>
        ///     <p><i>Example: <c>f(x) = 2*x;</c> OR <c>f(x) = { -x if x &lt; 0; x otherwise; }</c></i></p>
        /// </summary>
        FunctionDefinition,
        PiecewiseConditions,
        /// <summary>
        ///     <p>The structured representation of a piecewise condition in a <b>Diorite</b> function.</p>
        ///     <p>It's a tree with <c>2</c> children that hold:
        ///        <p><b>1.</b> the <c>Expression</c> to be evaluated if the <c>ComparisonOperation</c> evaluates to
        ///           <c>true</c>.
        ///        </p>
        ///        <p><b>2.</b> the <c>ComparisonOperation</c> which is the predicate to be tested.
        ///           This decides whether the function should return the <c>Expression</c> to the left of it, or move
        ///           down the conditional chain.
        ///        </p>
        ///     </p>
        /// </summary>
        PiecewiseCondition,
        /// <summary>
        ///     <p>An error thrown by a function composed of piecewise operations.</p>
        ///     <p>It has an optional error message.</p>
        /// </summary>
        FunctionError,
        /// <summary>
        ///     <p>The binary operation for addition (<c>a + b</c>).</p>
        ///     <p><i>associativity: left associative</i></p>
        /// </summary>
        BinaryAddition,
        /// <summary>
        ///     <p>The binary operation for subtraction (<c>a - b</c>).</p>
        ///     <p><i>associativity: left associative</i></p>
        /// </summary>
        BinarySubtraction,
        /// <summary>
        ///     <p>The binary operation for multiplication (<c>a * b</c>).</p>
        ///     <p><i>associativity: left associative</i></p>
        /// </summary>
        BinaryMultiplication,
        /// <summary>
        ///     <p>The binary operation for division (<c>a / b</c>).</p>
        ///     <p><i>associativity: left associative</i></p>
        /// </summary>
        BinaryDivision,
        /// <summary>
        ///     <p>The binary operation for modulo (<c>a % b</c>).</p>
        ///     <p><i>associativity: left associative</i></p>
        /// </summary>
        BinaryModulo,
        /// <summary>
        ///     <p>The binary operation for floor division (<c>a // b</c>).</p>
        ///     <p><i>associativity: left associative</i></p>
        /// </summary>
        BinaryFloorDivision,
        /// <summary>
        ///     <p>The binary operation for exponent (<c>a ^ b</c>).</p>
        ///     <p><i>associativity: right associative</i></p>
        /// </summary>
        BinaryExponent,
        /// <summary>
        ///     <p>The binary operation for declaring a complex value (<c>a + bi</c>).</p>
        ///     <p><i>binds: N/A</i></p>
        /// </summary>
        BinaryOfComplex,
        /// <summary>
        ///     <p>The unary operation for positive (<c>+a</c>) - the identity operation.</p>
        ///     <p><i>binds: to the right</i></p>
        /// </summary>
        UnaryPositive,
        /// <summary>
        ///     <p>The unary operation for negation (<c>-a</c>).</p>
        ///     <p><i>binds: to the right</i></p>
        /// </summary>
        UnaryNegative,
        /// <summary>
        ///     <p>The unary operation for factorial (<c>a!</c>).</p>
        ///     <p><i>binds: to the left</i></p>
        /// </summary>
        UnaryFactorial,
        /// <summary>
        ///     <p>The unary operation for absolute (<c>|a|</c>).</p>
        ///     <p><i>binds: N/A</i></p>
        /// </summary>
        UnaryAbsolute,
        /// <summary>
        ///     <p>The unary operation for obtaining the real component of a <c>Complex</c> value (<c>re(a)</c>).</p>
        ///     <p><i>binds: N/A</i></p>
        /// </summary>
        UnaryGetReal,
        /// <summary>
        ///     <p>The unary operation for obtaining the imaginary component of a <c>Complex</c> value
        ///        (<c>im(b)</c>).
        ///     </p>
        ///     <p><i>binds: N/A</i></p>
        /// </summary>
        UnaryGetImaginary,
        /// <summary>
        ///     <p>The comparison operation for checking the equality of two values (<c>a = b</c>).</p>
        /// </summary>
        EqualityComparison,
        /// <summary>
        ///     <p>The comparison operation for checking the inequality of two values (<c>a != b</c>).</p>
        /// </summary>
        InequalityComparison,
        /// <summary>
        ///     <p>The comparison operation for checking if the left-hand value is less than, but not equal-to the
        ///        right-hand value (<c>a &lt; b</c>).
        ///     </p>
        /// </summary>
        StrictLessThanComparison,
        /// <summary>
        ///     <p>The comparison operation for checking if the left-hand value is greater than, but not equal-to the
        ///        right-hand value (<c>a &gt; b</c>).
        ///     </p>
        /// </summary>
        StrictGreaterThanComparison,
        /// <summary>
        ///     <p>The comparison operation for checking if the left-hand value is less than, or equal-to the right-hand
        ///        value (<c>a &lt;= b</c>).
        ///     </p>
        /// </summary>
        NonStrictLessThanComparison,
        /// <summary>
        ///     <p>The comparison operation for checking if the left-hand value is greater than, or equal-to the
        ///        right-hand value (<c>a &lt;= b</c>).
        ///     </p>
        /// </summary>
        NonStrictGreaterThanComparison
    }
    
    /// <summary>
    ///     <p>The type of node this <c>AstNode</c> represents.</p>
    /// </summary>
    public Kind                           Type     { get; }
    /// <summary>
    ///     <p>The payload value that is stored by this <c>AstNode</c>.</p>
    /// </summary>
    public T                              Value    { get; }
    /// <summary>
    ///     <p>The children of this <c>AstNode</c>.</p>
    ///     <p>An <c>AstNode</c> is considered a <b>leaf node</b> if it has zero children.</p>
    /// </summary>
    public IReadOnlyList<AstNode<object?>> Children { get; }

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a leaf node.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> is a leaf node </returns>
    public bool IsLeafNode() =>
        Children.Count == 0;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a value type or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a value type </returns>
    public bool IsValue() =>
        Type is Kind.Value
             or Kind.Variable;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is an assignment statement or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes an assignment statement </returns>
    public bool IsAssignment() =>
        Type is Kind.Assignment;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a value type or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a value type </returns>
    public bool IsFunctionPlot() =>
        Type is Kind.PlotFunction;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a function definition or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a function definition </returns>
    public bool IsFunctionDefinition() =>
        Type is Kind.FunctionDefinition;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> carries a sequence of function arguments or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a sequence of function arguments </returns>
    public bool AreFunctionArgs() =>
        Type is Kind.FunctionArguments;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a function call or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a function call </returns>
    public bool IsFunctionCall() =>
        Type is Kind.FunctionCall;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a piecewise condition or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a piecewise condition </returns>
    public bool IsPiecewiseCondition() =>
        Type is Kind.PiecewiseCondition;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a function error or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a function error </returns>
    public bool IsFunctionError() =>
        Type is Kind.FunctionError;
    
    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a binary operation or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a binary operation </returns>
    public bool IsBinaryOperation() =>
        Type is Kind.BinaryAddition
             or Kind.BinarySubtraction
             or Kind.BinaryMultiplication
             or Kind.BinaryDivision
             or Kind.BinaryModulo
             or Kind.BinaryFloorDivision
             or Kind.BinaryExponent
             or Kind.BinaryOfComplex;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a unary operation or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a unary operation </returns>
    public bool IsUnaryOperation() =>
        Type is Kind.UnaryPositive
             or Kind.UnaryNegative
             or Kind.UnaryFactorial
             or Kind.UnaryAbsolute
             or Kind.UnaryGetReal
             or Kind.UnaryGetImaginary;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a comparison operation or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a comparison operation </returns>
    public bool IsComparison() =>
        Type is Kind.EqualityComparison
             or Kind.InequalityComparison
             or Kind.StrictLessThanComparison
             or Kind.StrictGreaterThanComparison
             or Kind.NonStrictLessThanComparison
             or Kind.NonStrictGreaterThanComparison;

    private AstNode(Kind type, T value, IReadOnlyList<AstNode<object?>> children) =>
        (Type, Value, Children) = (type, value, children);

    public override int GetHashCode() =>
        HashCode.Combine(Type, Value, Children);

    public override bool Equals(object? obj) =>
        obj is AstNode<T> other &&
        Type == other.Type && Value?.GetType() == other.Value?.GetType() && Children.Equals(other.Children);

    public override string ToString() =>
        $"AstNode[Type: {Type}, Stores: {(Value == null ? "None" : Value.GetType().Name)}, Children: {Children.Count}]";
}