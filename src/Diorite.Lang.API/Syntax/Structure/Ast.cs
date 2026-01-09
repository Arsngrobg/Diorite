// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Ast.cs
// Summary: The type definition for the Ast type, which is a node in the Abstract Syntax Tree (AST) of a Diorite
//          program
// Author:  Arsngrobg, Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using System.Collections;
using System.Runtime;
using Microsoft.FSharp.Core;
using Diorite.Lang.API.Syntax.Data;
using Diorite.Lang.API.Syntax.Views;

namespace Diorite.Lang.API.Syntax.Structure;

/// <summary>
///     <p>The <c>Ast</c> type is a structured representation of <b>Diorite</b> code.</p>
///     <p>In short, it is the Abstract Syntax Tree (AST) of the provided source code.</p>
///     <p>It provides a rich set of methods for querying the structure of the node itself or the subtree.</p>
/// </summary>
public abstract class Ast : IEnumerable<Ast>
{
    /// <summary>
    ///     <p>Parses <c>source</c> into a collection <c>Ast</c> objects, which is held up by a root <c>Ast</c>
    ///        node.
    ///     </p>
    /// </summary>
    /// <param name="source"> <b>Diorite</b> source code to parse </param>
    /// <returns> the AST </returns>
    public static Ast TreeOf(string source) {
        var root  = Core.Parser.TopLevelParser.ParseString(source).ResultValue;
        var asApi = new BranchNode(
            Kind.Root,
            root.Select(OfCoreNode).ToArray()
        ); 
        return asApi;
    }

    private static Ast OfCoreNode(Core.Syntax.ASTNode coreNode)
    {
        switch (coreNode)
        {
            case Core.Syntax.ASTNode.Expression exp:
                return OfCoreExpression(exp.Item);
            case Core.Syntax.ASTNode.PlotFunction pltFn:
                var anonFn  = OfCoreExpression(pltFn.Item.expression);
                var anonArg = new ValueNode<object>(
                    Kind.FunctionArguments,
                    Variable.OfCoreType(pltFn.Item.parameter)
                );
                return new BranchNode(
                    Kind.PlotFunction,
                    [anonFn, anonArg]
                );
            case Core.Syntax.ASTNode.Assignment assign:
                var lValue = new ValueNode<object>(
                    Kind.Variable,
                    Variable.OfCoreType(assign.Item1)
                );
                var rValue = OfCoreExpression(assign.Item2);
                return new BranchNode(
                    Kind.Assignment,
                    [lValue, rValue]
                );
            case Core.Syntax.ASTNode.FunctionDefinition fnDef:
                var fnAttrs = new ValueNode<object>(
                    Kind.FunctionAttributes,
                     FunctionAttributes.OfCoreType(fnDef.Item.Item1)
                );
                var fnBody  = OfCoreFunctionBody(fnDef.Item.Item2);
                return new BranchNode(
                    Kind.FunctionDefinition,
                    [fnAttrs, fnBody]
                );
            default:
                throw new AmbiguousImplementationException($"Missing mapping for {coreNode.GetType()}");
        }
    }

    private static Ast OfCoreFunctionBody(Core.Syntax.FunctionBody coreBody)
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
                            new ValueNode<object>(
                                Kind.FunctionError,
                                FSharpOption<string>.get_IsNone(err.Item) ? string.Empty : err.Item.Value
                            ),
                        Core.Syntax.FunctionResult.Expression exp =>
                            OfCoreExpression(exp.Item),
                        _ => throw new AmbiguousImplementationException($"Missing mapping for {pwc.Item1.GetType()}")
                    };
                    
                    var left  = OfCoreExpression(pwc.Item2.Item1);
                    var right = OfCoreExpression(pwc.Item2.Item3);
                    var cmpOp = pwc.Item2.Item2;

                    Kind type;
                    if      (cmpOp.IsEquality)             type = Kind.EqualityComparison;
                    else if (cmpOp.IsInequality)           type = Kind.InequalityComparison;
                    else if (cmpOp.IsStrictLessThan)       type = Kind.StrictLessThanComparison;
                    else if (cmpOp.IsStrictGreaterThan)    type = Kind.StrictGreaterThanComparison;
                    else if (cmpOp.IsNonStrictLessThan)    type = Kind.NonStrictLessThanComparison;
                    else if (cmpOp.IsNonStrictGreaterThan) type = Kind.NonStrictGreaterThanComparison;
                    else throw new AmbiguousImplementationException($"Missing {cmpOp} mapping");
                    var comparisonNode = new BranchNode(type, [left, right]);

                    return new BranchNode(
                        Kind.PiecewiseCondition,
                        [resultNode, comparisonNode]
                    );
                }).ToArray();

                return new BranchNode(
                    Kind.PiecewiseConditions,
                    conditions
                );
            default:
                throw new AmbiguousImplementationException($"Missing mapping for {coreBody.GetType()}");
        }
    }

    private static Ast OfCoreExpression(Core.Syntax.Expression coreExpression)
    {
        Kind type;
        switch (coreExpression)
        {
            case Core.Syntax.Expression.Value value:
                return new ValueNode<object>(
                    Kind.Value,
                    Value.OfCoreType(value.Item)
                );
            case Core.Syntax.Expression.Variable variable:
                return new ValueNode<object>(
                    Kind.Variable,
                    Variable.OfCoreType(variable.Item)
                );
            case Core.Syntax.Expression.BinaryOperation binOp:
                var left  = OfCoreExpression(binOp.Item1);
                var right = OfCoreExpression(binOp.Item3);
                var bop   = binOp.Item2;
                
                if      (bop.IsAddition)       type = Kind.BinaryAddition;
                else if (bop.IsSubtraction)    type = Kind.BinarySubtraction;
                else if (bop.IsMultiplication) type = Kind.BinaryMultiplication;
                else if (bop.IsDivision)       type = Kind.BinaryDivision;
                else if (bop.IsModulo)         type = Kind.BinaryModulo;
                else if (bop.IsFloorDivision)  type = Kind.BinaryFloorDivision;
                else if (bop.IsExponent)       type = Kind.BinaryExponent;
                else if (bop.IsOfComplex)      type = Kind.BinaryOfComplex;
                else throw new AmbiguousImplementationException($"Missing {bop} mapping");

                return new BranchNode(type, [left, right]);
            case Core.Syntax.Expression.UnaryOperation unOp:
                var operand = OfCoreExpression(unOp.Item1);
                var uop     = unOp.Item2;
                
                if      (uop.IsPositive)     type = Kind.UnaryPositive;
                else if (uop.IsNegative)     type = Kind.UnaryNegative;
                else if (uop.IsFactorial)    type = Kind.UnaryFactorial;
                else if (uop.IsAbsolute)     type = Kind.UnaryAbsolute;
                else if (uop.IsGetReal)      type = Kind.UnaryGetReal;
                else if (uop.IsGetImaginary) type = Kind.UnaryGetImaginary;
                else throw new AmbiguousImplementationException($"Missing {uop} mapping");

                return new BranchNode(type, [operand]);
            case Core.Syntax.Expression.FunctionCall fnCall:
                var fnRef  = fnCall.Item1;
                var fnArgs = new ValueNode<object>(
                    Kind.FunctionArguments,
                    fnCall.Item2.Select(OfCoreExpression).ToArray()
                );

                var fnIdStr = fnRef.IsOfSymbol
                    ? ((Core.Syntax.FunctionReferenceType.OfSymbol) fnRef).Item
                    : Variable.OfCoreType(((Core.Syntax.FunctionReferenceType.OfVariable) fnRef).Item).ToString();
                var fnId = new ValueNode<object>(
                    Kind.FunctionIdentifier,
                    fnIdStr
                );

                return new BranchNode(
                    Kind.FunctionCall,
                    [fnId, fnArgs]
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
        ///     <p>The top-level type of <c>Ast</c>.</p>
        ///     <p>This is the root of the tree.</p>
        /// </summary>
        Root,
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
        ///     <p>A node representing a <c>FunctionReferenceType</c>.</p>
        ///     <p>It is usually paired with a <c>FunctionArguments</c> <c>Ast</c> case.</p>
        /// </summary>
        FunctionIdentifier,
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
        /// <summary>
        ///     <p>Function attributes.</p>
        /// </summary>
        FunctionAttributes,
        /// <summary>
        ///     <p>A sequence of <c>PiecewiseCondition</c>.</p>
        /// </summary>
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
    public Kind Type { get; }

    private Ast(Kind type) =>
        Type = type;

    /// <summary>
    ///     <p>Returns the value stored by this node.</p>
    ///     <p>If it does not carry a value or <c>T</c> does not match, then <c>null</c> is returned.</p>
    ///     <p><i>null here is a reliable value to check for a <c>ValueNode</c> as payload values are not allowed to be
    ///           <c>null</c>.
    ///     </i></p>
    /// </summary>
    /// <typeparam name="T"> the non-<c>null</c> type of the value </typeparam>
    /// <returns> the value carried by this node, if any </returns>
    public abstract T? GetValue<T>() where T : notnull;

    /// <summary>
    ///     <p>Returns the number of children this node has.</p>
    ///     <p><c>ValueNode</c>s have <c>0</c> children, and <c>BranchNode</c>s have <c>N</c> children. Where <c>N</c>
    ///        is non-zero.
    ///     </p>
    /// </summary>
    /// <returns> the number of children this node has </returns>
    public abstract int ChildCount();

    public abstract IEnumerator<Ast> GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator() =>
        GetEnumerator();

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a leaf node.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> is a leaf node </returns>
    public bool IsLeafNode() =>
        this is ValueNode<object>;

    /// <summary>
    ///     <p>Checks whether this <c>AstNode</c> is a root node or not.</p>
    /// </summary>
    /// <returns> if this <c>AstNode</c> describes a root node </returns>
    public bool IsRootNode() =>
        Type is Kind.Root;

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
        Type is Kind.PiecewiseConditions
             or Kind.PiecewiseCondition;

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

    /// <summary>
    ///     <p>Converts this <c>Ast</c> into a string representation of its recursive structure.</p>
    ///     <p>Recurses down each child, incrementing the level of indentation to match the tree depth.</p>
    /// </summary>
    /// <param name="indentLevel"> the visible depth level </param>
    /// <returns> the string representation of the structure of this <c>Ast</c> </returns>
    public string TreeStr(int indentLevel = 0)
    {
        var indent = new string(' ', indentLevel * 2);
        var result = $"{indent}{this}\n";
        foreach (var child in this) {
            result += child.TreeStr(indentLevel + 1);
        }
        return result;
    }

    public abstract override int    GetHashCode();
    public abstract override bool   Equals(object? obj);
    public abstract override string ToString();

    /// <summary>
    ///     <p>This is the case of <c>Ast</c> where it has child nodes.</p>
    ///     <p>You can check to see if a generic <c>Ast</c> is a <c>BranchNode</c> via the <c>false</c> result of the
    ///        <see cref="Ast.IsLeafNode()"/> method.
    ///     </p>
    /// </summary>
    private sealed class BranchNode : Ast
    {
        /// <summary>
        ///     <p>The ordered sequence of children this <c>BranchNode</c> branches to.</p>
        /// </summary>
        private IReadOnlyList<Ast> Children { get; }

        internal BranchNode(Kind type, IReadOnlyList<Ast> children) : base(type) =>
            Children = children;

        public override T? GetValue<T>() where T : default =>
            default;

        public override IEnumerator<Ast> GetEnumerator() =>
            Children.GetEnumerator();

        public override int ChildCount() =>
            Children.Count;

        public override int GetHashCode() =>
            HashCode.Combine(Type, Children);

        public override bool Equals(object? obj) =>
            obj is BranchNode other &&
            Type == other.Type && Children.SequenceEqual(other.Children);

        public override string ToString() =>
            $"AST[Type: {Type}, Children: {Children.Count}]";
    }

    /// <summary>
    ///     <p>This is the case of <c>Ast</c> where it has no children (leaf node) and carries a payload value of type
    ///        <c>T</c>, which cannot be <c>null</c>.
    ///     </p>
    ///     <p>You can check to see if a generic <c>Ast</c> is a <c>ValueNode</c> via the <see cref="Ast.IsLeafNode()"/>
    ///        method.
    ///     </p>
    /// </summary>
    /// <typeparam name="T"> the payload value of this <c>ValueNode</c> </typeparam>
    private sealed class ValueNode<T> : Ast where T : notnull
    {
        /// <summary>
        ///     <p>The payload value of this <c>ValueNode</c>.</p>
        ///     <p><i>It cannot be null.</i></p>
        /// </summary>
        private  T Value { get; }

        internal ValueNode(Kind type, T value) : base(type) =>
            Value = value;

        public override IEnumerator<Ast> GetEnumerator() =>
            Enumerable.Empty<Ast>().GetEnumerator();

        public override TU? GetValue<TU>() where TU : default =>
            Value is TU v
                ? v
                : default;

        public override int ChildCount() =>
            0;

        public override int GetHashCode() =>
            HashCode.Combine(Type, Value);

        public override bool Equals(object? obj) =>
            obj is ValueNode<T> other &&
            Type == other.Type && Value.Equals(other.Value);

        public override string ToString() =>
            Value is Array values
                ? $"AST[Type: {Type}, Values: {values.Length}]"
                : $"AST[Type: {Type}, Value: {Value}]";
    }
}
