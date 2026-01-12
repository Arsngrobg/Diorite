// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    DioriteEvaluator.cs
// Summary: The class definition for the DioriteEvaluator type, a stateful object that maintains a reference to a
//          memory state and evaluates against that
// Author:  Arsngrobg, Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using System.Runtime.CompilerServices;
using Diorite.Lang.API.Callbacks;
using Diorite.Lang.API.Syntax.Views;
using Diorite.Lang.Core.Parser;
using Diorite.Lang.Core.Syntax;
using Microsoft.FSharp.Collections;

namespace Diorite.Lang.API;

public sealed class DioriteEvaluator
{
    public static DioriteEvaluator Configure(Memory memory, PlotCallback callback)
    {
        var defaultWithCallback = Memory.FromDefaults(callback);
        defaultWithCallback.OverwriteWith(memory);
        return new DioriteEvaluator(defaultWithCallback.AsCoreType());
    }

    public static DioriteEvaluator OfNoState() =>
        Configure(Library.Library.OfNothing().BuildMemorySnapshot(), Memory.PlotDoNothing);

    private Core.Runtime.VirtualMemory.Memory _coreMemory;

    private DioriteEvaluator(Core.Runtime.VirtualMemory.Memory coreMemory) =>
        _coreMemory = coreMemory;

    /// <summary>
    ///     <p>Evaluates the supplied <b>Diorite</b> <c>source</c> code.</p>
    /// </summary>
    /// <param name="source"> the <b>Diorite</b> source code to evaluate </param>
    /// <returns> the sequence of <see cref="Value"/>s from the evaluated <b>Diorite</b> source code </returns>
    /// <exception cref="AggregateException"> if many <see cref="DioriteError"/>s occurs </exception>
    /// <exception cref="DioriteError"> if a single error occurs in parsing or evaluation </exception>
    public Value[] EvaluateSource(string source)
    {
        var tokens = Core.Lexer.Tokenizer.Tokenise(source);
        var errors = Core.Lexer.Tokenizer.GetTokenizerErrors(tokens).Select(DioriteError.OfCoreType)
                                                                    .Cast<Exception>()
                                                                    .ToArray();
        switch (errors.Length)
        {
            case 0:  break;
            case 1:  throw     errors[0];
            default: throw new AggregateException(errors);
        }

        var tree = Core.Parser.TopLevelParser.ParseTokens(tokens);
        if (tree.IsError)
            throw DioriteError.OfCoreType(tree.ErrorValue);

        var (results, memory) = Core.Runtime.Evaluation.EvalTree(tree.ResultValue, _coreMemory);
        _coreMemory = memory;
        return results.Select(r => r.IsOk
                                   ? Value.OfCoreType(r.ResultValue)
                                   : throw DioriteError.OfCoreType(r.ErrorValue)
                     ).ToArray();
    }

    /// <summary>
    ///     <p>Evaluates the supplied <see cref="Function"/> and returns the result of the function call.</p>
    /// </summary>
    /// <param name="function"> the <b>Diorite</b> <see cref="Function"/> to evaluate </param>
    /// <param name="arguments"> the <see cref="Value"/> arguments to supply to the function </param>
    /// <returns> the <see cref="Value"/> result of the <b>Diorite</b> function call </returns>
    /// <exception cref="DioriteError"> if the function call failed, or incorrect argument count </exception>
    public Value EvaluateFunction(Function function, params Value[] arguments)
    {
        var asExpressions = ListModule.OfSeq(
            arguments.Select(arg => Core.Syntax.Expression.NewValue(arg.AsCoreType()))
        );
        var (c, s) = function.FunctionAttributes.Identifier.AsCoreType();
        var withFunctionDef = Core.Runtime.VirtualMemory.SetVariable(
            _coreMemory,
            c, s,
            Core.Runtime.VirtualMemory.CellData.NewOfFunction(function.AsCoreType())
        );
        var result = Core.Runtime.Evaluation.EvalExpression(
            Core.Syntax.Expression.NewFunctionCall(
                Core.Syntax.FunctionReferenceType.NewOfVariable(function.FunctionAttributes.Identifier.AsCoreType()),
                asExpressions
            ),
            withFunctionDef
        );
        
        return result.IsOk
               ? Value.OfCoreType(result.ResultValue)
               : throw DioriteError.OfCoreType(result.ErrorValue);
    }

    /// <summary>
    ///     <p>Evaluates the given <c>expression</c> and returns the result upon finishing execution.</p>
    ///     <p>This method assumes the entire string is a <b>Diorite</b> expression statement with a semicolon
    ///        (<c>';'</c>) already appended to the end.
    ///     </p>
    /// </summary>
    /// <param name="expression"> the <b>Diorite</b> expression to evaluate </param>
    /// <returns> the resulting <c>Value</c> of the expression </returns>
    /// <exception cref="AggregateException"> if multiple <see cref="DioriteError"/>s occur</exception>
    /// <exception cref="DioriteError"> if a single error occurs in parsing or evaluation </exception>
    public Value EvaluateExpression(string expression)
    {
        var tokens = ListModule.OfSeq(Core.Lexer.Tokenizer.Tokenise(expression)
            .Select
                (t => t.id.Equals(Core.Syntax.TokenType.SemiColon)
                      ? new Core.Syntax.Token(
                          t.lexeme,
                          Core.Syntax.TokenType.IllegalToken,
                          Core.Syntax.TokenValue.None,
                          t.line,
                          t.column
                      )
                      : t
            ));
        var errors = Core.Lexer.Tokenizer.GetTokenizerErrors(tokens).Select(DioriteError.OfCoreType)
                                                                    .Cast<Exception>()
                                                                    .ToArray();
        switch (errors.Length)
        {
            case 0:  break;
            case 1:  throw     errors[0];
            default: throw new AggregateException(errors);
        }

        var lastToken = tokens.Reverse().Last();
        var endOfExp  = new Core.Syntax.Token(
            ";",
            Core.Syntax.TokenType.SemiColon,
            Core.Syntax.TokenValue.None,
            lastToken.line,
            lastToken.column + 1
        );

        var tree = Core.Parser.TopLevelParser.ParseTokens(ListModule.OfSeq(tokens.Append(endOfExp)));
        if (tree.IsError)
            throw DioriteError.OfCoreType(tree.ErrorValue);

        var (result, _) = Core.Runtime.Evaluation.EvalTree(tree.ResultValue, _coreMemory);
        if (result.Length > 1)
            throw new InvalidOperationException("Results of expression greater than one.");
        
        return result[0].IsOk
               ? Value.OfCoreType(result[0].ResultValue)
               : throw DioriteError.OfCoreType(result[0].ErrorValue);
    }
    
    /// <summary>
    /// <p>Checks if <c>input</c> expression contains an <see cref="ASTNode.FunctionDefinition"/>, and returns a
    /// new <see cref="Function"/> if it does.</p>
    /// </summary>
    /// <param name="input"> the <b>Diorite</b> expression to evaluate </param>
    /// <returns> a <see cref="Function"/> </returns>
    public static Function? ExtractFunction(string input) {
        Microsoft.FSharp.Collections.FSharpList<ASTNode> nodes = TopLevelParser.ParseString(input).ResultValue;
        foreach (var node in nodes) {
            if (node.IsFunctionDefinition) {
                ASTNode.FunctionDefinition functionDefinition = node as ASTNode.FunctionDefinition;
                Function function = Function.OfCoreType(functionDefinition.Item);
                return function;
            }
        }
        return null;
    }

    public Memory GetMemory() =>
        Memory.OfCoreType(_coreMemory);

    public override int GetHashCode() =>
        RuntimeHelpers.GetHashCode(this);

    public override bool Equals(object? obj) =>
        ReferenceEquals(this, obj);

    public override string ToString() =>
        $"Evaluator[Memory: {GetMemory()}]";
}
