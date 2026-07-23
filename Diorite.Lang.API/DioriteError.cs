// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    DioriteError.cs
// Summary: The type definition for the DioriteError type, which is an exception type in the Diorite API. Thrown by
//          methods that involve tokenising, parsing, and evaluating Diorite code
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Diorite.Lang.API.Syntax.Views;
using Diorite.Lang.API.Traits;
using Microsoft.FSharp.Core;

namespace Diorite.Lang.API;

/// <summary>
///     <p>The <c>DioriteError</c> class is an <see cref="Exception"/> type in the <b>Diorite</b> API.</p>
///     <p>This is an <see cref="Exception"/> which is thrown by methods that involve tokenising, parsing, and
///        evaluating.
///     </p>
/// </summary>
public abstract class DioriteError : Exception,
                                     ICoreConverter<Core.Errors.DioriteError, DioriteError>
{
    /// <summary>
    ///     <p>Creates a new <c>DioriteError</c> that describes a <c>SyntaxError</c> case.</p>
    ///     <p>It describes an error that was caused by illegal syntax, whether that be: illegal <c>Token</c>s, or an
    ///        illegal arrangement of <c>Token</c>s.
    ///     </p>
    ///     <p>It carries a concrete message and an ordered sequence of inputs (<see cref="Value"/>s) that caused this
    ///        error.
    ///     </p>
    /// </summary>
    /// <param name="message"> the concrete message this error should display upon error </param>
    /// <param name="position"> the position of the offending syntax </param>
    /// <returns> a <c>DioriteError</c>, specifically tailored a case where there is illegal syntax </returns>
    public static DioriteError OfSyntaxError(string message, Tuple<uint, uint>? position = null) =>
        new SyntaxError(message, position);

    /// <summary>
    ///     <p>Creates a new <c>DioriteError</c> that describes a <c>MathError</c> case.</p>
    ///     <p>It describes an error that was caused by a logical error, illegal operations, or unexpected values.</p>
    ///     <p>It carries a concrete message and an ordered sequence of inputs (<see cref="Value"/>s) that caused this
    ///        error.
    ///     </p>
    /// </summary>
    /// <param name="message"> the concrete message this error should display upon error </param>
    /// <param name="inputs"> the <see cref="Value"/>s that caused this error <i>(can be empty)</i>. </param>
    /// <returns> a <c>DioriteError</c>, specifically tailored a case where there is a mathematical error </returns>
    public static DioriteError OfMathError(string? message = null, params Value[] inputs) =>
        new MathError(message, inputs);

    public static DioriteError OfCoreType(Core.Errors.DioriteError coreError) => coreError switch
    {
        Core.Errors.DioriteError.SyntaxError syntaxError => OfSyntaxError(
            syntaxError.msg,
            FSharpOption<Tuple<uint, uint>>.get_IsNone(syntaxError.pos) ? null : syntaxError.pos.Value
        ),
        Core.Errors.DioriteError.MathError mathError => OfMathError(
            FSharpOption<string>.get_IsNone(mathError.msg) ? null : mathError.msg.Value,
            mathError.inputs.Select(Value.OfCoreType).ToArray()
        ),
        _ => throw new InvalidOperationException($"Invalid case {coreError.GetType()}")
    };

    /// <summary>
    ///     <p>Performs pattern matching on this <c>DioriteError</c>.</p>
    ///     <p>This is used to extract the additional values of the <c>DioriteError</c>.</p>
    ///     <p>For syntax errors, it's the position; for math error, it's the inputs.</p>
    /// </summary>
    /// <param name="onSyntaxError"> the callback to execute if this <c>DioriteError</c> is a <c>SyntaxError</c> </param>
    /// <param name="onMathError"> the callback to execute if this <c>DioriteError</c> is a <c>MathError</c> </param>
    /// <typeparam name="T"> the result of this pattern matching operation </typeparam>
    /// <returns> the result of the pattern match, bound by the type <c>T</c> </returns>
    public abstract T Match<T>(Func<Tuple<uint, uint>?, T> onSyntaxError, Func<Value[], T> onMathError);

    // caused by invalid syntax or incorrect sequence of tokens
    private sealed class SyntaxError(string message, Tuple<uint, uint>? position) : DioriteError
    {
        public override string Message =>
            position == null
            ? $"SyntaxError: {message}"
            : $"SyntaxError: {message} at line: {position.Item1}, column: {position.Item2}";

        public override T Match<T>(Func<Tuple<uint, uint>?, T> onSyntaxError, Func<Value[], T> _) =>
            onSyntaxError(position);
    }

    // caused by illegal math operations
    private sealed class MathError(string? message, params Value[] inputs) : DioriteError
    {
        public override string Message
        {
            get
            {
                if (inputs.Length == 0)
                {
                    return message == null
                           ?  "MathError"
                           : $"MathError: {message}";
                }

                var inputsStr = string.Join(", ", inputs.Select(i => i.ToString()).ToArray());
                return message == null
                       ? $"MathError from input: {inputsStr}"
                       : $"MathError: {message} - inputs: {inputsStr}";
            }
        }

        public override T Match<T>(Func<Tuple<uint, uint>?, T> _, Func<Value[], T> onMathError) =>
            onMathError(inputs);
    }
}