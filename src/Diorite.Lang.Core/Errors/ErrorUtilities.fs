// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    ErrorUtilities.fs
// Summary: The utility functions for the redefined Result type in the Diorite project 
// Author:  Arsngrobg
// Version: v1.8
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Errors

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors

/// <summary>
///     <p>The utility functions for the redefined <c>Result</c> type in the <b>Diorite</b> project.</p>
/// </summary>
[<AutoOpen>]
module ErrorUtilities =
    /// <summary>
    ///     <p>Produces a <i>prettier</i> <c>string</c> representation of the supplied <c>DioriteError</c>.</p>
    /// </summary>
    /// <param name='error'> the <c>DioriteError</c> </param>
    /// <returns> the <c>string</c> representation of the supplied <c>DioriteError</c> </returns>
    let StrError (error: DioriteError): string =
        match error with
         | MathError   (Some msg, []         ) -> $"MathError: {msg}"
         | MathError   (None,     []         ) ->  "MathError"
         | MathError   (Some msg, inputs     ) -> $"MathError: {msg} - inputs: {inputs}"
         | MathError   (None,     inputs     ) -> $"MathError from input: {inputs}"
         | SyntaxError (msg,      Some (l, c)) -> $"SyntaxError: {msg} at line {l}, column {c}"
         | SyntaxError (msg,      None       ) -> $"SyntaxError: {msg}"

    /// <summary>
    ///     <p>A functional wrapper around a <c>Result</c> that contains a <c>MathError</c> with an optional and
    ///        meaningful message of the error, and the <c>input</c> that caused the <c>MathError</c>.
    ///     </p>
    /// </summary>
    /// <param name='msg'> the optional message to be display upon encountering this <c>MathError</c> </param>
    /// <param name='inputs'> the input values that caused this <c>MathError</c> </param>
    /// <returns> a <c>MathError</c> wrapped in an <c>Error</c> case </returns>
    let inline MathError<'a> (msg: string option) (inputs: ValueType list): 'a Result =
        (msg, inputs) |> (MathError >> Error)

    /// <summary>
    ///     <p>A functional wrapper around a <c>Result</c> that contains a <c>SyntaxError</c> with a meaningful message
    ///        of the error, including the <c>line</c> and <c>column</c> of the offending syntax.
    ///     </p>
    /// </summary>
    /// <param name='msg'> the message to be display upon encountering this <c>SyntaxError</c> </param>
    /// <param name='pos'> the optional position of the offending syntax </param>
    /// <typeparam name="'a"> the inferred type the <c>Result</c> should be bound to </typeparam>
    /// <returns> a <c>SyntaxError</c> wrapped in a <c>Error</c> case </returns>
    let inline SyntaxError<'a> (msg: string) (pos: (uint * uint) option): 'a Result =
        (msg, pos) |> (SyntaxError >> Error)

    /// <summary>
    ///     <p>Interprets the supplied generic <c>Result</c> as a <c>bool</c>.</p>
    ///     <p>This operation is destructive and hence any information stored in the <c>Result</c> will be lost.</p>
    /// </summary>
    /// <param name='result'> the <c>Result</c> </param>
    /// <typeparam name="'a"> the inferred type the <c>Result</c> should be bound to </typeparam>
    /// <returns> <c>true</c> if <c>Ok</c>; <c>false</c> if an <c>Error</c> </returns>
    let inline ResultAsBool<'a> (result: 'a Result): bool =
        match result with Ok _ -> true | Error _ -> false

    /// <summary>
    ///     <p>Attempts to safely unwrap the value within the supplied <c>Result</c> if is the <c>Ok</c> case.
    ///        If an <c>Error</c> case, the provided <c>alternative</c> value will be produced instead.
    ///     </p>
    /// </summary>
    /// <param name='result'> the <c>Result</c> to be unwrapped </param>
    /// <param name='alternative'> the alternative value to be returned if it was an <c>Error</c> </param>
    /// <typeparam name="'a"> the type of value that the supplied <c>Result</c> may contain </typeparam>
    /// <returns> either the value wrapped by the <c>Result</c> or the <c>alternative</c> value instead </returns>
    let inline GetOrElse<'a> (result: 'a Result) (alternative: 'a): 'a =
        match result with Ok value -> value | Error _ -> alternative

    /// <summary>
    ///     <p>Coerces the provided <c>Result</c> bound by the generic type <c>'a</c> into a <c>unit</c> bound
    ///        <c>Result</c>.
    ///     </p>
    /// </summary>
    /// <param name='result'> the generic <c>Result</c> </param>
    /// <typeparam name="'a"> the type that the supplied <c>Result</c> may contain </typeparam>
    /// <returns> a nullified <c>Result</c> </returns>
    let inline Generalized<'a> (result: 'a Result): unit Result =
        result |> Result.map (fun _ -> ())

    /// <summary>
    ///     <p>Forcefully unwraps the value within the supplied <c>Result</c>.</p>
    ///     <p>If there is no value present it will throw a <c>System.Exception</c>.</p>
    ///     <p><b>This is used mainly for quick debugging or cooking up a quick snippet of code for a showcase for
    ///        example.
    ///     </b></p>
    /// </summary>
    /// <param name='result'> the <c>Result</c> to unwrap </param>
    /// <typeparam name="'a"> the type that the supplied <c>Result</c> may contain </typeparam>
    /// <returns> the value stored within the <c>Result</c> if it was <c>Ok</c> </returns>
    /// <exception cref='System.Exception'> if the <c>Result</c> is an <c>Error</c> </exception>
    [<System.Obsolete("Do not use this - use GetOrElse instead!")>]
    let inline ForceUnwrap<'a> (result: 'a Result): 'a =
        match result with
         | Error err   -> raise <| System.Exception $"Result failed{err}"
         | Ok    value -> value
