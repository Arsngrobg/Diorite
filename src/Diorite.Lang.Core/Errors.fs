// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Error.fs
// Summary: The error handling system for Diorite
// Author:  Arsngrobg
// Version: v1.11
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The <c>Errors</c> module consist of the error handling system within the <b>Diorite</b> language.</p>
///     <p>Its main focus is to override the default <c>FSHarp.Core.Result</c> type into a custom <c>Result</c> type
///        that is strictly bound to the <c>DioriteError</c> as its <c>Error</c> case.
///     </p>
/// </summary>
// marked as AutoOpen as used across the entire project
[<AutoOpen>]
module Errors =
    /// <summary>
    ///     <p>A discriminated union type for an error in the <b>Diorite</b> language. Every error stores a message that
    ///        displays a descriptive message of what went wrong in the software. These are not <c>Exceptions</c> nor
    ///        are they thrown, however they need to be detected by the language in order to safely exit and return a
    ///        valid error code.
    ///     </p>
    /// </summary>
    /// <typeparam name='string'> the error message </typeparam>
    type DioriteError =
        /// <summary>
        ///     <p>Caused by illegal math operations.</p>
        ///     <p><i>Example: division by zero</i></p>
        /// </summary>
        | MathError   of string
        /// <summary>
        ///     <p>Caused by invalid syntax or incorrect sequence of tokens.</p>
        /// </summary>
        | SyntaxError of string

    /// <summary>
    ///     <p>Produces a <i>prettier</i> <c>string</c> representation of the supplied <c>DioriteError</c>.</p>
    /// </summary>
    /// <param name='error'> the <c>DioriteError</c> </param>
    /// <returns> the <c>string</c> representation of the supplied <c>DioriteError</c> </returns>
    let strError (error: DioriteError): string =
        match error with
         | MathError   msg -> $"MathError: {msg}"
         | SyntaxError msg -> $"SyntaxError: {msg}"

    /// <summary>
    ///     <p>A stricter version of the standard <c>FSHarp.Core.Result</c> where its <c>Error</c> case is strictly
    ///        bound to the <c>DioriteError</c> type.
    ///     </p>
    ///     <p>This should be used over the standard <c>Result</c> type.</p>
    /// </summary>
    type internal Result<'a> = Result<'a, DioriteError>

    /// <summary>
    ///     <p>The globally-defined operator for chaining successful <c>Result</c> types by applying callback functions
    ///        on them.
    ///     </p>
    ///     <p>If the supplied <c>Result</c> matches the <c>Ok</c> case, then the registered <c>fn</c> function is
    ///        applied to it.
    ///     </p>
    /// </summary>
    /// <param name='result'> the result to apply to this </param>
    /// <param name='fn'> the callback function to apply on the successful value stored in the <c>Result</c> </param>
    /// <typeparam name="'a"> the type of value stored in the original <c>Result</c> type </typeparam>
    /// <typeparam name="'b"> the type of value that the callback function <c>fn</c> returns </typeparam>
    /// <returns> a value of type <c>'b</c> if the <c>Result</c> is the <c>Ok</c> case </returns>
    let inline internal (?=>) (result: 'a Result) (fn: 'a -> 'b Result): 'b Result =
        match result with
         | Error err   -> Error err
         | Ok    value -> fn value

    /// <summary>
    ///     <p>The implementation of the <b>bind</b> operator for <c>Result</c>s.</p>
    ///     <p>It binds two functions together to act as a single atomic unit of operation.</p>
    /// </summary>
    /// <param name='a'> the first function </param>
    /// <param name='b'> the second function </param>
    /// <typeparam name="'a"> the type accepted by the <c>a</c> function </typeparam>
    /// <typeparam name="'b"> the type accepted by the <c>b</c> function </typeparam>
    /// <typeparam name="'c"> the type returned by the <c>b</c> function </typeparam>
    /// <returns> a function that accepts <c>'a</c>, and returns <c>'c Result</c> </returns>
    let internal (>>=) (a: 'a -> 'b Result) (b: 'b -> 'c Result): 'a -> 'c Result =
        (fun _a -> (a _a ?=> b))

    /// <summary>
    ///     <p>A functional wrapper around a <c>Result</c> that contains a <c>MathError</c> with a meaningful message
    ///        of the error.
    ///     </p>
    /// </summary>
    /// <param name='msg'> the message to be display upon encountering this <c>MathError</c> </param>
    /// <returns> a <c>MathError</c> wrapped in an <c>Error</c> case </returns>
    let inline internal MathError<'a> (msg: string): 'a Result =
        msg |> (MathError >> Error)

    /// <summary>
    ///     <p>A functional wrapper around a <c>Result</c> that contains a <c>SyntaxError</c> with a meaningful message
    ///        of the error.
    ///     </p>
    /// </summary>
    /// <param name='msg'> the message to be display upon encountering this <c>SyntaxError</c> </param>
    /// <typeparam name="'a"> the inferred type the <c>Result</c> should be bound to </typeparam>
    /// <returns> a <c>SyntaxError</c> wrapped in a <c>Error</c> case </returns>
    let inline internal SyntaxError<'a> (msg: string): 'a Result =
        msg |> (SyntaxError >> Error)

    /// <summary>
    ///     <p>Interprets the supplied generic <c>Result</c> as a <c>bool</c>.</p>
    ///     <p>This operation is destructive and hence any information stored in the <c>Result</c> will be lost.</p>
    /// </summary>
    /// <param name='result'> the <c>Result</c> </param>
    /// <typeparam name="'a"> the inferred type the <c>Result</c> should be bound to </typeparam>
    /// <returns> <c>true</c> if <c>Ok</c>; <c>false</c> if an <c>Error</c> </returns>
    let inline internal resultAsBool<'a> (result: 'a Result): bool =
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
    let inline internal getOrElse<'a> (result: 'a Result) (alternative: 'a): 'a =
        match result with Ok value -> value | Error _ -> alternative

    /// <summary>
    ///     <p>Coerces the provided <c>Result</c> bound by the generic type <c>'a</c> into a <c>unit</c> bound
    ///        <c>Result</c>.
    ///     </p>
    /// </summary>
    /// <param name='result'> the generic <c>Result</c> </param>
    /// <typeparam name="'a"> the type that the supplied <c>Result</c> may contain </typeparam>
    /// <returns> a nullified <c>Result</c> </returns>
    let inline internal generalized<'a> (result: 'a Result): unit Result =
        result ?=> (fun _ -> Ok ())

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
    [<System.Obsolete("Do not use this - use getOrElse instead!")>]
    let inline internal forceUnwrap<'a> (result: 'a Result): 'a =
        match result with
         | Error err   -> raise <| System.Exception $"Result failed{err}"
         | Ok    value -> value