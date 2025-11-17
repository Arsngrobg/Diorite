// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Error.fs
// Summary: The error system in Diorite
// Author:  Arsngrobg
// Version: v1.4
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

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
    type DioriteError =
        | MathError   of string // caused by division by zero for example
        | SyntaxError of string // caused by illegal tokens or illegal token pattern
        | SystemError of string // illegal state caused by external interop code

    /// <summary>
    ///     <p>A stricter version of the standard <c>Result</c> where it is strictly bound to the <c>DioriteError</c>
    ///        error type.
    ///     </p>
    /// </summary>
    type Result<'a> = Result<'a, DioriteError>

    /// <summary>
    ///     <p>Functional wrapper around a <c>Result</c> that contains a <c>MathError</c>.</p>
    /// </summary>
    /// <typeparam name='string'> the message to be display upon encountering this <c>MathError</c> </typeparam>
    /// <returns> a <c>MathError</c> wrapped in an <c>Error</c> case </returns>
    val inline MathError<'a>: string -> Result<'a>

    /// <summary>
    ///     <p>Functional wrapper around a <c>Result</c> that contains a <c>SyntaxError</c>.</p>
    /// </summary>
    /// <typeparam name='string'> the message to be display upon encountering this <c>SyntaxError</c> </typeparam>
    /// <returns> a <c>SyntaxError</c> wrapped in a <c>Error</c> case </returns>
    val inline SyntaxError<'a>: string -> Result<'a>

    /// <summary>
    ///     <p>Functional wrapper around a <c>Result</c> that contains a <c>SystemError</c>.</p>
    /// </summary>
    /// <typeparam name='string'> the message to be display upon encountering this error </typeparam>
    /// <returns> a <c>SystemError</c> wrapped in an <c>Error</c> case </returns>
    val inline SystemError<'a>: string -> Result<'a>

    /// <summary>
    ///     <p>Interprets the supplied generic <c>Result</c> as a <c>bool</c>.</p>
    ///     <code>
    ///         let result: Result = functionThatReturnsResult ()
    ///         IO.output $"success: {asBool(result)}\n" |> ignore
    ///     </code>
    /// </summary>
    /// <typeparam name='Result'> the <c>Result</c> </typeparam>
    /// <returns> <c>true</c> if <c>Ok</c>; <c>false</c> if an <c>Error</c> </returns>
    val inline resultAsBool: unit Result -> bool

    /// <summary>
    ///     <p>Safely unwraps the provided <c>result</c> by either returning the value wrapped by the
    ///        <c>Success</c> case, or the <i>alternative</i> value provided to this function.
    ///     </p>
    ///     <code>
    ///         let result: string = getOrElse (IO.input(Some "Enter something: ")) "hi"
    ///         IO.output result // either the user input or the string "hi"
    ///     </code>
    /// </summary>
    /// <typeparam name='Result'> the <c>Result</c> to be unwrapped </typeparam>
    /// <typeparam name="'a"> the alternative value to be returned if it was an <c>Error</c> </typeparam>
    /// <returns> either the value wrapped by the <c>Result</c> or the <c>alternative</c> value instead </returns>
    val inline getOrElse<'a>: 'a Result -> 'a -> 'a

    /// <summary>
    ///     <p>Coerces the provided <c>Result</c> bound by the generic type <c>'a</c> into a <c>unit</c> bound
    ///        <c>Result</c>.
    ///     </p>
    /// </summary>
    /// <typeparam name='Result'> the generic <c>Result</c> </typeparam>
    /// <returns> a nullified <c>Result</c> </returns>
    val inline generalized<'a>: 'a Result -> unit Result

    /// <summary>
    ///     <p>Forcefully unwraps the value within the supplied <c>Result</c>.</p>
    ///     <p>If there is no value present it will throw a <c>System.Exception</c>.</p>
    ///     <p><b>This is used mainly for quick debugging or cooking up a quick snippet of code for a showcase for
    ///        example.
    ///     </b></p>
    /// </summary>
    /// <typeparam name='Result'> the <c>Result</c> to unwrap </typeparam>
    /// <returns> the value stored within the <c>Result</c> if it was <c>Ok</c> </returns>
    /// <exception cref='System.Exception'> if the <c>Result</c> is an <c>Error</c> </exception>
    [<System.Obsolete("Do not use this - use getOrElse instead!")>]
    val forceUnwrap<'a>: 'a Result -> 'a