// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Error.fs
// Summary: A module for containing the bindings and type declaration for within the error system in Diorite
// Author:  Arsngrobg
// Version: v1.4
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

// marked as AutoOpen as used across the entire project
[<AutoOpen>]
module Error =
    /// <summary>
    ///     A discriminated union type for an error in the <b>Diorite</b> language. Every error stores a message that
    ///     displays a descriptive message of what went wrong in the software. These are not <c>Exceptions</c> nor are
    ///     they thrown, however they need to be detected by the language in order to safely exit and return a valid
    ///     error code.
    /// </summary>
    type DioriteError =
        | MathError   of string // caused by division by zero for example
        | SyntaxError of string // caused by illegal tokens or illegal token pattern
        | SystemError of string // illegal state caused by external interop code

    /// <summary>
    ///     A stricter version of the standard <c>Result</c> where it is strictly bound to the <c>DioriteError</c> error
    ///     type.
    /// </summary>
    type Result<'a> = Result<'a, DioriteError>

    /// <summary>
    ///     Functional wrapper around a <c>Result</c> that contains a <c>MathError</c>.
    /// </summary>
    /// <param name='msg'> the message to be display upon encountering this <c>MathError</c> </param>
    /// <returns> a <c>MathError</c> wrapped in an <c>Error</c> case </returns>
    let inline MathError<'a> (msg: string): Result<'a> = Error (MathError msg)

    /// <summary>
    ///     Functional wrapper around a <c>Result</c> that contains a <c>SyntaxError</c>.
    /// </summary>
    /// <param name='msg'> the message to be display upon encountering this <c>SyntaxError</c> </param>
    /// <returns> a <c>SyntaxError</c> wrapped in a <c>Error</c> case </returns>
    let inline SyntaxError<'a> (msg: string): Result<'a> = Error (SyntaxError msg)

    /// <summary>
    ///     Functional wrapper around a <c>Result</c> that contains a <c>SystemError</c>.
    /// </summary>
    /// <param name='msg'> the message to be display upon encountering this error </param>
    /// <returns> a <c>SystemError</c> wrapped in an <c>Error</c> case </returns>
    let inline SystemError<'a> (msg: string): Result<'a> = Error (SystemError msg)

    /// <summary>
    ///     Interprets the supplied generic <c>Result</c> as a <c>bool</c>.
    ///     <code>
    ///         let result: Result = functionThatReturnsResult ()
    ///         IO.output $"success: {asBool(result)}\n" |> ignore
    ///     </code>
    /// </summary>
    /// <param name='result'> the <c>Result</c> </param>
    /// <returns> <c>true</c> if <c>Ok</c>; <c>false</c> if an <c>Error</c> </returns>
    let inline resultAsBool (result: unit Result): bool =
        match result with
         | Ok    _ -> true
         | Error _ -> false

    /// <summary>
    ///     Safely unwraps the provided <c>result</c> by either returning the value wrapped by the
    ///     <c>Success</c> case, or the <c>alternative</c> value provided to this function.
    ///     <code>
    ///         let result: string = getOrElse (IO.input(Some "Enter something: ")) "hi"
    ///         IO.output result // either the user input or the string "hi"
    ///     </code>
    /// </summary>
    /// <param name='result'> the <c>Result</c> to be unwrapped </param>
    /// <param name='alternative'> the alternative value to be returned if it was an <c>Error</c> </param>
    /// <returns> either the value wrapped by the <c>Result</c> or the <c>alternative</c> value instead </returns>
    let inline getOrElse<'a> (result: 'a Result) (alternative: 'a): 'a =
        match result with
         | Ok    value -> value
         | Error _     -> alternative

    /// <summary>
    ///     Coerces the provided <c>Result</c> bound by the generic type <c>'a</c> into a <c>unit</c> bound
    ///     <c>Result</c>.
    /// </summary>
    /// <param name='result'> the generic <c>Result</c> </param>
    /// <returns> a nullified <c>Result</c> </returns>
    let inline generalized<'a> (result: 'a Result): unit Result =
        match result with
         | Error e -> Error e
         | Ok    _ -> Ok    ()

    /// <summary>
    ///     Forcefully unwraps the value within the supplied <c>Result</c>.
    ///     If there is no value present it will throw an <c>System.Exception</c>.
    ///     <b>This is used mainly for quick debugging or cooking up a quick snippet of code for a showcase for
    ///        example.
    ///     </b>
    /// </summary>
    /// <param name='result'> the <c>Result</c> to unwrap </param>
    /// <returns> the value stored within the <c>Result</c> if it was <c>Ok</c> </returns>
    /// <exception cref='System.Exception'> if the <c>Result</c> is an <c>Error</c> </exception>
    [<System.Obsolete("Do not use this - use getOrElse instead!")>]
    let forceUnwrap<'a> (result: 'a Result): 'a =
        match result with
         | Error err   -> raise <| System.Exception $"Result failed{err}"
         | Ok    value -> value
