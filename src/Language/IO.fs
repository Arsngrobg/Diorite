// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    IO.fs
// Summary: Module consisting of functions that may have side effects and ways of handling side effects
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
/// 	The <c>IO</c> module consists of functions that may have side effects and are deemed <i>unsafe</i>.
/// </summary>
module IO =
    /// <summary>
    ///     A discriminated union type for an error in the <b>Diorite</b> language. Every error stores a message that
    ///     displays a descriptive message of what went wrong in the software. These are not <c>Exceptions</c> nor are
    ///     they thrown, however they need to be detected by the language in order to safely exit and return a valid
    ///     error code.
    /// </summary>
    type DioriteError =
        | MathError     of string // caused by division by zero for example
        | LexerError    of string // illegal token recognised
        | ParseError    of string // illegal sequence of tokens
        | ExternalError of string // illegal state caused by interop code

    /// <summary>
    ///     A stricter version of the standard <c>Result</c> where it is strictly bound to the <c>DioriteError</c> error
    ///     type.
    /// </summary>
    type Result<'T> =
        | Success of 'T
        | Failure of DioriteError

    /// <summary>
    ///     Functional wrapper around the <c>IO.Result.Success</c> union type.
    ///     It can be referenced through the <c>IO</c> module over the <c>IO.Result</c> type.
    /// </summary>
    /// <param name='value'> the value to represent this <c>Result</c> </param>
    /// <returns> a <c>Success</c> case in the <c>Result</c> union type, containing the <c>value</c> </returns>
    let inline Success<'T> (value: 'T): Result<'T> = Success value

    /// <summary>
    ///     Functional wrapper around the <c>IO.Result.Failure</c> union type.
    ///     It can be referenced through the <c>IO</c> module over the <c>IO.Result</c> type.
    /// </summary>
    /// <param name='err'> the error to represent this <c>Result</c> </param>
    /// <returns> a <c>Failure</c> case in the <c>Result</c> union type, containing the <c>err</c> </returns>
    let inline Failure<'T> (err: DioriteError): Result<'T> = Failure err

    /// <summary>
    ///     Forcefully unwraps the value within the supplied <c>Result</c>.
    ///     If there is no value present it will throw an <c>System.Exception</c>.
    ///     <b>This is used mainly for quick debugging or cooking up a quick snippet of code for a showcase for
    ///        example.
    ///     </b>
    /// </summary>
    /// <param name='result'> the <c>Result</c> to unwrap </param>
    /// <returns> the value stored within the <c>Result</c> if it was a <c>Success</c> </returns>
    /// <exception cref='System.Exception'> if the <c>Result</c> is a <c>Failure</c> </exception>
    [<System.Obsolete("Do not use this unless you are aware that this is very unsafe!")>]
    let forceUnwrap<'T> (result: 'T Result): 'T =
        match result with
         | Success value -> value
         | Failure err   -> raise <| System.Exception $"{err}"

    // private helper for easily extracting errors from side-effecting interop code
    // it receives an 'unsafe' function that wraps an executable block of code that returns a generic value
    // it returns a custom Result discriminated union type depending on Success or Failure
    let inline private test<'T> (unsafe: Unit -> 'T): Result<'T> =
        try Success (unsafe ())
        with ex -> Failure ( ExternalError $"{ex.GetType.ToString()}: {ex.Message}" )

    /// <summary>
    ///     Outputs the supplied <c>str</c> argument to the console.
    ///     This function call should include an explicit newline (<c>'\n'</c>) character, as this function does not
    ///     insert a newline character pre output.
    ///     <code>
    ///         let r: Unit IO.Result = IO.output "Hello, World!" // output: "Hello, World!"
    ///         let success: bool = match r with
    ///          | Success _ -> true  // success = true
    ///          | Failure _ -> false // success = false
    ///     </code>
    /// </summary>
    /// <param name='str'> the string to output to the console </param>
    /// <returns> a <c>Result</c> that may fail with a <c>System.IO.IOException</c> </returns>
    let output (str: string): Unit Result =
        test <| (fun () -> System.Console.Write str)

    /// <summary>
    ///     Reads the characters entered by the user in the console until a carriage return (<c>'\r'</c>), newline
    ///     (<c>'\n'</c>), or carriage return immediately followed by a newline (<c>"\r\n"</c>). The resulting string
    ///     returned contains all the characters until, and not including, the terminating character(s).
    ///     <code>
    ///         let r: string IO.Result = IO.input ">>> " // output: >>> _
    ///         match r with
    ///          | Success input -> IO.output $"The user entered: {input}" |> ignore
    ///          | Failure err   -> IO.output $"{err}"                     |> ignore
    ///                             // output: EXCEPTION_NAME: ERROR_MESSAGE
    ///     </code>
    /// </summary>
    /// <param name="prompt"> the optional prompt string to display to the user </param>
    /// <returns> a <c>Result</c> that may contain the input string or the <c>Exception</c> it may throw </returns>
    let input (prompt: string option): string Result =
        match prompt with
         | Some(p) -> output p  |> ignore
         | None    -> output "" |> ignore

        test <| System.Console.ReadLine

    /// <summary>
    ///     Shifts the cursor position in the console by the <c>dx</c> and <c>dy</c> values and returns the new position
    ///     of the cursor.
    ///     <b>NOTE</b>: <i>(0, 0) is at the top-left of the console</i>
    ///     <code>
    ///         // assume cursor starts at (0, 0)
    ///         let newPos: (int * int) Result = IO.moveCursorRelative 1 1
    ///         match newPos with
    ///          | Success (x, y) -> IO.output $"({x}, {y})"                     |> ignore // output: "(1, 1)"
    ///          | Failure _      -> IO.output "Could not get console position." |> ignore
    ///     </code>
    /// </summary>
    /// <param name="dx"> the amount to move along the x-axis </param>
    /// <param name="dy"> the amount to move along the y-axis </param>
    /// <returns> a <c>Result</c> that may contain the new position of the cursor on the console or an error </returns>
    let moveCursorRelative (dx: int) (dy: int): (int * int) Result =
         let getAndSet (): int * int =
             let (x:  int), (y:  int) = match System.Console.GetCursorPosition() with (x, y) -> x, y
             let (nx: int), (ny: int) = (x + dx, y + dy)
             System.Console.SetCursorPosition(nx, ny)
             (nx, ny)

         test <| getAndSet

    /// <summary>
    ///     Eagerly reads the contents of the supplied file derived from the <c>path</c> argument.
    ///     <code>
    ///         let r: string IO.Result = IO.readFile "example.diorite"
    ///         match r with
    ///          | Success file -> IO.output $"{file}" |> ignore // output: FILE_CONTENTS
    ///          | Failure err  -> IO.output $"{err}"  |> ignore // output: EXCEPTION_NAME: ERROR_MESSAGE
    ///     </code>
    /// </summary>
    /// <param name="path"> the relative or absolute file path to the file to be read </param>
    /// <returns>
    ///     a <c>Result</c> that may contain the file contents as a complete <c>string</c> or an <c>Exception</c>
    /// </returns>
    let readFile (path: string): string Result =
        // unsafe code
        let load (): string =
            let fileReader: System.IO.StreamReader = new System.IO.StreamReader (path)
            let content = fileReader.ReadToEnd()
            fileReader.Close()
            content

        test <| load
