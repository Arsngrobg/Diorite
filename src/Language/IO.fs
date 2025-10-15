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
// Version: v1.6
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
/// 	The <c>IO</c> module consists of functions that may have side effects and are deemed <i>unsafe</i>.
///     For example, <c>System.Console.ReadLine</c> is a method that has side effects (it may not allow for input all
///     the time). It allows for explicit handling of success and failure cases:
///     <code>
///         let result: unit IO.Result = IO.input "Enter your name: "
///         match result with
///          | IO.Success name -> IO.output $"Hello {name}!" |> ignore
///          | IO.Failure err  -> System.exception($"console broke {err}")
///     </code>
///     As shown above, we used <c>IO.output</c>. This is a wrapper around the <c>System.Console.Write</c> method. It is
///     also a side-effecting method so it can fail. However, in most cases you should be fine with using <c>ignore</c>
///     <code>
///         IO.output "Hello, World!" |> ignore // ignore the Result
///     </code>
///     Unsafe <c>IO</c> functions can be composed or chained together as if it were a single atomic operation that
///     produces an overall <c>Success</c> or <c>Failure</c> <c>IO.Result</c>.
///     <code>
///         let result: unit Result = IO.compose [
///             IO.output "Hello, World!"
///             IO.input  Some("Enter something: ") |> IO.generalize // throws away the output (does nothing)
///         ]
///         match result with
///          | Success _ -> IO.output  "Task Success!" |> ignore
///          | Failure e -> IO.output $"Error: {err}"  |> ignore
///     </code>
///     <c>IO.generalize</c> is a helper function for transforming the resulting generic result (<c>'a IO.Result</c>)
///     into the nullified <c>unit IO.result</c> type.
/// </summary>
[<RequireQualifiedAccess>]
module IO =
    /// <summary>
    ///     A discriminated union type for an error in the <b>Diorite</b> language. Every error stores a message that
    ///     displays a descriptive message of what went wrong in the software. These are not <c>Exceptions</c> nor are
    ///     they thrown, however they need to be detected by the language in order to safely exit and return a valid
    ///     error code.
    /// </summary>
    type DioriteError =
        | MathError   of string // caused by division by zero for example
        | SyntaxError of string // caused by illegal syntax
        | SystemError of string // illegal state caused by external interop code

    /// <summary>
    ///     A stricter version of the standard <c>Result</c> where it is strictly bound to the <c>DioriteError</c> error
    ///     type.
    /// </summary>
    type Result<'a> =
        | Success of 'a
        | Failure of DioriteError

    /// <summary>
    ///     Functional wrapper around the <c>IO.Result.Success</c> union type.
    ///     It can be referenced through the <c>IO</c> module over the <c>IO.Result</c> type.
    /// </summary>
    /// <param name='value'> the value to represent this <c>Result</c> </param>
    /// <returns> a <c>Success</c> case in the <c>Result</c> union type, containing the <c>value</c> </returns>
    let inline Success<'a> (value: 'a): Result<'a> = Success value

    /// <summary>
    ///     Functional wrapper around the <c>IO.Result.Failure</c> union type.
    ///     It can be referenced through the <c>IO</c> module over the <c>IO.Result</c> type.
    /// </summary>
    /// <param name='err'> the error to represent this <c>Result</c> </param>
    /// <returns> a <c>Failure</c> case in the <c>Result</c> union type, containing the <c>err</c> </returns>
    let inline Failure<'a> (err: DioriteError): Result<'a> = Failure err

    /// <summary>
    ///     Safely unwraps the provided <c>result</c> by either returning the value wrapped by the
    ///     <c>IO.Success</c> case, or the <c>alternative</c> value provided to this function.
    ///     <code>
    ///         let result: string = IO.getOrElse (IO.input(Some "Enter something: ")) "hi"
    ///         IO.output result
    ///     </code>
    /// </summary>
    /// <param name='result'> the <c>IO.Result</c> to be unwrapped </param>
    /// <param name='alternative'> the alternative value to be returned if it was a <c>IO.Failure</c> </param>
    /// <returns>
    ///     either the value wrapped by the <c>IO.Result</c> or the <c>alternative</c> value instead
    /// </returns>
    let getOrElse (result: 'a Result) (alternative: 'a): 'a =
        match result with
         | Success value -> value
         | Failure _     -> alternative

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
    [<System.Obsolete("Do not use this - use getOrElse instead!")>]
    let forceUnwrap<'a> (result: 'a Result): 'a =
        match result with
         | Success value -> value
         | Failure err   -> raise <| System.Exception $"Result failed{err}"

    // private helper for easily extracting errors from side-effecting interop code
    // it receives an 'unsafe' function that wraps an executable block of code that returns a generic value
    // it returns a custom Result discriminated union type depending on Success or Failure
    let inline private test<'a> (unsafe: unit -> 'a): 'a Result =
        try Success (unsafe ())
        with ex -> Failure ( SystemError $"{ex.GetType.ToString()}: {ex.Message}" )

    /// <summary>
    ///     Coerces the provided <c>IO.Result</c> bound by the generic type <c>'a</c> into a <c>unit</c> bound
    ///     <c>IO.Result</c>.
    /// </summary>
    /// <param name='result'> the generic <c>IO.Result</c> </param>
    /// <returns> a nullified <c>IO.Result</c> </returns>
    let inline generalized<'a> (result: 'a Result): unit Result =
        match result with
         | Failure e -> Failure e
         | Success _ -> Success ()

    /// <summary>
    ///     Computes the chain of actions from left to right.
    ///     Upon each the execution of each function, it checks whether the unsafe function produced a <c>IO.Failure</c>
    ///     result. If so, the <c>IO.Failure</c> is returned. A <c>IO.Success</c> is returned when all
    ///     <c>actions</c> have returned successful results. This allows for the chain of operations to be tested as if
    ///     it were a single atomic operation that produces a singular <c>IO.Result</c>.
    ///     <code>
    ///         let result: unit Result = IO.compose [
    ///             IO.output "Hello, World!"
    ///             IO.input  Some("Enter something: ") |> IO.generalize // throws away the output (does nothing)
    ///         ]
    ///         match result with
    ///          | Success _ -> IO.output  "Task Success!" |> ignore
    ///          | Failure e -> IO.output $"Error: {err}"  |> ignore
    ///     </code>
    /// </summary>
    /// <param name='actions'> the list of operations that produce <c>IO.Result</c>s </param>
    /// <returns> a singular <c>IO.Result</c> that determines the success state of the operation chain </returns>
    let rec compose (actions: unit Result list): unit Result =
        match actions with
         | []           -> Success ()
         | head :: tail ->
             match head with
              | Failure e -> Failure e
              | Success _ -> compose tail

    /// <summary>
    ///     Outputs the supplied <c>str</c> argument to the console.
    ///     This function call should include an explicit newline (<c>'\n'</c>) character, as this function does not
    ///     insert a newline character pre output.
    ///     <code>
    ///         let r: unit IO.Result = IO.output "Hello, World!" // output: "Hello, World!"
    ///         let success: bool = match r with
    ///          | Success _ -> true  // success = true
    ///          | Failure _ -> false // success = false
    ///     </code>
    /// </summary>
    /// <param name='str'> the string to output to the console </param>
    /// <returns> an <c>IO.Result</c> that may fail with a <c>System.IO.IOException</c> </returns>
    let output (str: string): unit Result =
        // writing to the standard output
        let unsafe (): unit =
            System.Console.Write str

        test <| unsafe

    /// <summary>
    ///     Reads the characters entered by the user in the console until a carriage return (<c>'\r'</c>), newline
    ///     (<c>'\n'</c>), or carriage return immediately followed by a newline (<c>"\r\n"</c>). The resulting string
    ///     returned contains all the characters until, and not including, the terminating character(s).
    ///     <code>
    ///         let r: string IO.Result = IO.input(Some ">>> ") // output: >>> _
    ///         match r with
    ///          | Success input -> IO.output $"The user entered: {input}" |> ignore
    ///          | Failure err   -> IO.output $"{err}"                     |> ignore
    ///                             // output: EXCEPTION_NAME: ERROR_MESSAGE
    ///     </code>
    /// </summary>
    /// <param name="prompt"> the optional prompt string to display to the user </param>
    /// <returns> an <c>IO.Result</c> that may contain the input string or the <c>Exception</c> it may throw </returns>
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
    /// <returns>
    ///     an <c>IO.Result</c> that may contain the new position of the cursor on the console or an error
    /// </returns>
    let moveCursorRelative (dx: int) (dy: int): (int * int) Result =
         let getAndSet (): int * int =
             let (x:  int), (y:  int) = match System.Console.GetCursorPosition() with x, y -> x, y
             let (nx: int), (ny: int) = (x + dx, y + dy)
             System.Console.SetCursorPosition(nx, ny)
             (nx, ny)

         test <| getAndSet

    /// <summary>
    ///     Sets the background color of the console.
    ///     <code>
    ///         let result: System.ConsoleColor IO.Result = setConsoleBackgroundColor System.ConsoleColor.White
    ///         match result with
    ///          | IO.Success c -> IO.output $"Set the background color to: {c}"     |> ignore
    ///          | IO.Failure _ -> IO.output "Could not change the background color" |> ignore
    ///     </code>
    /// </summary>
    /// <param name='color'> the color to set the console background to </param>
    /// <returns> an <c>IO.Result</c> that may contain the new background <c>System.ConsoleColor</c> </returns>
    let setConsoleBackgroundColor (color: System.ConsoleColor): System.ConsoleColor Result =
        // write & read
        let unsafe (): System.ConsoleColor =
            System.Console.BackgroundColor <- color
            System.Console.BackgroundColor

        test <| unsafe

    /// <summary>
    ///     Sets the foreground color of the console.
    ///     <code>
    ///         let result: System.ConsoleColor IO.Result = setBackgroundConsoleColor System.ConsoleColor.Black
    ///         match result with
    ///          | IO.Success c -> IO.output $"Set the foreground color to: {c}"     |> ignore
    ///          | IO.Failure _ -> IO.output "Could not change the foreground color" |> ignore
    ///     </code>
    /// </summary>
    /// <param name='color'> the color to set the console foreground to </param>
    /// <returns> an <c>IO.Result</c> that may contain the new foreground <c>System.ConsoleColor</c> </returns>
    let setConsoleForegroundColor (color: System.ConsoleColor): System.ConsoleColor Result =
        // write & read
        let unsafe (): System.ConsoleColor =
            System.Console.ForegroundColor <- color
            System.Console.ForegroundColor

        test <| unsafe

    /// <summary>
    ///     Sets the title of the console.
    ///     <code>
    ///         let result: string IO.Result = setConsoleTitle "Hello, World!"
    ///         match result with
    ///          | IO.Success t -> IO.output $"Set the title color to: {t}" |> ignore
    ///          | IO.Failure _ -> IO.output "Could not change the title"   |> ignore
    ///     </code>
    /// </summary>
    /// <param name='title'> the title to set the console title to </param>
    /// <returns> an <c>IO.Result</c> that may contain the new title <c>string</c> </returns>
    let setConsoleTitle (title: string): string Result =
        // write & read
        let unsafe (): string =
            System.Console.Title <- title
            System.Console.Title

        test <| unsafe

    /// <summary>
    ///     Clears the console with the optional <c>color</c> value.
    ///     If <c>None</c> is provided, then it will try and use the Console's background color or <b>Black</b>.
    ///     The background color is reset after clearing it, so no need to change it after the function call.
    ///     <code>
    ///         let result: System.ConsoleColor = clearConsole None // uses the current color or Black
    ///         match result with
    ///          | IO.Success c -> IO.output $"Cleared the console with the color: {c}"         |> ignore
    ///          | IO.Failure _ -> IO.output "Cleared the console with the default Black color" |> ignore
    ///     </code>
    /// </summary>
    /// <param name='color'> the color to clear the console with </param>
    /// <returns> an <c>IO.Result</c> that may contain the color in which the console was cleared with </returns>
    let clearConsole (color: System.ConsoleColor option): System.ConsoleColor Result =
        let unsafe (): System.ConsoleColor =
            // read the current background color
            let unsafeGrabBGColor (): System.ConsoleColor =
                System.Console.BackgroundColor // implicitly calls a getter that is unsafe

            let currentColor: System.ConsoleColor =
                (test <| unsafeGrabBGColor) |> getOrElse <| System.ConsoleColor.Black

            let newColor: System.ConsoleColor =
                match color with
                 | Some c -> c
                 | None   -> currentColor

            System.Console.BackgroundColor <- newColor
            System.Console.Clear()
            System.Console.BackgroundColor <- currentColor
            newColor

        test <| unsafe

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
        // read file
        let unsafe (): string =
            let fileReader: System.IO.StreamReader = new System.IO.StreamReader (path)
            let content = fileReader.ReadToEnd()
            fileReader.Close()
            content

        test <| unsafe
