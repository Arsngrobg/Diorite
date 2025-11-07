// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    IO.fs
// Summary: Namespace consisting of IO functions that may have side effects
// Author:  Arsngrobg, Borngle
// Version: v1.9
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
/// 	The <c>IO</c> module consists of functions that may have side effects and are deemed <i>tryResult</i>.
///     For example, <c>System.Console.ReadLine</c> is a method that has side effects (it may not allow for input all
///     the time). It allows for explicit handling of success and failure cases:
///     <code>
///         let result: unit Result = IO.input "Enter your name: "
///         match result with
///          | Ok    name -> IO.output $"Hello {name}!" |> ignore
///          | Error err  -> System.exception($"console broke {err}")
///     </code>
///     As shown above, we used <c>IO.output</c>. This is a wrapper around the <c>System.Console.Write</c> method. It is
///     also a side-effecting method so it can fail. However, in most cases you should be fine with using <c>ignore</c>
///     <code>
///         IO.output "Hello, World!" |> ignore // ignore the Result
///     </code>
///     Unsafe <c>IO</c> functions can be composed or chained together as if it were a single atomic operation that
///     produces an overall <c>Ok</c> or <c>Error</c> <c>Result</c>.
///     <code>
///         let result: unit Result = IO.compose [
///             IO.output "Hello, World!"
///             IO.input  Some("Enter something: ") |> IO.generalize // throws away the output (does nothing)
///         ]
///         match result with
///          | Ok    _ -> IO.output  "Task Success!" |> ignore
///          | Error e -> IO.output $"Error: {err}"  |> ignore
///     </code>
///     <c>IO.generalize</c> is a helper function for transforming the resulting generic result (<c>'a Result</c>)
///     into the nullified <c>unit Result</c> type.
/// </summary>
[<RequireQualifiedAccess>]
module IO =
    // private helper for easily extracting errors from side-effecting operations
    // it receives an 'unsafe' function that wraps an executable block of code that returns a generic value
    // it returns a custom Result discriminated union type depending on Ok or Error
    let inline private tryAsResult<'a> (unsafe: unit -> 'a): 'a Result =
        try Ok (unsafe ())
        with ex -> SystemError $"{ex.GetType.ToString()}: {ex.Message}"

    /// <summary>
    ///     Computes the chain of actions from left to right.
    ///     Upon each the execution of each function, it checks whether the tryResult function produced an <c>Error</c>
    ///     result. If so, the <c>Error</c> is returned. A <c>Ok</c> is returned when all <c>actions</c> have returned
    ///     successful results. This allows for the chain of operations to be tested as if it were a single atomic
    ///     operation that produces a singular <c>Result</c>.
    ///     <code>
    ///         let result: unit Result = IO.compose [
    ///             IO.output "Hello, World!"
    ///             IO.input  Some("Enter something: ") |> IO.generalize // throws away the output (does nothing)
    ///         ]
    ///         match result with
    ///          | Ok    _ -> IO.output  "Task Success!" |> ignore
    ///          | Error e -> IO.output $"Error: {err}"  |> ignore
    ///     </code>
    /// </summary>
    /// <param name='actions'> the list of operations that produce <c>Result</c>s </param>
    /// <returns> a singular <c>Result</c> that determines the success state of the operation chain </returns>
    let rec compose (actions: unit Result list): unit Result =
        match actions with
         | []           -> Ok ()
         | head :: tail ->
             match head with
              | Error e -> Error e
              | Ok    _ -> compose tail

    /// <summary>
    ///     Outputs the supplied <c>str</c> argument to the console.
    ///     This function call should include an explicit newline (<c>'\n'</c>) character, as this function does not
    ///     insert a newline character pre output.
    ///     <code>
    ///         let r: unit Result = IO.output "Hello, World!" // output: "Hello, World!"
    ///         let success: bool = match r with
    ///          | Ok    _ -> true  // success = true
    ///          | Error _ -> false // success = false
    ///     </code>
    /// </summary>
    /// <param name='str'> the string to output to the console </param>
    /// <returns> an <c>Result</c> that may fail with a <c>System.IO.IOException</c> </returns>
    let output (str: string): unit Result =
        // writing to the standard output
        let unsafe (): unit =
            System.Console.Write str

        tryAsResult <| unsafe

    /// <summary>
    ///     Reads the characters entered by the user in the console until a carriage return (<c>'\r'</c>), newline
    ///     (<c>'\n'</c>), or carriage return immediately followed by a newline (<c>"\r\n"</c>). The resulting string
    ///     returned contains all the characters until, and not including, the terminating character(s).
    ///     <code>
    ///         let r: string Result = IO.input(Some ">>> ") // output: >>> _
    ///         match r with
    ///          | OK    input -> IO.output $"The user entered: {input}" |> ignore
    ///          | Error err   -> IO.output $"{err}"                     |> ignore
    ///                           // output: EXCEPTION_NAME: ERROR_MESSAGE
    ///     </code>
    /// </summary>
    /// <param name='prompt'> the optional prompt string to display to the user </param>
    /// <returns> an <c>Result</c> that may contain the input string or the <c>Exception</c> it may throw </returns>
    let input (prompt: string option): string Result =
        match prompt with
         | Some(p) -> output p  |> ignore
         | None    -> output "" |> ignore

        tryAsResult <| System.Console.ReadLine

    /// <summary>
    ///     Shifts the cursor position in the console by the <c>dx</c> and <c>dy</c> values and returns the new position
    ///     of the cursor.
    ///     <b>NOTE</b>: <i>(0, 0) is at the top-left of the console</i>
    ///     <code>
    ///         // assume cursor starts at (0, 0)
    ///         let newPos: (int * int) Result = IO.moveCursorRelative 1 1
    ///         match newPos with
    ///          | OK   (x, y) -> IO.output $"({x}, {y})"                     |> ignore // output: "(1, 1)"
    ///          | Error _     -> IO.output "Could not get console position." |> ignore
    ///     </code>
    /// </summary>
    /// <param name='dx'> the amount to move along the x-axis </param>
    /// <param name='dy'> the amount to move along the y-axis </param>
    /// <returns>
    ///     an <c>Result</c> that may contain the new position of the cursor on the console or an error
    /// </returns>
    let moveCursorRelative (dx: int) (dy: int): (int * int) Result =
         let getAndSet (): int * int =
             let (x:  int), (y:  int) = match System.Console.GetCursorPosition() with x, y -> x, y
             let (nx: int), (ny: int) = (x + dx, y + dy)
             System.Console.SetCursorPosition(nx, ny)
             (nx, ny)

         tryAsResult <| getAndSet

    /// <summary>
    ///     Sets the background color of the console.
    ///     <code>
    ///         let result: System.ConsoleColor Result = setConsoleBackgroundColor System.ConsoleColor.White
    ///         match result with
    ///          | Ok    c -> IO.output $"Set the background color to: {c}"     |> ignore
    ///          | Error _ -> IO.output "Could not change the background color" |> ignore
    ///     </code>
    /// </summary>
    /// <param name='color'> the color to set the console background to </param>
    /// <returns> an <c>Result</c> that may contain the new background <c>System.ConsoleColor</c> </returns>
    let setConsoleBackgroundColor (color: System.ConsoleColor): System.ConsoleColor Result =
        // write & read
        let unsafe (): System.ConsoleColor =
            System.Console.BackgroundColor <- color
            System.Console.BackgroundColor

        tryAsResult <| unsafe

    /// <summary>
    ///     Sets the foreground color of the console.
    ///     <code>
    ///         let result: System.ConsoleColor Result = setBackgroundConsoleColor System.ConsoleColor.Black
    ///         match result with
    ///          | Ok    c -> IO.output $"Set the foreground color to: {c}"     |> ignore
    ///          | Error _ -> IO.output "Could not change the foreground color" |> ignore
    ///     </code>
    /// </summary>
    /// <param name='color'> the color to set the console foreground to </param>
    /// <returns> an <c>Result</c> that may contain the new foreground <c>System.ConsoleColor</c> </returns>
    let setConsoleForegroundColor (color: System.ConsoleColor): System.ConsoleColor Result =
        // write & read
        let unsafe (): System.ConsoleColor =
            System.Console.ForegroundColor <- color
            System.Console.ForegroundColor

        tryAsResult <| unsafe

    /// <summary>
    ///     Sets the title of the console.
    ///     <code>
    ///         let result: string Result = setConsoleTitle "Hello, World!"
    ///         match result with
    ///          | Ok    t -> IO.output $"Set the title color to: {t}" |> ignore
    ///          | Error _ -> IO.output "Could not change the title"   |> ignore
    ///     </code>
    /// </summary>
    /// <param name='title'> the title to set the console title to </param>
    /// <returns> an <c>Result</c> that may contain the new title <c>string</c> </returns>
    let setConsoleTitle (title: string): string Result =
        // write & read
        let unsafe (): string =
            System.Console.Title <- title
            System.Console.Title

        tryAsResult <| unsafe

    /// <summary>
    ///     Clears the console with the optional <c>color</c> value.
    ///     If <c>None</c> is provided, then it will try and use the Console's background color or <b>Black</b>.
    ///     The background color is reset after clearing it, so no need to change it after the function call.
    ///     <code>
    ///         let result: System.ConsoleColor = clearConsole None // uses the current color or Black
    ///         match result with
    ///          | Ok    c -> IO.output $"Cleared the console with the color: {c}"         |> ignore
    ///          | Error _ -> IO.output "Cleared the console with the default Black color" |> ignore
    ///     </code>
    /// </summary>
    /// <param name='color'> the color to clear the console with </param>
    /// <returns> an <c>Result</c> that may contain the color in which the console was cleared with </returns>
    let clearConsole (color: System.ConsoleColor option): System.ConsoleColor Result =
        let unsafe (): System.ConsoleColor =
            // read the current background color
            let unsafeGrabBGColor (): System.ConsoleColor =
                System.Console.BackgroundColor // implicitly calls a getter that is tryResult

            let currentColor: System.ConsoleColor =
                (tryAsResult <| unsafeGrabBGColor) |> getOrElse <| System.ConsoleColor.Black

            let newColor: System.ConsoleColor =
                match color with
                 | Some c -> c
                 | None   -> currentColor

            System.Console.BackgroundColor <- newColor
            System.Console.Clear()
            System.Console.BackgroundColor <- currentColor
            newColor

        tryAsResult <| unsafe

    /// <summary>
    ///     Eagerly reads the contents of the supplied file derived from the <c>path</c> argument.
    ///     <code>
    ///         let r: string Result = IO.readFile "example.diorite"
    ///         match r with
    ///          | OK    file -> IO.output $"{file}" |> ignore // output: FILE_CONTENTS
    ///          | Error err  -> IO.output $"{err}"  |> ignore // output: EXCEPTION_NAME: ERROR_MESSAGE
    ///     </code>
    /// </summary>
    /// <param name='path'> the relative or absolute file path to the file to be read </param>
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

        tryAsResult <| unsafe

    /// <summary>
    ///     Writes the <c>contents</c> to the desired <c>directory</c> with the <c>fileName</c>.
    /// </summary>
    /// <param name='fileName'> name of the <c>.diorite</c> file to be written </param>
    /// <param name='directory'> location the <c>.diorite</c> file is written to </param>
    /// <param name='contents'> text contents of the file </param>
    /// <returns> an empty <c>Result</c> which indicates if the write operation was a success or failure </returns>
    let writeFile (fileName: string) (directory: string) (contents: string): unit Result =
        let unsafe (): unit =
            let path =
                match directory with
                | null | "" -> "." // Project folder as default for now
                | _ -> directory
            let filePath = System.IO.Path.Combine(path, fileName + Properties.fileExtension)
            use fileWriter = new System.IO.StreamWriter(filePath, false) // false = overwrite, true = append
            fileWriter.WriteLine(contents)
        
        tryAsResult <| unsafe
                