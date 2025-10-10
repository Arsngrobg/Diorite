// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Interpreter.fs
// Summary: The interpreter of for the Diorite language, which also includes a REPL
// Author:  Arsngrobg
// Version: v1.3
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>REPL</c> module is the functionality related to the live interpreter environment in the terminal.
///     It provides a neat and simple environment for writing <b>Diorite</b> mathematics code.
///     It exposes a singular function for initialisation.
///     <code>
///         let stable: bool = REPL.launch()
///         match stable with
///          | true  -> IO.output "REPL executed with no errors :)"    |> ignore
///          | false -> IO.output "REPL had an error during execution" |> ignore
///     </code>
/// </summary>
module REPL =
    /// <summary>
    ///     A binding that defines the title of the REPL when in use.
    /// </summary>
    /// <returns> the title of the REPL </returns>
    let title: string = $"{Identity.name} REPL (v{Version.languageVersion.ToString()})"

    // helper function to test a string to see if it is a blank line
    let private isBlankLine (line: string): bool =
        System.String.IsNullOrWhiteSpace line

    // initializes the console environment and hence the REPL environment.
    let private initialiseConsole (): bool =
        // execute batch operation
        let result: unit IO.Result = IO.compose [
            title                       |> IO.setConsoleTitle           |> IO.generalized
            None                        |> IO.clearConsole              |> IO.generalized
            System.ConsoleColor.Magenta |> IO.setConsoleBackgroundColor |> IO.generalized
            $"    {title} \n"           |> IO.output
            System.ConsoleColor.Black   |> IO.setConsoleBackgroundColor |> IO.generalized
        ]
        match result with
         | IO.Failure _ -> false
         | IO.Success _ -> true

    // processes the provided input from the user
    let private processInput (input: string): bool =
        // tokenize the input
        let lexResult: Lexer.Token list IO.Result = Lexer.lex input

        // defines what is output depending on the lexer result
        let noOutputIfNoTokens (): unit IO.Result =
            match lexResult with
             | IO.Failure err -> IO.compose [
                 System.ConsoleColor.Red |> IO.setConsoleForegroundColor |> IO.generalized;
                 IO.output $" X  {err}\n"
               ]
             | IO.Success tokens ->
                 match tokens with
                  | [] -> IO.Success () // do nothing
                  | _  -> IO.compose [
                      System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> IO.generalized;
                      IO.output $" ¦  {Lexer.tokens2str tokens}\n"
                    ]


        // partial for moving the cursor up or down by n units
        let moveCursorY: int -> (int * int) IO.Result = IO.moveCursorRelative 0

        // execute batch operation
        let result: unit IO.Result = IO.compose [
            -1                           |> moveCursorY                  |> IO.generalized
            System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> IO.generalized
            " |"                         |> IO.output
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> IO.generalized
            $"  {input}\n"               |> IO.output;
                                            noOutputIfNoTokens()
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> IO.generalized
        ]
        match result with
         | IO.Failure _ -> false
         | IO.Success _ -> true

    /// <summary>
    ///     Launches the REPL environment in the user's terminal.
    /// </summary>
    /// <returns> <c>true</c> if the REPL exited without error; <c>false</c> if a fatal error occurred </returns>
    let rec launch (): bool =
        let rec env (): bool =
            match IO.input(Some ">>> ") with
             | IO.Failure _     -> false
             | IO.Success input ->
                 match input with
                  | "@quit" -> true
                  | _       ->
                      match processInput input with
                       | true  -> env()
                       | false -> false

        // exit if initialisation failed
        match initialiseConsole() with
         | true  -> env()
         | false -> false
