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
// Version: v1.5
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
[<RequireQualifiedAccess>]
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
        let result: unit Result = IO.compose [
            title                        |> IO.setConsoleTitle           |> generalized
            None                         |> IO.clearConsole              |> generalized
            System.ConsoleColor.DarkGray |> IO.setConsoleBackgroundColor |> generalized
            System.ConsoleColor.Black    |> IO.setConsoleForegroundColor |> generalized
            $"    {title} \n"            |> IO.output
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
            System.ConsoleColor.Black    |> IO.setConsoleBackgroundColor |> generalized
        ]
        match result with
         | Failure _ -> false
         | Success _ -> true

    // processes the provided input from the user
    let private processInput (input: string): bool =
        // tokenize the input
        let tokens: Lexer.TokenStream = Lexer.tokenize input

        // defines what is output depending on the lexer result
        let noOutputIfNoTokens (): unit Result =
            let error: DioriteError option = Lexer.getError tokens
            match error with
             | Some e -> IO.compose [
                 System.ConsoleColor.Red |> IO.setConsoleForegroundColor |> generalized;
                 IO.output $" X  {e}\n"
               ]
             | None ->
                  match tokens with
                   | [] -> Success () // do nothing
                   | _  -> IO.compose [
                      System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> generalized;
                      IO.output $" ¦  {Lexer.tokens2str tokens}\n"
                      IO.output $" ¦  {(Parser.parser >> Lexer.tokens2str) tokens}\n"
                      IO.output $" =  {(Parser.parser >> Parser.eval) tokens}\n"
                   ]

        // partial for moving the cursor up or down by n units
        let moveCursorY: int -> (int * int) Result = IO.moveCursorRelative 0

        // execute batch operation
        let result: unit Result = IO.compose [
            -1                           |> moveCursorY                  |> generalized
            System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> generalized
            " |"                         |> IO.output
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
            $"  {input}\n"               |> IO.output;
            ()                           |> noOutputIfNoTokens
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
        ]
        match result with
         | Failure _ -> false
         | Success _ -> true

    /// <summary>
    ///     Launches the REPL environment in the user's terminal.
    /// </summary>
    /// <returns> <c>true</c> if the REPL exited without error; <c>false</c> if a fatal error occurred </returns>
    let rec launch (): bool =
        let rec env (): bool =
            match IO.input(Some ">>> ") with
             | Failure _     -> false
             | Success input ->
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
