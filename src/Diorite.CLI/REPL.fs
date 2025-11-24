// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    REPL.fs
// Summary: The logic for running the REPL environment in the user's terminal
// Author:  Arsngrobg, Borngle
// Version: v1.9
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.CLI

open Diorite.Lang.Core

/// <summary>
///     <p>The <c>REPL</c> module is the functionality related to the live interpreter environment in the terminal.</p>
///     <p>It provides a neat and simple environment for writing <b>Diorite</b> mathematics source code, and exposes a
///        singular function for initialisation (<c>REPL.launch</c>).
///     </p>
/// </summary>
module REPL =
    /// <summary>
    ///     <p>The title of the REPL when in use.</p>
    /// </summary>
    let title: string = $"{Properties.projectName} (v{Version.languageVersion |> Version.strVersion}) REPL"

    let launch (): unit =
        // helper function for initialising the console
        let initTerminal (): unit =
            IO.run <| (
                Terminal.setConfiguration {
                    title            = title                     |> Terminal.UseValue
                    backgroundColour = System.ConsoleColor.Black |> Terminal.UseValue
                    foregroundColour = System.ConsoleColor.White |> Terminal.UseValue
                    lines            = Terminal.UseDefault
                    columns          = Terminal.UseDefault
                } |> IO.seq <|
                Terminal.clear |> IO.seq <|
                Terminal.setConfiguration {
                    title            = Terminal.UseDefault
                    backgroundColour = System.ConsoleColor.DarkGray |> Terminal.UseValue
                    foregroundColour = System.ConsoleColor.Black    |> Terminal.UseValue
                    lines            = Terminal.UseDefault
                    columns          = Terminal.UseDefault
                } |> IO.seq <|
                Terminal.write $"    {title} \n"
            )

        // IO functor to reset the cursor up by one and left
        let resetCursor: IO<unit> =
            Terminal.getCursorPosition |> IO.bind <| (fun (_, y) -> Terminal.setCursorPosition (0, y - 1))

        let rec env (): unit =
            // prepare for user input
            IO.run <| (
                Terminal.setConfiguration {
                    title            = Terminal.UseDefault
                    backgroundColour = System.ConsoleColor.Black |> Terminal.UseValue
                    foregroundColour = System.ConsoleColor.White |> Terminal.UseValue
                    lines            = Terminal.UseDefault
                    columns          = Terminal.UseDefault
                } |> IO.seq <|
                Terminal.write ">>> "
            )

            let code: string = IO.run (Terminal.readLine false)

            // output the code entered and the tokens (if necessary)
            IO.run <| (
                resetCursor |> IO.seq <|
                Terminal.setConfiguration {
                    title            = Terminal.UseDefault
                    backgroundColour = Terminal.UseDefault
                    foregroundColour = System.ConsoleColor.DarkGray |> Terminal.UseValue
                    lines            = Terminal.UseDefault
                    columns          = Terminal.UseDefault
                } |> IO.seq <|
                Terminal.write " |" |> IO.seq <|
                Terminal.setConfiguration {
                    title            = Terminal.UseDefault
                    backgroundColour = Terminal.UseDefault
                    foregroundColour = System.ConsoleColor.White |> Terminal.UseValue
                    lines            = Terminal.UseDefault
                    columns          = Terminal.UseDefault
                } |> IO.seq <|
                Terminal.write $"  {code}\n" |> IO.bind <|
                (fun _ ->
                   if code |> System.String.IsNullOrEmpty then
                       IO.nil // do not output tokens
                   else
                       Terminal.setConfiguration {
                            title            = Terminal.UseDefault
                            backgroundColour = Terminal.UseDefault
                            foregroundColour = System.ConsoleColor.DarkGray |> Terminal.UseValue
                            lines            = Terminal.UseDefault
                            columns          = Terminal.UseDefault
                       } |> IO.seq <|
                       Terminal.write $" ¦  {code |> (Lexer.tokenise >> Lexer.strTokens)}\n" |> IO.bind <|
                       (fun _ ->
                           match (code |> (Lexer.tokenise >> Parser.parse)) with
                            | Ok    root -> Terminal.write $" ¦  {root}\n"
                            | Error err  ->
                                Terminal.setConfiguration {
                                    title            = Terminal.UseDefault
                                    backgroundColour = Terminal.UseDefault
                                    foregroundColour = System.ConsoleColor.Red |> Terminal.UseValue
                                    lines            = Terminal.UseDefault
                                    columns          = Terminal.UseDefault
                                } |> IO.seq <|
                                Terminal.write $" X  {strError err}\n"
                       )
                )
            )

            env ()

        initTerminal ()
        env          ()
