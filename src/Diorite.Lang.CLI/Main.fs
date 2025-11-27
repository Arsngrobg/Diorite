// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Main.fs
// Summary: The entry point for Diorite when ran from the user's terminal
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.CLI

open Diorite.Lang.Core

/// <summary>
///     <p>The primary module of the CLI.</p>
/// </summary>
[<AutoOpen>]
module Main =
    /// <summary>
    ///     <p>The main function.</p>
    /// </summary>
    /// <param name='argv'> the arguments provided to the executable </param>
    /// <returns> an exit code that describes the state of the CLI after exiting. </returns>
    [<EntryPoint>]
    let main (argv: string array): int =
        while true do
            let source: string = System.Console.ReadLine ()
            match source |> (Lexer.tokenise >> Parser.parse) with
             | Error err  -> printf $"{strError err}\n"
             | Ok    root ->
                 match (Interpreter.eval (Interpreter.Memory.initStorage ())) root with
                  | Error err    -> printf $"{strError err}\n"
                  | Ok    (r, _) -> printf $"{r}\n"
        0
