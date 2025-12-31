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

open Diorite.Lang.API
open Diorite.Lang.Core

/// <summary>
///     <p>The primary module of the CLI.</p>
/// </summary>
[<AutoOpen>]
module Main =
    let rec pretty (arr: AST): string =
        let asStr (elem: ASTNode): string =
            $"{elem}"

        match arr with
         | []           -> ""
         | [elem]       -> $"\t{asStr elem}"
         | head :: tail -> $"\t{asStr head}\n{pretty tail}"

    /// <summary>
    ///     <p>The main function.</p>
    /// </summary>
    /// <param name="argv"> the arguments provided to the executable </param>
    /// <returns> an exit code that describes the state of the CLI after exiting. </returns>
    [<EntryPoint>]
    let Main (argv: string array): int =
        let source: string = "# ------------------------------------------------------------------------------------------------------------------
#    _____  __              __ __
#   |     \|__|.-----.----.|__|  |_.-----.
#   |  --  |  ||  _  |   _||  |   _|  -__|
#   |_____/|__||_____|__|  |__|____|_____|
#
# ------------------------------------------------------------------------------------------------------------------
# File:    example.diorite
# Summary: An example file, primarily for testing purposes
# Author:  Arsngrobg
# Version: v1.4
# ------------------------------------------------------------------------------------------------------------------
# Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
# ------------------------------------------------------------------------------------------------------------------

# value types
1000!!!!!!;
"
        match (source |> Parser.ParseString) with
         | Error err   -> printf $"{StrError err}\n"
         | Ok    state ->
             printf $"[TranslationUnit]\n{state |> pretty}\n"
        0
