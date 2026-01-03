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

module Diorite.Lang.CLI

open Diorite.Lang.Core.Errors
open Diorite.Lang.Core.Runtime
open Diorite.Lang.Core.Runtime.Evaluation
open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Parser

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
z = complex(0, 1);
w = complex(1, 1);
r = re(z);
i = im(z);
z*w;

f(x) = 2*x;
f(x: R) -> R = 2*x;

plot x/0 using x; # plot this anonymous function using x as the domain
"
    match (source |> ParseString) with
     | Error err   -> printf $"{StrError err}\n"
     | Ok    state ->
         EvalTree state (Defaults (fun eval ->
            for i = 0 to 5 do
                let value = ValueType.Number i
                match (eval value) with
                 | Ok    result -> printf $"{result}\n"
                 | Error err    -> printf $"{StrError err}\n"
         ))
         |> List.iter (fun i ->
                match i with
                 | Ok (s, _) -> printf $"{s}\n"
                 | Error err -> printf $"{StrError err}\n"
            )
         //printf $"[TranslationUnit]\n{state |> pretty}\n"
    0
