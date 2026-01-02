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
open Diorite.Lang.Core.Runtime.Evaluators
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
z = complex(1, 5);
r = re(z);
i = im(z);
-z;

[symbol:foo]
f(x) = 2*x;
plot x using x;
f(2);
fo(2);
"
    match (source |> ParseString) with
     | Error err   -> printf $"{StrError err}\n"
     | Ok    state ->
         EvaluateTree state (Defaults (fun eval ->
            for i = 0 to 5 do
                let value = ValueType.Number i
                printfn $"{eval(value)}\n"
         ))
         |> List.iter (fun i ->
                match i with
                 | Ok (s, _) -> printf $"{s}\n"
                 | Error err -> printf $"{StrError err}\n"
            )
         printf $"[TranslationUnit]\n{state |> pretty}\n"
    0
