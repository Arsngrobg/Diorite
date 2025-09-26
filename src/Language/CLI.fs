// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    CLI.fs
// Summary: Command-Line Interface utils and the entry point for the Diorite language utilities
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

module Diorite.Lang.CLI

/// <summary>The arguments available to be supplied through the CLI.</summary>
type Argument =
    | ARG_HELP
    | ARG_UPGRADE
    | ARG_VERSION
    | ARG_INTERPRETER
    | ARG_COMPILE
    | ARG_LITERAL of string

/// <summary>
///     Parses the array of raw literal arguments (<c>argv</c>) into a concrete list of tokenized <c>Argument</c> types.
/// </summary>
/// <param name='argv'>the raw variadic list of arguments supplied through the CLI</param>
/// <seealso cref="Diorite.Lang.CLI.Argument"/>
let collectArgs(argv: array<string>): list<Argument> =
    let rec read(argv: list<string>): list<Argument> =
        match argv with
         | []                                 -> []
         | "-h"::tail | "--help"::tail        -> ARG_HELP              :: (read <| tail)
         | "-u"::tail | "--upgrade"::tail     -> ARG_UPGRADE           :: (read <| tail)
         | "-v"::tail | "--version"::tail     -> ARG_VERSION           :: (read <| tail)
         | "-i"::tail | "--interpreter"::tail -> ARG_INTERPRETER       :: (read <| tail)
         | "-c"::tail | "--compile"::tail     -> ARG_COMPILE           :: (read <| tail)
         | _                                  -> ARG_LITERAL argv.Head :: (read <| argv.Tail)

    read <| ( argv |> Array.toList )

// dummy function for now
let rec list2str<'T>(list: list<'T>): string =
    match list with
        | head::tail -> $"{head.ToString()}, {list2str <| tail}"
        | []         ->  "\n"


[<EntryPoint>]
let main(argv: array<string>): int32 =
    printf $"{ list2str<Argument> <| (collectArgs <| argv) }"
    0
