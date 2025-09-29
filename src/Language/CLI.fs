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

/// <summary>
///     The cached <c>help</c> string, that displays when the CLI arguments: <c>-h</c>/<c>--help</c> is supplied or no
///     args are supplied.
/// </summary>
let HELP_STRING: string = $"\b
Usage: diorite [-h | --help]
       (to display usage)
    or
       diorite [-u | --upgrade]
       (to upgrade the current version of {Meta.files.PROG_NAME_TITLE})
    or
       diorite [-v | --version]
       (to display the current version of {Meta.files.PROG_NAME_TITLE})
    or
       diorite [-i | --interpreter] <file>?
       (to run the interpreter, either through the REPL or execution of a {Meta.files.FILE_EXTENSION} file)
    or
       diorite [-c | --compile] <file>
       (To compile a given .diorite file)

    <file> ::= a file name, suffixed with the {Meta.files.FILE_EXTENSION} extension
"

/// <summary>The arguments available to be supplied through the CLI.</summary>
type Argument =
    | ARG_HELP
    | ARG_UPGRADE
    | ARG_VERSION
    | ARG_INTERPRETER
    | ARG_COMPILE
    | ARG_LITERAL of string

/// <summary>Displays the <c>HELP_STRING</c> in the user's CLI.</summary>
let help: int32 =
    printf $"{HELP_STRING}"
    0

/// <summary>
///     Parses the list of raw literal arguments (<c>argv</c>) into a concrete list of tokenized <c>Argument</c> types.
/// </summary>
/// <param name='argv'>the raw variadic list of arguments supplied through the CLI or otherwise</param>
/// <seealso cref="Diorite.Lang.CLI.Argument"/>
let collectArgs (argv: list<string>): list<Argument> =
    let rec read (argv: list<string>): list<Argument> =
        match argv with
         | []                                 -> []
         | "-h"::tail | "--help"::tail        -> ARG_HELP              :: (read tail)
         | "-u"::tail | "--upgrade"::tail     -> ARG_UPGRADE           :: (read tail)
         | "-v"::tail | "--version"::tail     -> ARG_VERSION           :: (read tail)
         | "-i"::tail | "--interpreter"::tail -> ARG_INTERPRETER       :: (read tail)
         | "-c"::tail | "--compile"::tail     -> ARG_COMPILE           :: (read tail)
         | _                                  -> ARG_LITERAL argv.Head :: (read argv.Tail)

    read argv

/// <summary>
///     Executes the specific <b>Diorite</b> functionality through the provided combination of <c>Argument</c> list.
/// </summary>
/// <param name="args">the <c>Argument</c> tokens to `parse`</param>
/// <seealso cref="Diorite.Lang.CLI.Argument"/>
let executeArgs (args: list<Argument>): int32 =
    match args with
     | []                                 -> help
     | [ ARG_VERSION ]                    -> failwith "[TODO] Display version"
     | [ ARG_HELP ]                       -> help
     | [ ARG_UPGRADE ]                    -> failwith "[TODO] Offer some sort of update feature (use gh releases?)"
     | [ ARG_INTERPRETER ]                -> failwith "[TODO] Bring up CLI for writing program"
     | [ ARG_INTERPRETER; ARG_LITERAL _ ] -> failwith "[TODO] Execute file by interpretation"
     | [ ARG_COMPILE; ARG_LITERAL _ ]     -> failwith "[TODO] Compile that shit"
     | _                                  -> failwith "Illegal combination of arguments"

// dummy function for now
let rec list2str<'T> (list: list<'T>): string =
    match list with
        | head::tail -> $"{head.ToString()}, {list2str tail}"
        | []         ->  "\n"

[<EntryPoint>]
let main (argv: array<string>): int32 =
    let args: list<Argument> = collectArgs ( Array.toList argv )
    printf $"{ list2str<Argument> args }"
    executeArgs args
