// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    CLI.fs
// Summary: Command-Line Interface utils and the entry point for the Diorite language utility
// Author:  Arsngrobg
// Version: v1.9
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

module Diorite.Lang.CLI

open Diorite.Lang.Meta

/// <summary>
///     A binding that returns the string used by the CL utility when no args are provided or the
///     <c>-h</c>/<c>--help</c> flag is provided to the <c>Diorite</c> CL utility.
/// </summary>
/// <returns> the help string of the CL utility </returns>
let helpString: string = $"{Identity.name} {Version.languageVersion}
Usage: {Identity.programName} [-h | --help]
       (to display usage)
    or
       {Identity.programName} [-u | --upgrade]
       (to upgrade the current version of {Identity.name})
    or
       {Identity.programName} [-v | --version]
       (to display the current version of {Identity.name})
    or
       {Identity.programName} [-i | --interpreter] <file>?
       (to run the interpreter, either through the REPL or execution of a {Identity.fileExtension} file)
    or
       {Identity.programName} [-c | --compile] <file>
       (To compile a given {Identity.fileExtension} file)

    <file> ::= a file name, suffixed with the {Identity.fileExtension} extension
"

/// <summary>
///     The argument types recognised by the <c>Diorite</c> CL utility.
/// </summary>
type Argument =
    /// <summary> The typed argument flag determined via <c>-h</c>/<c>--help</c> string. </summary>
    | ARG_HELP
    /// <summary> The typed argument flag determined via the <c>-u</c>/<c>--upgrade</c> string. </summary>
    | ARG_UPGRADE
    /// <summary> The typed argument flag determined via the <c>-v</c>/<c>--version</c> string. </summary>
    | ARG_VERSION
    /// <summary> The typed argument flag determined via the <c>-i</c>/<c>--interpreter</c> string. </summary>
    | ARG_INTERPRETER
    /// <summary> The typed argument flag determined via the <c>-c</c>/<c>--compile</c> string. </summary>
    | ARG_COMPILE
    /// <summary> The typed argument flag of any string value passed as an argument. </summary>
    | ARG_LITERAL of string

/// <summary>
///     A binding that uplifts the raw <c>string</c> literals into typed <c>Argument</c> flags.
///     <code>
///         let args = collectArgs [ "-i", "foo.diorite" ]
///         printf $"{args}" // output: "[ARG_INTERPRETER; ARG_LITERAL]"
///     </code>
/// </summary>
/// <param name="argv"> the variadic list of raw string arguments </param>
/// <returns> a list of typed <c>Argument</c> union type </returns>
let collectArgs (argv: list<string>): list<Argument> =
    let rec read (argv: list<string>): list<Argument> =
        match argv with
         | []                                 -> []
         | "-h"::tail | "--help"       ::tail -> ARG_HELP         :: (read tail)
         | "-u"::tail | "--upgrade"    ::tail -> ARG_UPGRADE      :: (read tail)
         | "-v"::tail | "--version"    ::tail -> ARG_VERSION      :: (read tail)
         | "-i"::tail | "--interpreter"::tail -> ARG_INTERPRETER  :: (read tail)
         | "-c"::tail | "--compile"    ::tail -> ARG_COMPILE      :: (read tail)
         | head::tail                         -> ARG_LITERAL head :: (read tail)

    read argv

/// <summary>
///     A binding that executes the typed <c>Argument</c>, depending on the sequence of tokens provided to it.
/// </summary>
/// <param name="args"> the list of <c>Argument</c>s to process </param>
/// <returns> the error code, or <c>0</c> if no error occured </returns>
let executeArgs (args: list<Argument>): int32 =
    match args with
     | [ ARG_VERSION ]                    ->
         printf $"{Version.languageVersion}"
         0
     | [ ARG_HELP ] | []                  ->
         printf $"{helpString}"
         0
     | [ ARG_UPGRADE ]                    -> failwith "[TODO] Offer some sort of update feature (use gh releases?)"
     | [ ARG_INTERPRETER ]                ->
         0
     | [ ARG_INTERPRETER; ARG_LITERAL _ ] -> failwith "[TODO] Execute file by interpretation"
     | [ ARG_COMPILE; ARG_LITERAL _ ]     -> failwith "[TODO] Compile that shit"
     | _                                  -> failwith "Illegal combination of arguments"

[<EntryPoint>]
let main (argv: array<string>): int32 =
    let args: list<Argument> = collectArgs ( Array.toList argv )
    printf $"{args}"
    executeArgs args
