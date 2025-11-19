// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Configuration.fs
// Summary: Defines the configuration logic that determine the behaviour based on a given set of user-supplied
//          command-line arguments
// Author:  Arsngrobg
// Version: v1.3
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.CLI

open Diorite.Lang.Core

/// <summary>
///     The <c>Configuration</c> module consists of the logic that determines the behaviour of the CL utility provided a
///     given set of user-supplied command-line arguments.
/// </summary>
module Configuration =
    /// <summary>
    ///     <p>An enum consisting of exit codes that may be returned by the CL utility upon exit.
    ///        Each exit code represents a singular binary digit with no overlap, meaning it may be possible for the
    ///        execution to produce an exit code which is a combination of two or more exit codes.
    ///     </p>
    ///     <p>A successful execution is always represented by the <c>NoError</c> case, or <c>0</c>.</p>
    /// </summary>
    type ExitCode =
        | NoError      = 0b0000 // no error occurred
        | IllegalArgs  = 0b0001 // an illegal arrangement
        | UpdateFailed = 0b0010 // update failed to download
        | REPLFailure  = 0b0100 // the REPL failed
        | SyntaxError  = 0b1000 // a syntax error in compilation

    /// <summary>
    ///     <p>The argument types that are recognised by the <b>Diorite</b> CL utility.</p>
    ///     <p>These types are derived from the raw <c>string</c> literal arguments supplied to the CL utility and
    ///        executed via their arrangement of arguments.
    ///     </p>
    /// </summary>
    type CLIArg =
        | Help                // [ -h | --help ]
        | Version             // [ -v | --version ]
        | Upgrade             // [ -u | --upgrade ]
        | Interpret           // [ -i | --interpreter ] <filename>
        | Compile             // [ -c | --compile ] <filename>
        | Literal   of string // raw string

    /// <summary>
    ///     Executes the supplied list of <c>CLIArg</c>s depending on the sequence of tokens provided to it.
    /// </summary>
    /// <param name='args'> the list of <c>Argument</c>s to process </param>
    /// <returns> the exit code, if <c>ExitCode.NoError</c> if no error occured </returns>
    let executeArgs (args: CLIArg list): ExitCode =
        // string that is output when the user supplies the help argument or no arguments
        let helpString: string = $"{Properties.projectName} v{Version.languageVersion |> Version.strVersion}
Usage: {Properties.programName} [-h | --help]
       (to display usage)
     or
       {Properties.programName} [-u | --upgrade]
       (to upgrade the current version of {Properties.projectName})
     or
       {Properties.programName} [-v | --version]
       (to display the current version of {Properties.projectName})
     or
       {Properties.programName} [-i | --interpreter] <file>?
       (to run the interpreter, either through the REPL or execution of a {Properties.fileExtension} file)
     or
       {Properties.programName} [-c | --compile] <file>
       (To compile a given {Properties.fileExtension} file)

     <file> ::= a file name, suffixed with the {Properties.fileExtension} extension"

        match args with
         | [ CLIArg.Help ] | [] ->
             $"{helpString}\n" |> (Terminal.write >> IO.run >> ignore)
             ExitCode.NoError

         | [ CLIArg.Version ] ->
             printf $"{Properties.projectName} v{Version.languageVersion |> Version.strVersion}\n"
             ExitCode.NoError

         | [ CLIArg.Upgrade ] -> failwith "[TODO] Offer some sort of update feature (gh releases?)"

         | [ CLIArg.Interpret ] ->
             REPL.launch ()
             ExitCode.NoError

         | [ CLIArg.Interpret; CLIArg.Literal _ ] -> ExitCode.REPLFailure

         | [ CLIArg.Compile; CLIArg.Literal _ ] -> failwith "[TODO] Compile that shit"

         | _ -> ExitCode.IllegalArgs

    /// <summary>
    ///     Uplifts the raw <c>string</c> literals into typed <c>CLIArg</c> flags.
    /// </summary>
    /// <param name='argv'> the variadic array of <c>string</c>s </param>
    /// <returns> a list of typed <c>CLIArg</c>s </returns>
    let collectArgs (argv: string array): CLIArg list =
        let rec scan (argv: string list): CLIArg list =
            match argv with
             | []                               -> []
             | ("-h" | "--help")        :: tail -> CLIArg.Help        :: scan tail
             | ("-v" | "--version")     :: tail -> CLIArg.Version     :: scan tail
             | ("-u" | "--upgrade")     :: tail -> CLIArg.Upgrade     :: scan tail
             | ("-i" | "--interpreter") :: tail -> CLIArg.Interpret   :: scan tail
             | ("-c" | "--compile")     :: tail -> CLIArg.Compile     :: scan tail
             | arg                      :: tail -> CLIArg.Literal arg :: scan tail

        argv |> (List.ofArray >> scan)
