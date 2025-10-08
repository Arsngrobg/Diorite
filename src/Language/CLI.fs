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
// Version: v1.10
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>CLI</c> module relates to the Command Line Interface utilities that face the user when compiling or
///     interpreting <b>Diorite</b> source files. It provides many tools for getting metadata (e.g. the current version
///     of <b>Diorite</b>) or launching the REPL environment in the user's terminal.
///
///     It also encompasses the main entry point for <b>Diorite</b>.
/// </summary>
module CLI =
    /// <summary>
    ///     An enum consisting of exit codes that may be returned by the <c>CLI::executeArgs (Argument list)</c>
    ///     function.
    ///     <code>
    ///         IO.output $"{ExitCode.NO_ERROR}"       |> ignore // output: "0"
    ///         IO.output $"{ExitCode.FILE_NOT_FOUND}" |> ignore // output: "1"
    ///     </code>
    /// </summary>
    type ExitCode =
        | NO_ERROR       = 0 // no error was caused
        | FILE_NOT_FOUND = 1 // the file specified was not found
        | ILLEGAL_ARGS   = 2 // illegal sequence of arguments
        | ILLEGAL_TOKEN  = 4 // illegal token found
        | ILLEGAL_TOKENS = 8 // illegal token sequence

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
        | ARG_HELP              // -h / --help
        | ARG_UPGRADE           // -u / --upgrade
        | ARG_VERSION           // -v / --version
        | ARG_INTERPRETER       // -i / --interpreter
        | ARG_COMPILE           // -c / --compile
        | ARG_LITERAL of string // any other value (e.g. file path)

    /// <summary>
    ///     A binding that uplifts the raw <c>string</c> literals into typed <c>Argument</c> flags.
    ///     <code>
    ///         let args = collectArgs [ "-i", "foo.diorite" ]
    ///         printf $"{args}" // output: "[ARG_INTERPRETER; ARG_LITERAL]"
    ///     </code>
    /// </summary>
    /// <param name="argv"> the variadic list of raw string arguments </param>
    /// <returns> a list of typed <c>Argument</c> union type </returns>
    let collectArgs (argv: string list): Argument list =
        let rec read (argv: string list): Argument list =
            match argv with
             | []                                 -> []
             | ( "-h" | "--help"        ) :: tail -> ARG_HELP         :: read tail
             | ( "-u" | "--upgrade"     ) :: tail -> ARG_UPGRADE      :: read tail
             | ( "-v" | "--version"     ) :: tail -> ARG_VERSION      :: read tail
             | ( "-i" | "--interpreter" ) :: tail -> ARG_INTERPRETER  :: read tail
             | ( "-c" | "--compile"     ) :: tail -> ARG_COMPILE      :: read tail
             | head :: tail                       -> ARG_LITERAL head :: read tail

        read argv

    /// <summary>
    ///     A binding that executes the typed <c>Argument</c>, depending on the sequence of tokens provided to it.
    /// </summary>
    /// <param name="args"> the list of <c>Argument</c>s to process </param>
    /// <returns> the error code, or <c>0</c> if no error occured </returns>
    let executeArgs (args: Argument list): ExitCode =
        match args with
         // display this version of diorite
         | [ ARG_VERSION ]                    ->
             IO.output $"{Version.languageVersion}" |> ignore
             ExitCode.NO_ERROR

         // display help if the ARG_HELP or no args are given
         | [ ARG_HELP ] | []                  ->
             IO.output $"{helpString}" |> ignore
             ExitCode.NO_ERROR

         // checks and upgrades this version of diorite to the latest version
         | [ ARG_UPGRADE ]                    -> failwith "[TODO] Offer some sort of update feature (use gh releases?)"

         // launches the REPL environment in the user's terminal
         | [ ARG_INTERPRETER ]                -> failwith "[TODO] launch REPL environment in the terminal"

         | [ ARG_INTERPRETER; ARG_LITERAL file ] ->
             let result: string IO.Result = IO.readFile file
             match result with
              | IO.Success file ->
                   IO.output $"'''\n{file}'''\n" |> ignore
                   IO.output $"{file |> Lexer.lex |> Lexer.tokens2str}" |> ignore
                   ExitCode.NO_ERROR
              | IO.Failure err  ->
                   IO.output $"{err}" |> ignore
                   ExitCode.FILE_NOT_FOUND

         // compile the given .diorite file
         | [ ARG_COMPILE; ARG_LITERAL _ ]     -> failwith "[TODO] Compile that shit"

         // illegal combination of arguments given to the CLI
         | _                                  ->
             ExitCode.ILLEGAL_ARGS

    [<EntryPoint>]
    let main (argv: string array): int32 =
        let args: Argument list = collectArgs ( Array.toList argv )
        let exitCode: ExitCode = executeArgs args
        int32 <| exitCode
