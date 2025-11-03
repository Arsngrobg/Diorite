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
// Version: v1.12
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
module private CLI =
    /// <summary>
    ///     An enum consisting of exit codes that may be returned by the <c>CLI::executeArgs (Argument list)</c>
    ///     function.
    ///     <code>
    ///         IO.output $"{ExitCode.NoError}"       |> ignore // output: "0"
    ///         IO.output $"{ExitCode.REPLFailure}"   |> ignore // output: "1"
    ///         IO.output $"{ExitCode.FileNotFound}"  |> ignore // output: "2"
    ///         IO.output $"{ExitCode.IllegalArgs}"   |> ignore // output: "4"
    ///         IO.output $"{ExitCode.IllegalToken}"  |> ignore // output: "8"
    ///         IO.output $"{ExitCode.IllegalTokens}" |> ignore // output: "16"
    ///     </code>
    /// </summary>
    type ExitCode =
        | NoError       = 0b00000 // no error was caused
        | REPLFailure   = 0b00001 // any failed state caused by the REPL
        | FileNotFound  = 0b00010 // the file specified was not found
        | IllegalArgs   = 0b00100 // illegal sequence of arguments
        | IllegalToken  = 0b01000 // illegal token found
        | IllegalTokens = 0b10000 // illegal token sequence

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
        | Help                  // -h / --help
        | Upgrade               // -u / --upgrade
        | Version               // -v / --version
        | Interpreter           // -i / --interpreter
        | Compile               // -c / --compile
        | Literal     of string // any other value (e.g. file path)

    /// <summary>
    ///     A binding that executes the typed <c>Argument</c>, depending on the sequence of tokens provided to it.
    /// </summary>
    /// <param name="args"> the list of <c>Argument</c>s to process </param>
    /// <returns> the error code, or <c>0</c> if no error occured </returns>
    let executeArgs (args: Argument list): ExitCode =
        match args with
         // display this version of diorite
         | [ Version ] ->
             IO.output $"{Version.languageVersion}" |> ignore
             ExitCode.NoError

         // display help if the ARG_HELP or no args are given
         | [ Help ] | [] ->
             IO.output $"{helpString}" |> ignore
             ExitCode.NoError

         // checks and upgrades this version of diorite to the latest version
         | [ Upgrade ] -> failwith "[TODO] Offer some sort of update feature (use gh releases?)"

         // launches the REPL environment in the user's terminal (IT DOES NOT WORK IN IDE INTEGRATED TERMINALS)
         | [ Interpreter ] ->
             match REPL.launch() with
              | true  -> ExitCode.NoError
              | false -> ExitCode.REPLFailure

         // attempt to load the file into the REPL environment
         | [ Interpreter; Literal filename ] ->
             match IO.readFile filename with
              | Ok fileContents ->
                  let tokens = Lexer.tokenize(fileContents)
                  IO.output $"{Lexer.tokens2str tokens}\n" |> ignore
                  let result = Parser.parse tokens
                  match result with
                   | Ok root   -> IO.output $"{root}\n" |> ignore
                   | Error err -> IO.output $"{err}\n"  |> ignore
                  ExitCode.NoError
              | Error err ->
                  IO.output $"{err}\n" |> ignore
                  ExitCode.FileNotFound

         // compile the given .diorite file
         | [ Compile; Literal _ ] -> failwith "[TODO] Compile that shit"

         // illegal combination of arguments given to the CLI
         | _ -> ExitCode.IllegalArgs

    /// <summary>
    ///     A binding that uplifts the raw <c>string</c> literals into typed <c>Argument</c> flags.
    ///     <code>
    ///         let args = collectArgs [ "-i", "foo.diorite" ]
    ///         printf $"{args}" // output: "[ARG_INTERPRETER; ARG_LITERAL]"
    ///     </code>
    /// </summary>
    /// <param name='argv'> the variadic list of raw string arguments </param>
    /// <returns> a list of typed <c>Argument</c> union type </returns>
    let collectArgs (argv: string list): Argument list =
        let rec read (argv: string list): Argument list =
            match argv with
             | []                                 -> []
             | ( "-h" | "--help"        ) :: tail -> Help         :: read tail
             | ( "-u" | "--upgrade"     ) :: tail -> Upgrade      :: read tail
             | ( "-v" | "--version"     ) :: tail -> Version      :: read tail
             | ( "-i" | "--interpreter" ) :: tail -> Interpreter  :: read tail
             | ( "-c" | "--compile"     ) :: tail -> Compile      :: read tail
             | head :: tail                       -> Literal head :: read tail

        read argv

    [<EntryPoint>]
    let main (argv: string array): int =
        let args: Argument list = collectArgs ( Array.toList argv )
        let exitCode: ExitCode = executeArgs args
        int <| exitCode
