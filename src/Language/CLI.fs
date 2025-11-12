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
// Version: v1.15
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>REPL</c> module is the functionality related to the live interpreter environment in the terminal.
///     It provides a neat and simple environment for writing <b>Diorite</b> mathematics code.
///     It exposes a singular function for initialisation.
///     <code>
///         let stable: bool = REPL.launch()
///         match stable with
///          | true  -> IO.output "REPL executed with no errors :)"    |> ignore
///          | false -> IO.output "REPL had an error during execution" |> ignore
///     </code>
/// </summary>
[<RequireQualifiedAccess>]
module private REPL =
    /// <summary>
    ///     A binding that defines the title of the REPL when in use.
    /// </summary>
    /// <returns> the title of the REPL </returns>

    let title: string = $"{Properties.name} (v{Version.languageVersion}) REPL"

    // all statements input in the REPL
    let mutable history: string list = []

    // helper function to test a string to see if it is a blank line
    let isBlankLine (line: string): bool =
        System.String.IsNullOrWhiteSpace line

    // initializes the console environment and hence the REPL environment.
    let initialiseConsole (): bool =
        // execute batch operation
        let result: unit Result = IO.compose [
            title                        |> IO.setConsoleTitle           |> generalized
            None                         |> IO.clearConsole              |> generalized
            System.ConsoleColor.DarkGray |> IO.setConsoleBackgroundColor |> generalized
            System.ConsoleColor.Black    |> IO.setConsoleForegroundColor |> generalized
            $"    {title} \n"            |> IO.output
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
            System.ConsoleColor.Black    |> IO.setConsoleBackgroundColor |> generalized
        ]

        resultAsBool result

    // processes the provided input from the user
    let processInput (input: string): bool =
        // defines what is output depending on the lexer result
        let noOutputIfNoTokens (): unit Result =
            if System.String.IsNullOrEmpty input then
                Ok ()
            else
            match Interpreter.eval input with
             | Error err -> IO.compose [
                 System.ConsoleColor.Red   |> IO.setConsoleForegroundColor |> generalized;
                 IO.output $" X  {err}\n"
                 System.ConsoleColor.White |> IO.setConsoleForegroundColor |> generalized;
               ]
             | Ok result -> IO.compose [
                 System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> generalized
                 IO.output $" ¦  {result}\n"
               ]

        // partial for moving the cursor up or down by n units
        let moveCursorY: int -> (int * int) Result = IO.moveCursorRelative 0

        // execute batch operation
        let result: unit Result = IO.compose [
            -1                           |> moveCursorY                  |> generalized
            System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> generalized
            " |"                         |> IO.output
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
            $"  {input}\n"               |> IO.output;
            ()                           |> noOutputIfNoTokens
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
        ]
        resultAsBool result
        
    /// <summary>
    ///     Saves REPL history to a <c>.diorite</c> file.
    /// </summary>
    /// <param name='saveInput'> the <c>@save</c> command arguments </param>
    let save (saveInput: string): unit =
        let parts: string array = saveInput.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        match parts with
        | [|"@save"; fileName; directory|] ->
            IO.writeFile fileName directory (String.concat "\n" history) |> ignore
            System.ConsoleColor.Green |> IO.setConsoleForegroundColor |> generalized |> ignore;
            IO.output $"    Saved REPL history to %s{directory}\%s{fileName}.diorite\n" |> ignore
        | _ ->
            System.ConsoleColor.Yellow |> IO.setConsoleForegroundColor |> generalized |> ignore;
            IO.output "    Usage: @save <filename> [directory]\n" |> ignore
        |> ignore
        ()
        
    /// <summary>
    ///     Launches the REPL environment in the user's terminal.
    /// </summary>
    /// <returns> <c>true</c> if the REPL exited without error; <c>false</c> if a fatal error occurred </returns>
    let rec launch (): bool =
        let rec env (): bool =
            System.Console.ForegroundColor <- System.ConsoleColor.White
            System.Console.BackgroundColor <- System.ConsoleColor.Black
            match IO.input(Some ">>> ") with
             | Error _     -> false
             | Ok input ->
                 match input with
                  | "@quit" -> true
                  | saveInput when saveInput.StartsWith("@save") ->
                        save saveInput
                        env()
                  | _ ->
                      if processInput input then
                          match Interpreter.eval input with
                          | Ok _ ->
                              history <- history @ [input]
                          | Error _ ->
                              () // Error generating code not added to REPL history
                          env()
                      else
                        false

        // exit if initialisation failed
        if initialiseConsole() then env()
        else                        false

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
    ///         IO.output $"{ExitCode.NoError}\n"      |> ignore // output: "0"
    ///         IO.output $"{ExitCode.REPLFailure}\n"  |> ignore // output: "1"
    ///         IO.output $"{ExitCode.FileNotFound}\n" |> ignore // output: "2"
    ///         IO.output $"{ExitCode.IllegalArgs}\n"  |> ignore // output: "4"
    ///         IO.output $"{ExitCode.SyntaxError}\n"  |> ignore // output: "8"
    ///     </code>
    /// </summary>
    type ExitCode =
        | NoError       = 0b00000 // no error was encountered
        | REPLFailure   = 0b00001 // any failed state caused by the REPL
        | FileNotFound  = 0b00010 // the file specified was not found
        | IllegalArgs   = 0b00100 // illegal sequence of arguments
        | SyntaxError   = 0b01000 // compile source file

    /// <summary>
    ///     A binding that returns the string used by the CL utility when no args are provided or the
    ///     <c>-h</c>/<c>--help</c> flag is provided to the <c>Diorite</c> CL utility.
    /// </summary>
    /// <returns> the help string of the CL utility </returns>
    let helpString: string = $"{Properties.name} v{Version.languageVersion}
Usage: {Properties.programName} [-h | --help]
       (to display usage)
    or
       {Properties.programName} [-u | --upgrade]
       (to upgrade the current version of {Properties.name})
    or
       {Properties.programName} [-v | --version]
       (to display the current version of {Properties.name})
    or
       {Properties.programName} [-i | --interpreter] <file>?
       (to run the interpreter, either through the REPL or execution of a {Properties.fileExtension} file)
    or
       {Properties.programName} [-c | --compile] <file>
       (To compile a given {Properties.fileExtension} file)

    <file> ::= a file name, suffixed with the {Properties.fileExtension} extension
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
    /// <param name='args'> the list of <c>Argument</c>s to process </param>
    /// <returns> the error code, or <c>0</c> if no error occured </returns>
    let executeArgs (args: Argument list): ExitCode =
        match args with
         // display this version of diorite
         | [ Version ] ->
             IO.output $"{Properties.name} v{Version.languageVersion}" |> ignore
             ExitCode.NoError

         // display help if the ARG_HELP or no args are given
         | [ Help ] | [] ->
             IO.output $"{helpString}" |> ignore
             ExitCode.NoError

         // checks and upgrades this version of diorite to the latest version
         | [ Upgrade ] -> failwith "[TODO] Offer some sort of update feature (use gh releases?)"

         // launches the REPL environment in the user's terminal (IT DOES NOT WORK IN IDE INTEGRATED TERMINALS)
         | [ Interpreter ] ->
             let success = REPL.launch()
             if success then ExitCode.NoError
             else            ExitCode.REPLFailure

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
    let rec parseArgs (argv: string list): Argument list =
        match argv with
         | []                                 -> []
         | ( "-h" | "--help"        ) :: tail -> Help         :: parseArgs tail
         | ( "-u" | "--upgrade"     ) :: tail -> Upgrade      :: parseArgs tail
         | ( "-v" | "--version"     ) :: tail -> Version      :: parseArgs tail
         | ( "-i" | "--interpreter" ) :: tail -> Interpreter  :: parseArgs tail
         | ( "-c" | "--compile"     ) :: tail -> Compile      :: parseArgs tail
         | head :: tail                       -> Literal head :: parseArgs tail

    [<EntryPoint>]
    let main (argv: string array): int =
        let args: Argument list = (Array.toList >> parseArgs) argv
        let exitCode: ExitCode = executeArgs args
        int <| exitCode
