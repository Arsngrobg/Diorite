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
// Version: v1.16
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>REPL</c> module is the functionality related to the live interpreter environment in the terminal.
///     It provides a neat and simple environment for writing <b>Diorite</b> mathematics code.
///     It exposes a singular function for initialization.
///     <code>
///         let stable: bool = REPL.launch()
///         match stable with
///          | true  -> IO.output "REPL executed with no errors :)"    |> ignore
///          | false -> IO.output "REPL had an error during execution" |> ignore
///     </code>
/// </summary>
[<RequireQualifiedAccess>]
module private REPL =
    // shorthand typedefs
    type AST = Parser.AST

    /// <summary>
    ///     The command types available.
    /// </summary>
    type CommandToken =
        | Help
        | Quit
        | Clear
        | Save
        | Literal of string

    /// <summary>
    ///     The title of the REPL when in use.
    /// </summary>
    let title: string = $"{Properties.name} (v{Version.languageVersion}) REPL"

    // shows the error in the REPL
    let showError (err: DioriteError): unit Result =
        IO.compose [
            System.ConsoleColor.Red   |> IO.setConsoleForegroundColor |> generalized
            IO.output $" X  {err}\n"
            System.ConsoleColor.White |> IO.setConsoleForegroundColor |> generalized
        ]

    // shows the successful result in the REPL
    let rec showSuccess (root: AST): unit =
        let rec getStrings (accumulator: string) (root: AST): string =
            match root with
             | AST.Begin (head :: tail) ->
                 showSuccess head
                 (AST.Begin >> getStrings accumulator) tail
             | AST.Undefined        -> (accumulator + "; undefined")
             | AST.Number value     -> (accumulator + $"; {value}")
             | AST.PositiveInfinity -> (accumulator + "; ∞")
             | AST.NegativeInfinity -> (accumulator + "; -∞")
             | _                    -> ""

        let output: string = getStrings "" root
        if output |> System.String.IsNullOrEmpty then
            ()
        else
            IO.compose [
                System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> generalized
                IO.output $" ¦  {output}\n"
            ] |> ignore

    // initializes the console environment
    let initialise (): bool =
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

    // parses raw strings into a typed system of command types
    let parseCommand (input: string): CommandToken list =
        let rec parse (tokens: string list): CommandToken list =
            match tokens with
             | [] -> []
             | "@help"  :: tail -> Help            :: parse tail
             | "@quit"  :: tail -> Quit            :: parse tail
             | "@clear" :: tail -> Clear           :: parse tail
             | "@save"  :: tail -> Save            :: parse tail
             | literal  :: tail -> Literal literal :: parse tail

        if not(input.StartsWith "@") then [ CommandToken.Literal input ]
        else (input.Split (" ", System.StringSplitOptions.RemoveEmptyEntries)) |> (Array.toList >> parse)

    // executes the command sequence
    let executeCommand (history: string list) (command: CommandToken list): (bool * string list) Result =
        match command with
         | []        -> Ok (true, history) // if no command - do nothing

         | [ Help ]  -> Ok (true, history) // display help

         | [ Quit ]  -> Ok (false, history) // quit

         | [ Clear ] -> Ok (true, []) // clear the history

         | [ Save; Literal filename; Literal directory ] ->
             IO.compose [
                IO.writeFile filename directory (String.concat "\n" history)                                      |> generalized
                System.ConsoleColor.Green                                         |> IO.setConsoleForegroundColor |> generalized
                $"    Saved REPL history to %s{directory}\%s{filename}.diorite\n" |> IO.output                    |> generalized
                System.ConsoleColor.Black                                         |> IO.setConsoleForegroundColor |> generalized
             ] |> ignore
             Ok (true, history)
         | Save :: _ ->
             IO.compose [
                 System.ConsoleColor.Yellow                  |> IO.setConsoleForegroundColor |> generalized
                 "    Usage: @save <filename> [directory]\n" |> IO.output                    |> generalized
                 System.ConsoleColor.Black                   |> IO.setConsoleForegroundColor |> generalized
             ] |> ignore
             Ok (true, history)

         | [ Literal code ] ->
             match Interpreter.eval code with
              | Error err  ->
                  (showError >> ignore) err
                  Ok (true, history)
              | Ok    root ->
                  (showSuccess >> ignore) root
                  Ok (true, history @ [code])

         | _ -> SyntaxError "Unrecognised REPL command"
        
    /// <summary>
    ///     Launches the REPL environment in the user's terminal.
    /// </summary>
    /// <returns> <c>true</c> if the REPL exited without error; <c>false</c> if a fatal error occurred </returns>
    let rec launch (): bool =
        // partial for moving the cursor up or down by n units
        let moveCursorY: int -> (int * int) Result = IO.moveCursorRelative 0

        let rec env(history: string list): bool =
            IO.setConsoleForegroundColor System.ConsoleColor.White |> ignore
            IO.setConsoleBackgroundColor System.ConsoleColor.Black |> ignore

            let code: string = (Some >> IO.input) ">>> " |> getOrElse <| ""
            moveCursorY -1 |> ignore
            IO.compose [
                System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> generalized
                " |"                         |> IO.output
                System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
                $"  {code}\n"                |> IO.output
                System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
            ] |> ignore
            let result: (bool * string list) Result = code |> (parseCommand >> (executeCommand history))

            match result with
             | Error err ->
                 showError err |> ignore
                 true
             | Ok (keepRunning, history) -> if keepRunning then history |> env else false

        // if initialized, run the REPL
        if initialise () then env []
        else                  false

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
    ///     The argument types recognized by the <c>Diorite</c> CL utility.
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
         | [ Argument.Version ] ->
             IO.output $"{Properties.name} v{Version.languageVersion}" |> ignore
             ExitCode.NoError

         // display help if the ARG_HELP or no args are given
         | [ Argument.Help ] | [] ->
             IO.output $"{helpString}" |> ignore
             ExitCode.NoError

         // checks and upgrades this version of diorite to the latest version
         | [ Argument.Upgrade ] -> failwith "[TODO] Offer some sort of update feature (use gh releases?)"

         // launches the REPL environment in the user's terminal (IT DOES NOT WORK IN IDE INTEGRATED TERMINALS)
         | [ Argument.Interpreter ] ->
             let success = REPL.launch()
             if success then ExitCode.NoError
             else            ExitCode.REPLFailure

         // attempt to load the file into the REPL environment
         | [ Argument.Interpreter; Argument.Literal filename ] ->
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
         | [ Argument.Compile; Argument.Literal _ ] -> failwith "[TODO] Compile that shit"

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
         | ( "-h" | "--help"        ) :: tail -> Argument.Help         :: parseArgs tail
         | ( "-u" | "--upgrade"     ) :: tail -> Argument.Upgrade      :: parseArgs tail
         | ( "-v" | "--version"     ) :: tail -> Argument.Version      :: parseArgs tail
         | ( "-i" | "--interpreter" ) :: tail -> Argument.Interpreter  :: parseArgs tail
         | ( "-c" | "--compile"     ) :: tail -> Argument.Compile      :: parseArgs tail
         | head :: tail                       -> Argument.Literal head :: parseArgs tail

    [<EntryPoint>]
    let main (argv: string array): int =
        let args: Argument list = (Array.toList >> parseArgs) argv
        let exitCode: ExitCode = executeArgs args
        exitCode |> int
