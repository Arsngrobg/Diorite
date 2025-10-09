// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Interpreter.fs
// Summary: The interpreter of for the Diorite language, which also includes a REPL
// Author:  Arsngrobg
// Version: v1.3
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

module REPL =
    let rec launch (): int =
        // TODO: put unsafe code into IO module
        System.Console.BackgroundColor <- System.ConsoleColor.Black
        System.Console.Clear()
        System.Console.Title           <- Identity.name
        System.Console.BackgroundColor <- System.ConsoleColor.DarkRed

        let title: string = $"{Identity.name} v{Version.languageVersion.ToString()} (REPL)"
        IO.output $"    {title} \n" |> ignore

        System.Console.BackgroundColor <- System.ConsoleColor.Black

        let print (input: string, tokens: Lexer.Token list): Unit =
            IO.moveCursorRelative 0 -1 |> ignore
            System.Console.ForegroundColor <- System.ConsoleColor.DarkGray
            IO.output "\r |" |> ignore
            System.Console.ForegroundColor <- System.ConsoleColor.White
            IO.output $"  {input}\n" |> ignore

            match tokens with
             | [] -> IO.output "" |> ignore
             | _  ->
                 System.Console.ForegroundColor <- System.ConsoleColor.DarkGray
                 IO.output " ¦" |> ignore
                 IO.output $"  {tokens |> Lexer.tokens2str}\n" |> ignore
                 System.Console.ForegroundColor <- System.ConsoleColor.White

        let eval (input: string): Unit =
             let lexResult: Lexer.Token list IO.Result = input |> Lexer.lex
             match lexResult with
              | IO.Success tokens ->
                  print(input, tokens)
              | IO.Failure err    ->
                  System.Console.ForegroundColor <- System.ConsoleColor.Red
                  IO.output $"{err} (unrecognised token)\n" |> ignore
                  System.Console.ForegroundColor <- System.ConsoleColor.White

        let rec read (): int =
            let inputResult: string IO.Result = Some ">>> " |> IO.input
            match inputResult with
             | IO.Failure _ -> 0
             | IO.Success i ->
                 match i with
                  | "@quit"  -> 0
                  | "@clear" ->
                    launch()
                  | _        ->
                      eval(i)
                      read()

        read()
