// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Terminal.fs
// Summary: Logic for configuring the terminal in the command-line
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.CLI

open Diorite.Lang.Core

/// <summary>
///     <p>The <c>Terminal</c> module contains bindings for custom logic containers that mutate the current state of the
///        user's terminal.
///     </p>
/// </summary>
module Terminal =
    /// <summary>
    ///     <p>The <c>TerminalConfiguration</c> type contains information about the user's terminal.</p>
    ///     <p>To get the current <c>TerminalConfiguration</c>, call the <c>Terminal.getConfiguration</c> function.</p>
    /// </summary>
    [<Struct>]
    type TerminalConfiguration = {
        title:            string
        backgroundColour: System.ConsoleColor
        foregroundColour: System.ConsoleColor
        lines:            int
        columns:          int
    }

    /// <summary>
    ///     <p>The <c>CursorPosition</c> type is a tuple which represents a 2D vector that resembles a position within
    ///        the user's terminal. The first element is the column and the second element is the line.
    ///     </p>
    /// </summary>
    type CursorPosition = int * int

    /// <summary>
    ///     <p>The <c>IO</c> functor that contains the computation that retrieves the current
    ///        <c>TerminalConfiguration</c> for the user's terminal.
    ///     </p>
    /// </summary>
    let getConfiguration: IO<TerminalConfiguration> =
        IO (fun () -> {
            title            = System.Console.Title
            backgroundColour = System.Console.BackgroundColor
            foregroundColour = System.Console.ForegroundColor
            lines            = System.Console.BufferHeight
            columns          = System.Console.BufferWidth
        })

    /// <summary>
    ///     <p>Produces an <c>IO</c> functor that contains the computation that applies the supplied
    ///        <c>TerminalConfiguration</c> to the user's terminal.
    ///     </p>
    /// </summary>
    /// <param name='data'> the <c>TerminalConfiguration</c> to apply to the user's terminal </param>
    /// <returns> an <c>IO</c> functor that sets the current <c>TerminalConfiguration</c> </returns>
    let setConfiguration (data: TerminalConfiguration): IO<bool> =
        IO (fun () ->
            try
                System.Console.Title           <- data.title
                System.Console.BackgroundColor <- data.backgroundColour
                System.Console.ForegroundColor <- data.foregroundColour
                System.Console.BufferHeight    <- data.lines
                System.Console.BufferWidth     <- data.columns
                true
            with
             | _ -> false
        )

    /// <summary>
    ///     <p>The <c>IO</c> functor that contains the computation that retrieves the current cursor position in the
    ///        user's terminal.
    ///     </p>
    /// </summary>
    let getCursorPosition: IO<CursorPosition> =
        IO (fun () ->
            match System.Console.GetCursorPosition () with x, y -> x, y
        )

    /// <summary>
    ///     <p>Produces an <c>IO</c> functor that contains the computation that sets the cursor position in the user's
    ///        terminal.
    ///     </p>
    /// </summary>
    /// <param name='pos'> the <c>CursorPosition</c> </param>
    /// <returns> an <c>IO</c> functor that sets the cursor position in the user's terminal </returns>
    let setCursorPosition (pos: CursorPosition): IO<unit> =
        IO (fun () ->
            System.Console.SetCursorPosition pos
        )

    /// <summary>
    ///     <p>The <c>IO</c> functor that contains the computation that clears the user's terminal.</p>
    /// </summary>
    let clear: IO<unit> =
        IO System.Console.Clear

    /// <summary>
    ///     <p>Produces an <c>IO</c> functor that contains the computation which writes the supplied <c>format</c>
    ///        to the user's terminal.
    ///     </p>
    /// </summary>
    /// <param name='format'> the <c>string</c> to write to the user's terminal </param>
    /// <returns> an <c>IO</c> functor that writes the supplied <c>format</c> to the user's terminal </returns>
    let writeLine (format: string): IO<unit> =
        IO (fun () ->
            System.Console.WriteLine format
        )

    /// <summary>
    ///     <p>Produces an <c>IO</c> functor that contains the computation which reads the user's input from their
    ///        keyboard until they press the <c>enter</c> key.
    ///     </p>
    /// </summary>
    /// <param name='hide'> whether to show the text entered as the user enters it </param>
    /// <returns> an <c>IO</c> functor that reads the user's input </returns>
    let readLine (hide: bool): IO<string> =
        if not hide then IO System.Console.ReadLine
        else
            let rec scanner (accumulator: char list): char list =
                let info: System.ConsoleKeyInfo = System.Console.ReadKey true
                if info.Key = System.ConsoleKey.Enter then
                    List.rev accumulator
                else
                    scanner (info.KeyChar :: accumulator)

            IO (fun () -> [] |> (scanner >> System.String.Concat))
