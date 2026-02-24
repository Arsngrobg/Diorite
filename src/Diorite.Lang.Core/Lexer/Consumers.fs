// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Consumers.fs
// Summary: The predefined set of Consumer types
// Author:  Arsngrobg, Borngle
// Version: v1.3
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Lexer

/// <summary>
///     <p>The library of predefined <c>Consumer</c> for the tokenizer.</p>
/// </summary>
module Consumers =
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are letters.</p>
    /// </summary>
    let ConsumeLetters:      Consumer = Consume System.Char.IsLetter
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are digits.</p>
    /// </summary>
    let ConsumeDigits:       Consumer = Consume System.Char.IsDigit
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are blank.</p>
    /// </summary>
    let ConsumeBlanks:       Consumer = Consume (fun c -> c <> '\n' && System.Char.IsWhiteSpace c)
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are non-newline.</p>
    /// </summary>
    let ConsumeNonNewlines:  Consumer = Consume (fun c -> c <> '\n')
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are newline.</p>
    /// </summary>
    let ConsumeOnlyNewlines: Consumer = Consume (fun c -> c = '\n')
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that they are not blank.</p>
    /// </summary>
    let ConsumeUntilBlank:   Consumer = Consume (System.Char.IsWhiteSpace >> not)
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that it is not a double quote.</p>
    /// </summary>
    let ConsumeUntilQuotes:  Consumer = Consume (fun c -> c <> '"')
    /// <summary>
    ///     <p>A <c>Consumer</c> that consumes characters, given that it is part of a valid symbol name.</p>
    /// </summary>
    let ConsumeSymbol:       Consumer = Consume (fun c -> (System.Char.IsLetter c) || c = '_')
