// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Parser.fs
// Summary: The lexical analyzer for the Diorite language
// Author:  Arsngrobg, Borngle
// Version: v1.8
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>Parser</c> module groups up related bindings for parsing a token stream.
/// </summary>
[<RequireQualifiedAccess>]
module Parser =
    let parse
    let parseExpression (tokens: Token list): ParseResult =
        Success (Addition, [])


    /// <summary>
    ///     Analyses the provided token stream and parses it into a structured AST (Abstract Syntax Tree).
    /// </summary>
    /// <param name='tokens'> the token stream to parse </param>
    /// <returns> a <c>Result</c> that may contain the successful result of the parse, or <c>Failure</c> </returns>
    let parse(tokens: Token): unit Result =
        Success ()
