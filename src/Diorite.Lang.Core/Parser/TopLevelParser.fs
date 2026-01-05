// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    TopLevelParser.fs
// Summary: The top-level parsing functions for the Diorite language
// Author:  Arsngrobg
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Parser

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors
open Diorite.Lang.Core.Parser.DSL
open Diorite.Lang.Core.Parser.Combinators
open Diorite.Lang.Core.Lexer

/// <summary>
///     <p>The submodule for the top-level parsing functions for <b>Diorite</b>.</p>
/// </summary>
[<AutoOpen>]
module TopLevelParser =
    /// <summary>
    ///     <p>Parses the supplied <c>TokenStream</c>, and returns the error status. This may be a successful parse,
    ///        which contains the resulting Abstract Syntax Tree (AST), or the relevant error message if the syntax is
    ///        invalid.
    ///     </p>
    /// </summary>
    /// <param name="tokens"> the <c>TokenStream</c> to parse </param>
    /// <returns> a <c>Result</c>, which may, or may not, contain the resulting AST </returns>
    let ParseTokens (tokens: TokenStream): Result<AST> =
        match (Deferred StatementParser) tokens with
         | Ok    (root, []           ) -> Ok root
         | Ok    (_,    trailing :: _) ->
             ($"Unexpected trailing \"{trailing.lexeme}\" token", Some (trailing.line, trailing.column))
             ||> SyntaxError
         | Error err                   -> Error err

    /// <summary>
    ///     <p>Parses the supplied <c>string</c>, which is interpreted as <b>Diorite</b> source code. It returns the
    ///        error status. This may be a successful parse, which contains the resulting Abstract Syntax Tree (AST), or
    ///        the relevant error message if the syntax is invalid.
    ///     </p>
    /// </summary>
    /// <param name="str"> the <b>Diorite</b> source code to parse </param>
    /// <returns> a <c>Result</c>, which may, or may not, contain the resulting AST </returns>
    let ParseString (str: string): Result<AST> =
        let tokens: TokenStream = Tokenise str
        match (GetTokenizerError tokens) with
         | None      -> ParseTokens tokens
         | Some errs -> Error errs
