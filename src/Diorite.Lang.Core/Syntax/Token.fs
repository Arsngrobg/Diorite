// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Token.fs
// Summary: The type definition for the Token type, which is an atomic lexical unit in Diorite 
// Author:  Arsngrobg, Borngle
// Version: v1.3
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>A <c>Token</c> is an atomic lexical unit in the <b>Diorite</b> language.</p>
/// </summary>
type Token = {
    /// <summary>
    ///     <p>The string literal this <c>Token</c> represents.</p>
    /// </summary>
    lexeme: string
    /// <summary>
    ///     <p>The identifier for this <c>Token</c>.</p>
    /// </summary>
    id:     TokenType
    /// <summary>
    ///     <p>The payload value this <c>Token</c> stores.</p>
    /// </summary>
    value:  TokenValue
    /// <summary>
    ///     <p>The line this <c>Token</c> is found on.</p>
    ///     <p>This is the relative line number from the lexer context.</p>
    /// </summary>
    line:   uint
    /// <summary>
    ///     <p>The column this <c>Token</c> is found on.</p>
    ///     <p>This is the relative line number from the lexer context.</p>
    /// </summary>
    column: uint
}
