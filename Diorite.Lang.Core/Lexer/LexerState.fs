// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    LexerState.fs
// Summary: The type definition for the LexerState, which is a stateful value returned by GetNextToken
// Author:  Arsngrobg, Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Lexer

open Diorite.Lang.Core.Syntax

/// <summary>
///     <p>The state returned by the <c>Tokenise</c> function.</p>
///     <p><b>1.</b> The first value (<c>Token option</c>), which is the potential token.</p>
///     <p><b>2.</b> The second value (<c>char list</c>), which are the remaining characters.</p>
/// </summary>
type LexerState = (Token * char list) option
