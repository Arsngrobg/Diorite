// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    TokenStream.fs
// Summary: The type definition for the TokenStream, a sequence of ordered Tokens
// Author:  Arsngrobg, Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Lexer

open Diorite.Lang.Core.Syntax

/// <summary>
///     <p>A <c>TokenStream</c> is a sequence of ordered <c>Token</c>s.</p>
///     <p>As of now, it is the type definition for a <c>Token list</c>.</p>
/// </summary>
type TokenStream = Token list
