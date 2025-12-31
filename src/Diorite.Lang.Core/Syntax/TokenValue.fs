// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    TokenValue.fs
// Summary: Definition for the TokenValue type, which is a payload for a Token in Diorite
// Author:  Arsngrobg, Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>This is the payload type for a <c>Token</c>.</p>
///     <p>The only data currently required for parsing:
///        <p><b>1.</b> the 64-bit floating-point representation of the number <c>Token</c> lexeme.</p>
///        <p><b>2.</b> the <c>VariableType</c> that correlates to the variable <c>Token</c> lexeme.</p>
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
type TokenValue =
    /// <summary>
    ///     <p>No payload value is stored by the <c>Token</c>.</p>
    /// </summary>
    | None
    /// <summary>
    ///     <p>A 64-bit, floating-point number is the payload for the <c>Token</c>.</p>
    /// </summary>
    | Number   of float
    /// <summary>
    ///     <p>The <c>VariableType</c> representation of the variable lexeme of the <c>Token</c>.</p>
    /// </summary>
    | Variable of VariableType