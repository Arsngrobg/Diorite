// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    DioriteError.fs
// Summary: The type definition for the DioriteError type, which is a non-terminating error type (in-terms of
//          live-interpreter execution). It signals errors within the language parsing and execution 
// Author:  Arsngrobg
// Version: v1.5
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Errors

open Diorite.Lang.Core.Syntax

/// <summary>
///     <p>A discriminated union type for an error in the <b>Diorite</b> language. Every error stores a message that
///        displays a descriptive message of what went wrong in the software. These are not <c>Exceptions</c> nor
///        are they thrown, however they need to be detected by the language in order to safely exit and return a
///        valid error code.
///     </p>
/// </summary>
type DioriteError =
    /// <summary>
    ///     <p>Caused by illegal math operations.</p>
    ///     <p><i>Example: division by zero</i></p>
    /// </summary>
    | MathError   of msg: string option * inputs: ValueType list
    /// <summary>
    ///     <p>Caused by invalid syntax or incorrect sequence of tokens.</p>
    /// </summary>
    | SyntaxError of msg: string * pos: (uint * uint) option
