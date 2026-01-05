// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionResult.fs
// Summary: The type definition for the FunctionResult, which contains the different paths a return value of a
//          Diorite function can take 
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax

/// <summary>
///     <p>A <c>FunctionResult</c> is exactly that, a result from a function that may be an <c>Expression</c>, or an
///        error with an optional error message.
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
type FunctionResult =
    /// <summary>
    ///     <p>An <c>Expression</c> returned by the function.</p>
    /// </summary>
    | Expression of Expression
    /// <summary>
    ///     <p>An error thrown by the function.</p>
    /// </summary>
    | Error      of string option
