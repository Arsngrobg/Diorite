// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Result.fs
// Summary: Redefinition of the F# Result type, but the Error is strictly bound to DioriteError 
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Errors

/// <summary>
///     <p>A stricter version of the standard <c>FSHarp.Core.Result</c> where its <c>Error</c> case is strictly bound to
///        the <c>DioriteError</c> type.
///     </p>
///     <p>This should be used over the standard <c>Result</c> type.</p>
/// </summary>
type Result<'a> = Result<'a, DioriteError>
