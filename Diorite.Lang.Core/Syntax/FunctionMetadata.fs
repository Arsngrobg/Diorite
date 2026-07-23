// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionMetadata.fs
// Summary: The type definition for the FunctionMetadata type, which is additional data about a Diorite function
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>The <c>FunctionMetadata</c> type are options relating to a function definition in <b>Diorite</b>.</p>
/// </summary>
type FunctionMetadata = {
    /// <summary>
    ///     <p>The optional alias for the function.</p>
    /// </summary>
    symbol:   string option
    /// <summary>
    ///     <p>Whether function calls should be inlined by the compiler.</p>
    ///     <p><i>This is a compile-time feature only.</i></p>
    /// </summary>
    inlined:  bool
    /// <summary>
    ///     <p>Whether previous function calls should be cached by</p>
    /// </summary>
    memoized: bool
}
