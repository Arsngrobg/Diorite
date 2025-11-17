// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    BaseTypes.fs
// Summary: Implementation details for BaseTypes.fsi
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

[<AutoOpen>]
module BaseTypes =
    // Implementation
    type VariableType = char * int

    // Implementation
    let strVariable (variable: VariableType): string =
        let (character: char), (subscript: int) = variable
        if subscript < 0  then (invalidArg "subscript") "encoded subscript value cannot be negative"
        if subscript > 10 then (invalidArg "subscript") "encoded subscript value cannot be greater than 10"
        if subscript = 0 then $"{character}" else $"{character}{subscript - 1}"
