// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionType.fs
// Summary: The type definition for the FunctionType, which is the structured representation of a function in
//          Diorite, it is the combination of FunctionAttributes and FunctionBody 
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax

/// <summary>
///     <p>The shorthand type abbreviation for a function in <b>Diorite</b>.</p>
/// </summary>
type FunctionType = FunctionAttributes * FunctionBody
