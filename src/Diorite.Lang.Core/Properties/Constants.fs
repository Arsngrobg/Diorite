// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Constants.fs
// Summary: The constants for the Properties namespace  
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

module Diorite.Lang.Core.Properties.Utilities

/// <summary>
///     <p>The name of the project.</p>
/// </summary>
let projectName: string   = "Diorite"

/// <summary>
///     <p>The name of the executable that is generated.</p>
/// </summary>
let programName: string   = "diorite"

/// <summary>
///     <p>The file extension used to describe <b>Diorite</b> source code.</p>
/// </summary>
let fileExtension: string = ".diorite"

/// <summary>
///     The current version of <b>Diorite</b>.
/// </summary>
let languageVersion: Version = {
    major = 0 |> uint8
    minor = 8 |> uint8
}
