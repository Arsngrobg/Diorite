// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Properties.fs
// Summary: The metadata for the Diorite project
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The metadata for the <b>Diorite</b> project.</p>
///     <p>It consists of attributes such as the <c>projectName</c> or <c>fileExtension</c>.</p>
/// </summary>
[<RequireQualifiedAccess>]
module Properties =
    /// <summary>
    ///     <p>The name of the project.</p>
    /// </summary>
    let projectName: string = "Diorite"

    /// <summary>
    ///     <p>The name of the executable.</p>
    /// </summary>
    let programName: string = "diorite"

    /// <summary>
    ///     <p>The file extension used to describe <b>Diorite</b> source code.</p>
    /// </summary>
    let fileExtension: string = ".diorite"
