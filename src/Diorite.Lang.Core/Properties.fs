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
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The metadata for the <b>Diorite</b> project.</p>
///     <p>It consists of attributes such as the <c>projectName</c> or <c>fileExtension</c>.</p>
///     <p>It also contains the version data for this version of <b>Diorite</b>.</p>
/// </summary>
[<RequireQualifiedAccess>]
module Properties =
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
    ///     A <c>Version</c> struct that holds the <b>major</b> and <b>minor</b> version of <b>Diorite</b> that is
    ///     currently running on the user's computer.
    /// </summary>
    [<Struct>]
    type Version = {
        major: uint8
        minor: uint8
    }

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>Version</c>.</p>
    /// </summary>
    /// <param name='version'> the <c>Version</c> </param>
    /// <returns> the <c>string</c> representation of the <c>Version</c> </returns>
    let strVersion (version: Version): string =
        $"{version.major}.{version.minor}"

    /// <summary>
    ///     The current version of <b>Diorite</b>.
    /// </summary>
    let languageVersion: Version = {
        major = 0 |> uint8
        minor = 8 |> uint8
    }
