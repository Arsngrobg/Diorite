// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Version.fs
// Summary: The versioning data for the Diorite project
// Author:  Arsngrobg
// Version: v1.3
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The <c>Version</c> module groups up bindings related to storing the version metadata of the <b>Diorite</b>
///        language currently running on the user's device.
///     </p>
///     <p><b>Diorite</b> uses a modified version of <i>semVer</i> to indicate unique versions.
///        Two version components are used: the <b>major</b> component, where any version past <c>1.0</c> means that
///        the current build of <b>Diorite</b> is considered a public and stable build; and the <b>minor</b> version,
///        which denotes a small change in the current <b>major</b> version of <b>Diorite</b>.
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
module Version =
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
