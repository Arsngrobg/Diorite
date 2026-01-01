// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Version.fs
// Summary: The type definition for the Version struct type 
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Properties

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
///     <p>A submodule, the utility functions for the <c>Version</c> type.</p>
/// </summary>
[<AutoOpen>]
module VersionUtilities =
    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>Version</c>.</p>
    /// </summary>
    /// <param name='version'> the <c>Version</c> </param>
    /// <returns> the <c>string</c> representation of the <c>Version</c> </returns>
    let strVersion (version: Version): string =
        $"{version.major}.{version.minor}"
