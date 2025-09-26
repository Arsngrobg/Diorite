// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    meta.fsx
// Summary: metadata for the Diorite project
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Meta

/// <summary>
///     The version module.
///     <b>Diorite</b> abides by a modified version of semantic versioning (SemVer), where the <c>Version</c> contains
///     two values: <c>major</c> & <c>minor</c>. These values represent the development stage of <b>Diorite</b>
/// </summary>
module version =
    /// <summary>
    ///     The <b>major</b> version component of <b>Diorite</b>.
    ///     <b>This should be accurate and updated accordingly.</b>
    /// </summary>
    let VERSION_MAJOR: uint8 = uint8 0
    /// <summary>
    ///     The <b>minor</b> version component of <b>Diorite</b>.
    ///     <b>This should be accurate and updated accordingly.</b>
    /// </summary>
    let VERSION_MINOR: uint8 = uint8 0

    /// <summary>
    ///     A struct, representing the current version of <b>Diorite</b> that is installed and currently running on the
    ///     user's system.
    /// </summary>
    ///
    /// <param name='major'>The <b>major</b> component of this <b>Diorite</b> language version</param>
    /// <param name='minor'>The <b>minor</b> component of this <b>Diorite</b> language version</param>
    [<Struct>]
    type Version(major: uint8, minor: uint8) =
        /// <summary>Transforms this <c>Version</c> struct into its <c>string</c> representation.</summary>
        /// <returns>the <c>string</c> representation of this <c>Version</c> struct</returns>
        member _.asString(): string = $"{major}.{minor}"

    /// <summary>This is the current language version for this instance of <b>Diorite</b>.</summary>
    /// <see cref='meta.version.Version'>sd</see>
    let public LANGUAGE_VERSION: Version = Version(VERSION_MAJOR, VERSION_MINOR)