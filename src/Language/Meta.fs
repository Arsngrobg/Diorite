// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Meta.fsx
// Summary: metadata for the Diorite project
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Meta

/// <summary>
///     The <c>Version</c> module.
///     <b>Diorite</b> abides by a modified version of semantic versioning (SemVer), where the <c>Version</c> contains
///     two values: <c>major</c> & <c>minor</c>. These values represent the development stage of <b>Diorite</b>.
/// </summary>
module Version =
    /// <summary>
    ///     The <b>major</b> version component of <b>Diorite</b>.
    ///     <b>This should be accurate and updated accordingly.</b>
    /// </summary>
    let VERSION_MAJOR: uint8 = uint8 0
    /// <summary>
    ///     The <b>minor</b> version component of <b>Diorite</b>.
    ///     <b>This should be accurate and updated accordingly.</b>
    /// </summary>
    let VERSION_MINOR: uint8 = uint8 4

    /// <summary>
    ///     A struct, representing the current version of <b>Diorite</b> that is installed and currently running on the
    ///     user's system.
    /// </summary>
    ///
    /// <param name='major'>The <b>major</b> component of this <b>Diorite</b> language version</param>
    /// <param name='minor'>The <b>minor</b> component of this <b>Diorite</b> language version</param>
    [<Struct>]
    type Version = {
        major: uint8
        minor: uint8
    }

    /// <summary>Transforms this <c>Version</c> struct into its <c>string</c> representation.</summary>
    /// <returns>the <c>string</c> representation of this <c>Version</c> struct</returns>
    let ver2str (v: Version): string =
        $"{v.major}{v.minor}"

    /// <summary>This is the current language version for this instance of <b>Diorite</b>.</summary>
    /// <see cref='meta.version.Version'>sd</see>
    let LANGUAGE_VERSION: Version = {
        major = VERSION_MAJOR;
        minor = VERSION_MINOR
    }

/// <summary>
///     The <c>Files</c> module.
///     This module contains data about file-specific attributes in the <b>Diorite</b> project.
/// </summary>
module Files =
    /// <summary>The file extension for <b>Diorite</b> language files.</summary>
    let FILE_EXTENSION: string = ".diorite"
    /// <summary>The name of the project.</summary>
    let PROG_NAME: string = "diorite"
    /// <summary>The TitleCase representation of the <c>PROG_NAME</c> value.</summary>
    let PROG_NAME_TITLE: string = $"{PROG_NAME.Substring(0, 1).ToUpper()}{PROG_NAME.Substring(1)}"
