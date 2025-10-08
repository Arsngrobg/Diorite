// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Meta.fs
// Summary: metadata for the Diorite project
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>Identity</c> module groups up bindings that represent the <c>Diorite</c> language.
///     <code>
///         let langName = Identity.name
///         let progName = Identity.programName
///         let fileExt  = Identity.fileExtension
///         printf $"{langName}, {progName}, {fileExt}" // output: "Diorite, diorite, .diorite"
///     </code>
/// </summary>
module Identity =
    /// <summary>
    ///     A binding that returns the name of the language.
    /// </summary>
    /// <returns> the name of the language </returns>
    let name: string = "Diorite"

    /// <summary>
    ///     A binding that returns the name of the program.
    /// </summary>
    /// <returns> the name of the program </returns>
    let programName: string = name.ToLower()

    /// <summary>
    ///     A binding that returns the file extension this language uses for compiling/interpreting source files.
    /// </summary>
    /// <returns>the file extension this language recognises</returns>
    let fileExtension: string = $".{programName}"

/// <summary>
///     The <c>Version</c> module groups up bindings related to version metadata for the <c>Diorite</c> language.
///     <c>Diorite</c> uses a simplified version of semVer to indicate unique versions. It is composed of two values:
///     the <b>major</b> and <b>minor</b> numbers and any release of a new version past <c>1.0</c> is considered a
///     public and stable build.
///     <code>
///         let langVer = Version.languageVersion
///         printf $"{langVer}"
///     </code>
/// </summary>
module Version =
    // the major version component of the current language version
    let private majorVersion: uint8 = uint8 0

    // the minor version component of the current language version
    let private minorVersion: uint8 = uint8 4

    /// <summary>
    ///     A type that wraps a tuple, grouping the <b>major</b> and <b>minor</b> version components together.
    /// </summary>
    /// <param name="major"> the <b>major</b> version component </param>
    /// <param name="minor"> the <b>minor</b> version component </param>
    [<Struct>]
    type Version =
        {
            /// <summary> The <b>major</b> version component. </summary>
            major: uint8
            /// <summary> The <b>minor</b> version component. </summary>
            minor: uint8
        }

        override this.ToString(): string =
            $"{this.major}.{this.minor}"

    /// <summary>
    ///     A binding that returns the current language version, composing the <c>majorVersion</c> and
    ///     <c>minorVersion</c> bindings.
    /// </summary>
    let languageVersion: Version = {
        major = majorVersion;
        minor = minorVersion;
    }
