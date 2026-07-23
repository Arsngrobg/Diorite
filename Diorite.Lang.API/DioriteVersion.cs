// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    DioriteVersion.cs
// Summary: The type definition of the DioriteVersion type for obtaining the version of Diorite and the version of
//          API that is currently being used
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API;

/// <summary>
///     <p>The <c>DioriteVersion</c> class holds data of a certain version of <b>Diorite</b>.</p>
///     <p>This class exposes two concrete <c>DioriteVersion</c> objects:
///        <p><b>1.</b> the Core version, which is baked into the core library of <b>Diorite</b>.</p>
///        <p><b>2.</b> the API version</p>
///     </p>
///     <p>These are two separate versions as the API/Core can update at different rates than the </p>
///     <p><b>Diorite</b> uses a very simplified version of the SemVer versioning scheme, where it only occupies a
///        <i>major</i> and <i>minor</i> version component. We state that any build above <c>1.0</c> is considered to be
///        stable.
///     </p>
/// </summary>
public sealed class DioriteVersion
{
    // change these to modify the current version of the Diorite API
    private const uint ApiMajorVersion = 1;
    private const uint ApiMinorVersion = 0;

    /// <summary>
    ///     <p>The <c>DioriteVersion</c> object that describes the current API version of <b>Diorite</b>.</p>
    /// </summary>
    public static readonly DioriteVersion ApiVersion  = new (ApiMajorVersion, ApiMinorVersion);
    /// <summary>
    ///     <p>The <c>DioriteVersion</c> object that describes the current Core version of <b>Diorite</b>.</p>
    /// </summary>
    public static readonly DioriteVersion CoreVersion = new (
        Core.Properties.Utilities.languageVersion.major,
        Core.Properties.Utilities.languageVersion.minor
    );

    /// <summary>
    ///     <p>The <i>major</i> version component of this <c>DioriteVersion</c> object.</p>
    /// </summary>
    public uint Major { get; }
    /// <summary>
    ///     <p>The <i>minor</i> version component of this <c>DioriteVersion</c> object.</p>
    /// </summary>
    public uint Minor { get; }
    
    private DioriteVersion(uint major, uint minor) =>
        (Major, Minor) = (major, minor);

    /// <summary>
    ///     <p>Compares the <c>DioriteVersion</c> object with this <c>DioriteVersion</c>.</p>
    /// </summary>
    /// <param name="other"> the <c>DioriteVersion</c> to compare with this <c>DioriteVersion</c> object </param>
    /// <returns> <c>true</c> if the unpacked <c>DioriteVersion</c> is older than this <c>DioriteVersion</c> </returns>
    public bool IsNewerThan(DioriteVersion other) =>
        IsNewerThan(other.Major, other.Minor);

    /// <summary>
    ///     <p>Compares the unpacked <c>DioriteVersion</c> object with this <c>DioriteVersion</c>.</p>
    /// </summary>
    /// <param name="major"> the <i>major</i> component of the unpacked <c>DioriteVersion</c> </param>
    /// <param name="minor"> the <i>minor</i> component of the unpacked <c>DioriteVersion</c> </param>
    /// <returns> <c>true</c> if the unpacked <c>DioriteVersion</c> is older than this <c>DioriteVersion</c> </returns>
    public bool IsNewerThan(uint major, uint minor)
    {
        if (Major > major) return true;
        if (Major < major) return true;

        return Minor > minor;
    }

    public override int GetHashCode() =>
        HashCode.Combine(Major, Minor);

    public override bool Equals(object? obj) =>
        obj is DioriteVersion other &&
        Major == other.Major && Minor == other.Minor;

    public override string ToString() =>
        $"{Major}.{Minor}";
}