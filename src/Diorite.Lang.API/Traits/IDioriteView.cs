// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    IDioriteView.cs
// Summary: The definition of the IDioriteView interface - which declares internal behaviour for mapping an API type
//          to its Core type
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Traits;

/// <summary>
///     <p>The <c>IDioriteView&lt;T&gt;</c> is an interface type which describes that the implementing type is
///        mirroring a core type in the <c>Diorite.Lang.Core</c> project.
///     </p>
///     <p>This is intended by API developers to allow for interacting with the Core layer through the API layer, whilst
///        hiding implementation details.
///     </p>
/// </summary>
/// <typeparam name="T"> the equivalent (mirrored) core type of the implementing class </typeparam>
public interface IDioriteView<out T>
{
    /// <summary>
    ///     <p>Produces the representation of the implementing type as its equivalent Core type.</p>
    ///     <p>This is an <c>internal</c> method, used by the API for interacting with the Core layer, whilst hiding
    ///        implementation details.
    ///     </p>
    /// </summary>
    /// <returns> this type as it is structured as in the Core project </returns>
    internal T AsCoreType();
}
