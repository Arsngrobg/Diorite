// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    ICoreConverter.cs
// Summary: The definition of the ICoreConverter interface - which declares internal behaviour for a static factory
//          method that transforms an API object into a Core object
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Traits;

/// <summary>
///     <p>The <c>ICoreView&lt;TC, TA&gt;</c> is an interface type which describes that the implementing class will
///        include a static factory method for converting a Core type into an API type.
///     </p>
///     <p>This is intended by API developers to allow for interacting with the Core layer through the API layer, whilst
///        hiding implementation details.
///     </p>
/// </summary>
/// <typeparam name="TC"> the equivalent (mirrored) core type of the implementing class </typeparam>
/// <typeparam name="TA"> the implementing class </typeparam>
internal interface ICoreConverter<in TC, out TA>
{
    /// <summary>
    ///     <p>Creates a new instance of the <c>TA</c> type from the core <c>TC</c> type.</p>
    /// </summary>
    /// <param name="coreType"> the core type to derive the API type from </param>
    /// <returns> a new instance of <c>TA</c>, derived from the input <c>TC</c> type </returns>
    internal static abstract TA OfCoreType(TC coreType);
}