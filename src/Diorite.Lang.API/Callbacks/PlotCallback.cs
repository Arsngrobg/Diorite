// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    PlotCallback.cs
// Summary: The type definition for the PlotCallback delegate, the callback for function for the plot syntax
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Diorite.Lang.API.Syntax.Views;

namespace Diorite.Lang.API.Callbacks;

/// <summary>
///     <p>The function callback for when the <c>plot</c> syntax is used in <b>Diorite</b> code.</p>
///     <p>It is supplied a pre-baked function that the end user can use to do whatever they want with it.</p>
/// </summary>
/// <param name="anonFn"> the anonymous function </param>
/// <returns> the result of the anonymous function </returns>
public delegate void PlotCallback(Func<Value, Value> anonFn);
