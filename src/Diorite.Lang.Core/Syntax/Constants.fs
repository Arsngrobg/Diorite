// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Constants.fs
// Summary: Contains all of the constant values 
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

[<AutoOpen>]
module Diorite.Lang.Core.Syntax.Constants

/// <summary>
///     <p>The value of infinity.</p>
///     <p><i>It is the constant wrapped as a <c>ValueType.Number</c></i>.</p>
/// </summary>
let ConstantInfinity: ValueType = infinity |> ValueType.Number

/// <summary>
///     <p>The constant <c>Pi</c> (π).</p>
///     <p><i>It is the constant wrapped as a <c>ValueType.Number</c>.</i></p>
/// </summary>
let ConstantPi: ValueType = System.Math.PI |> ValueType.Number

/// <summary>
///     <p>The constant <c>Tau</c> (Τ).</p>
///     <p><i>It is the constant wrapped as a <c>ValueType.Number</c>.</i></p>
/// </summary>
let ConstantTau: ValueType = System.Math.Tau |> ValueType.Number

/// <summary>
///     <p>The constant <c>Euler</c> (e).</p>
///     <p><i>It is the constant wrapped as a <c>ValueType.Number</c>.</i></p>
/// </summary>
let ConstantEuler: ValueType = System.Math.E |> ValueType.Number

/// <summary>
///     <p>The maximum number of variables supported by <b>Diorite</b>.</p>
///     <p>Breakdown:
///        <p><b>1.</b> <c>26</c> lowercase + <c>26</c> uppercase characters</p>
///        <p><b>2.</b> <c>1</c> character + <c>10</c> subscript variations of the same character.</p>
///        <p>Therefore, <c>(26 + 26) * (1 + 10) = 572</c> unique variables.</p>
///     </p>
/// </summary>
let MaxVariables: int = (26 + 26) * (1 + 10)

/// <summary>
///     <p>The default number set if none is provided.</p>
///     <p>Any parameter without the domain definition operator defaults to <c>Real</c>.</p>
///     <p>Any function without the range definition operator defaults to <c>Real</c> also.</p>
/// </summary>
let DefaultNumberSet: NumberSet = NumberSet.Real

/// <summary>
///     <p>The default <c>FunctionMetadata</c> for a <b>Diorite</b> function.</p>
///     <p>If no metadata attributes are given, by default, all values are 'empty'.</p>
/// </summary>
let DefaultFunctionMetadata: FunctionMetadata = {
    symbol   = None
    inlined  = false
    memoized = false
}
