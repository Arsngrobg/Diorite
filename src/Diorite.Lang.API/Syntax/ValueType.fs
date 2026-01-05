// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Syntax.fs
// Summary: The definition for the ValueType, which is the fundamental (atomic) value in Diorite
// Author:  Arsngrobg
// Version: v1.4
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax

/// <summary>
///     <p>The union type which describe the cases in which a <c>Value</c> is represented as in <b>Diorite</b>.</p>
///     <p><b>1.</b> <c>Number</c>: a 64-bit, floating-point decimal.</p>
///     <p><b>2.</b> <c>Complex</c>: a pair of real values, <c>a + im(b)</c>, where <c>a</c> &amp; <c>b</c> are
///        <c>Real</c> numbers.
///     </p>
///     <p><b>3.</b> <c>Undefined</c>: denoting that a <b>variable</b> or operation is not properly defined.</p>
/// </summary>
type ValueType =
    /// <summary>
    ///     <p>A 64-bit, floating-point decimal.</p>
    /// </summary>
    | Number    of float
    /// <summary>
    ///     <p>A complex number of the form <c>a + bi</c>.</p>
    ///     <p>Where <c>a</c> &amp; <c>b</c> are <c>Real</c> numbers.</p>
    /// </summary>
    | Complex   of float * float
    /// <summary>
    ///     <p>An undetermined value.</p>
    ///     <p><i>Can also be seen as the absence of a value - like <c>null</c>.</i></p>
    /// </summary>
    | Undefined
