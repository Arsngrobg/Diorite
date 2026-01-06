// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    NumberSet.fs
// Summary: The type definition for the NumberSet in Diorite
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>The number sets supported in the <b>Diorite</b> language.</p>
///     <p>These sets define the domain and/or range of a function.</p>
/// </summary>
[<RequireQualifiedAccess>]
type NumberSet =
    /// <summary>
    ///     <p>The set of all positive integers, including zero.</p>
    ///     <p><c>N = {0, ..., ∞}</c></p>
    /// </summary>
    | Natural
    /// <summary>
    ///     <p>The set of all whole numbers, including zero.</p>
    ///     <p><c>Z = {-∞, ..., 0, ..., ∞}</c></p>
    /// </summary>
    | Integer
    /// <summary>
    ///     <p>The set of all numbers that can be represented as points on an infinitely long number line.</p>
    ///     <p><c>R = {Q &amp; I}</c></p>
    /// </summary>
    | Real
    /// <summary>
    ///     <p>The set of all numbers that can be represented as a ratio of two integers that are not equal.</p>
    ///     <p><c>Q = {x | x = a/b &amp; (b != 0 OR b != a)}</c></p>
    /// </summary>
    | Rational
    /// <summary>
    ///     <p>The set of all numbers that cannot be represented as a ratio of two integers that are not equal.</p>
    ///     <p><c>{x | x != a/b &amp; (a != b OR b != a)}</c></p>
    /// </summary>
    | Irrational
    /// <summary>
    ///     <p>The set of all numbers in the form <c>a + bi</c>, where <c>i</c> is the imaginary unit sqrt(-1),
    ///        and <c>i^2 = -1</c>. <c>a</c> and <c>b</c> are <c>Real</c> numbers.
    ///     </p>
    ///     <p><c>C = {a + bi | a, b are Real numbers}</c></p>
    /// </summary>
    | Complex