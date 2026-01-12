// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    OperationRuleHelpers.fs
// Summary: Helper functions for the operation rules for ease-of-use
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax

/// <summary>
///     <p>Helper functions for the operation rule types for ease-of-use.</p>
/// </summary>
module OperationRuleHelpers =
    /// <summary>
    ///     <p>Upcasts the pair of values into equivalent types for relevant operations on equal types.</p>
    /// </summary>
    /// <param name="a"> the left-hand-side value </param>
    /// <param name="b"> the right-hand-side value </param>
    /// <returns> <c>a</c> &amp; <c>b</c>, as equal types, if possible </returns>
    let UpcastMin (a: ValueType, b: ValueType): ValueType * ValueType =
        match (a, b) with
         | ValueType.Integer  a,      ValueType.Float    b     -> (ValueType.Float (float a),       ValueType.Float b)
         | ValueType.Float  a,      ValueType.Integer    b     -> (ValueType.Float a,               ValueType.Float (float b))
         | a,                        b                         -> (a,                               b                       )

    /// <summary>
    ///     <p>Upcasts the pair of values into equivalent types for relevant operations on equal types.</p>
    /// </summary>
    /// <param name="a"> the left-hand-side value </param>
    /// <param name="b"> the right-hand-side value </param>
    /// <returns> <c>a</c> &amp; <c>b</c>, as equal types, if possible </returns>
    let UpcastMax (a: ValueType, b: ValueType): ValueType * ValueType =
        match UpcastMin (a, b) with
         | ValueType.Complex (a, b), ValueType.Integer  c      -> (ValueType.Complex (a, b),        ValueType.Complex (double c, 0))
         | ValueType.Integer a,      ValueType.Complex (b, c)  -> (ValueType.Complex (double a, 0), ValueType.Complex (b, c))
         | ValueType.Complex (a, b), ValueType.Float    c      -> (ValueType.Complex (a, b),        ValueType.Complex (double c, 0))
         | ValueType.Float    a,      ValueType.Complex (b, c) -> (ValueType.Complex (double a, 0), ValueType.Complex (b, c))
         | a,                        b                         -> (a,                               b                       )

    /// <summary>
    ///     <p>If the value of <c>x</c> is <c>nan</c>, it maps to <c>Undefined</c> or <c>Number</c> otherwise.</p>
    /// </summary>
    /// <param name="x"> the <c>decimal</c> number </param>
    /// <returns> the <c>ValueType</c> of <c>x</c>, where <c>nan</c> is <c>Undefined</c> </returns>
    let MaybeNaN (x: float): ValueType =
        if x |> System.Double.IsNaN then ValueType.Undefined else ValueType.Float x
