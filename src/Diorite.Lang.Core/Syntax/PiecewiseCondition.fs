// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    PiecewiseCondition.fs
// Summary: The type definition for the structured representation of a PiecewiseCondition in Diorite 
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>The structured representation of a piecewise condition in a <b>Diorite</b> function.</p>
///     <p>It's a tuple which holds:
///        <p><b>1.</b> the <c>Expression</c> to be evaluated if the <c>ComparisonOperation</c> evaluates to
///           <c>true</c>.
///        </p>
///        <p><b>2.</b> the <c>ComparisonOperation</c> which is the predicate to be tested.
///           This decides whether the function should return the <c>Expression</c> to the left of it, or move down
///           the conditional chain.
///        </p>
///     </p>
/// </summary>
type PiecewiseCondition = FunctionResult * ComparisonOperation

/// <summary>
///     <p>A submodule, which describes the factory functions for <c>PiecewiseCondition</c>s.</p>
/// </summary>
[<AutoOpen>]
module PiecewiseConditionFactories =
    /// <summary>
    ///     <p>A subtype of the <c>PiecewiseCondition</c> type where its <c>ComparisonOperation</c> will always evaluate
    ///        to <c>true</c>.
    ///     </p>
    ///     <p>This models the <c>Expression otherwise</c> syntax using the <c>PiecewiseCondition</c> type.</p>
    /// </summary>
    /// <param name="defaultResult"> the default <c>Expression</c> to return </param>
    /// <returns> a <c>PiecewiseCondition</c> that will always evaluate to <c>true</c> </returns>
    let PiecewiseBaseCase (defaultResult: FunctionResult): PiecewiseCondition = (
        defaultResult,
        (
            ValueType.Undefined |> Expression.Value,
            ComparisonOperator.Equality,
            ValueType.Undefined |> Expression.Value
        )
    )
