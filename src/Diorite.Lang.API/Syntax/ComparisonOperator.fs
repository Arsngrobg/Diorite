// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    ComparisonOperator.fs
// Summary: The type definition for the ComparisonOperator in Diorite, an operation applied on a pair of ValueTypes 
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax

/// <summary>
///     <p>A <c>ComparisonOperator</c> is an operator that is used to compare to values either side of it.</p>
///     <p><i>Literal pattern: <c>a [ComparisonOperator] b</c></i></p>
/// </summary>
[<RequireQualifiedAccess>]
type ComparisonOperator =
    /// <summary>
    ///     <p>The comparison operator for checking the equality of two values (<c>a = b</c>).</p>
    /// </summary>
    | Equality
    /// <summary>
    ///     <p>The comparison operator for checking the inequality of two values (<c>a != b</c>).</p>
    /// </summary>
    | Inequality
    /// <summary>
    ///     <p>The comparison operator for checking if the left-hand value is less than, but not equal-to the
    ///        right-hand value (<c>a &lt; b</c>).
    ///     </p>
    /// </summary>
    | StrictLessThan
    /// <summary>
    ///     <p>The comparison operator for checking if the left-hand value is greater than, but not equal-to the
    ///        right-hand value (<c>a &gt; b</c>).
    ///     </p>
    /// </summary>
    | StrictGreaterThan
    /// <summary>
    ///     <p>The comparison operator for checking if the left-hand value is less than, or equal-to the right-hand
    ///        value (<c>a &lt;= b</c>).
    ///     </p>
    /// </summary>
    | NonStrictLessThan
    /// <summary>
    ///     <p>The comparison operator for checking if the left-hand value is greater than, or equal-to the
    ///        right-hand value (<c>a &lt;= b</c>).
    ///     </p>
    /// </summary>
    | NonStrictGreaterThan
