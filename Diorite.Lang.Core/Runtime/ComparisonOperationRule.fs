// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    ComparisonOperationRule.fs
// Summary: The type definition for the ComparisonOperationRule, which defines the behaviour for a specific
//          comparison operation on a pair of ValueTypes
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors

/// <summary>
///     <p>Defines a function that accepts a pair of values that may produce a <c>bool</c> or an error.</p>
/// </summary>
type ComparisonOperationRule = ValueType * ValueType -> bool Result
