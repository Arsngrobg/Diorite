// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    UnaryOperationRule.fs
// Summary: The type definition for the UnaryOperationRule, which defines the behaviour for a specific unary
//          operation on ValueType
// Author:  Arsngrobg
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors

/// <summary>
///     <p>Defines a function that accepts a single value that may produce a <c>ValueType</c> or an error.</p>
/// </summary>
type UnaryOperationRule = ValueType -> ValueType Result
