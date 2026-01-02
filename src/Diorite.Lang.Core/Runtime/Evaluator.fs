// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Evaluators.fs
// Summary: The type definition of the Evaluator type
// Author:  Arsngrobg, Borngle
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors
open Diorite.Lang.Core.Runtime.VirtualMemory

/// <summary>
///     <p>The <c>Evaluator</c> is a function that accepts a value <c>'a</c> and <c>Memory</c>, produces a
///        <c>(ValueType * Memory) Result</c>.
///     </p>
/// </summary>
type Evaluator<'a> = 'a -> Memory -> (ValueType * Memory) Result
