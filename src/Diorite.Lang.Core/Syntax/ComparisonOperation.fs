// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    ComparisonOperation.fs
// Summary: Contains the shorthand type definition for the structure of a ComparisonOperation in Diorite 
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>A tuple which represents the structure of a comparison operation in <b>Diorite</b>.</p>
///     <p>It consist of:
///        <p><b>1.</b> the left-hand side <c>Expression</c>.</p>
///        <p><b>2.</b> the <c>ComparisonOperator</c>.</p>
///        <p><b>3.</b> the right-hand side <c>Expression</c>.</p>
///     </p>
/// </summary>
type ComparisonOperation = Expression * ComparisonOperator * Expression
