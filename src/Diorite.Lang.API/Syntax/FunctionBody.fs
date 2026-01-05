// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionBody.fs
// Summary: The type definition for the FunctionBody type, which describes the variations of functions that are
//          possible in Diorite 
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax

/// <summary>
///     <p>The structured representation of a function body in <b>Diorite</b>.</p>
///     <p>A function body in <b>Diorite</b> either contains:
///        <p><b>1.</b> a single <c>Expression</c></p>
///        <p><b>2.</b> a chain of piecewise conditions</p>
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
type FunctionBody =
    /// <summary>
    ///     <p>Describes a function with a single <c>Expression</c>.</p>
    /// </summary>
    | Expression          of Expression
    /// <summary>
    ///     <p>Describes a function consisting of a chain of piecewise conditions.</p>
    /// </summary>
    | PiecewiseConditions of PiecewiseCondition list