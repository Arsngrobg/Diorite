// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionReferenceType.fs
// Summary: The type definition of the FunctionReferenceType type, which describes the reference types for a Diorite
//          function  
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>A <c>FunctionReferenceType</c> is all cases in which a <b>Diorite</b> function can be referenced with. All
///        functions can be referenced via the variable (e.g. <c>f(...)</c>), or its alias (symbol).
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
type FunctionReferenceType =
    /// <summary>
    ///     <p>A variable reference to a function.</p>
    ///     <p>This is a reference to the function via the variable table.</p>
    /// </summary>
    | OfVariable of VariableType
    /// <summary>
    ///     <p>A reserved string symbol reference to a function.</p>
    ///     <p>This is a reference to the function via the symbol dictionary.</p>
    /// </summary>
    | OfSymbol   of string
