// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    AnonymousFunction.fs
// Summary: The type definition for the AnonymousFunction type
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>An <c>AnonymousFunction</c> in <b>Diorite</b> is defined using the <c>plot</c> syntax.</p>
///     <p><b>1.</b> the single <c>VariableType</c>, an anonymous function is restricted to a single parameter.</p>
///     <p><b>2.</b> the <c>Expression</c> the anonymous function is defined with.</p>
/// </summary>
type AnonymousFunction = {
    /// <summary>
    ///     <p>The single parameter type this function is bound to.</p>
    ///     <p><i>This value cannot be restricted by the set hint feature.</i></p>
    /// </summary>
    parameter:  VariableType
    /// <summary>
    ///     <p>The <c>Expression</c>, which is the body of the function.</p>
    ///     <p><i>Anonymous functions are only simple expression-based functions.</i></p>
    /// </summary>
    expression: Expression
}
