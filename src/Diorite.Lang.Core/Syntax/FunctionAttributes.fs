// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionAttributes.fs
// Summary: The type definition for the FunctionAttributes type, which contains essential data for identifying, and
//          executing the body of the function 
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>The <c>FunctionMetadata</c> record type contains type information about a function definition.</p>
///     <p>It also maintains the identifier which this function is bound to, and its <c>FunctionMetadata</c>.</p>
///     <p><i>This is a concrete field, meaning it cannot be modified after the function definition.</i></p>
/// </summary>
type FunctionAttributes = {
    /// <summary>
    ///     <p>The <c>VariableType</c> that identifies this function.</p>
    ///     <p>This is the identifier that was used to originally define the function, however is not always
    ///        accurate to what is in memory at a given moment.
    ///     </p>
    /// </summary>
    identifier: VariableType
    /// <summary>
    ///     <p>The list of parameters that the function requires to execute.</p>
    /// </summary>
    parameters: FunctionParameter list
    /// <summary>
    ///     <p>The return <i>type</i> of the function.</p>
    ///     <p>This is used to check for set membership on function return.</p>
    /// </summary>
    range:      NumberSet
    /// <summary>
    ///     <p>The metadata for the function.</p>
    /// </summary>
    metadata:   FunctionMetadata
}
