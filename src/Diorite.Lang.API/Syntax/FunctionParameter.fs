// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionParameter.fs
// Summary: The type definition for the structured representation of a parameter in Diorite 
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax

/// <summary>
///     <p>The <c>FunctionParameter</c> is a parameter in a function in <b>Diorite</b>.</p>
///     <p><b>1.</b> The first value (<c>VariableType</c>), which is the identifier for the parameter.</p>
///     <p><b>2.</b> The second value (<c>NumberSet</c>), which denotes the number set which the parameter must comply
///        with in order for the function to accept it.
///     </p>
/// </summary>
type FunctionParameter = VariableType * NumberSet