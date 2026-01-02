// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    RuntimeConfiguration.fs
// Summary: The type definition of RuntimeConfiguration, a record for configuring the Diorite runtime.
// Author:  Arsngrobg, Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax

/// <summary>
///     <p>The <c>RuntimeConfiguration</c> record type is a configurable object that determines how the
///        <b>Diorite</b> runtime operates.
///     </p>
///     <p>This allows the <b>Diorite</b> interpreter to be embeddable in other software.</p>
/// </summary>
type RuntimeConfiguration = {
    /// <summary>
    ///     <p>The callback function that is called whenever the <c>plot &lt;Expression&gt;</c> syntax is evaluated.
    ///        It accepts a function that takes in a parameter, and produces a value, only 1-dimensional functions are
    ///        supported for plotting functions as the interpreter evaluates it as a <c>FunctionType</c> with a singular
    ///        argument.
    ///     </p>
    /// </summary>
    plotCallback: (ValueType -> ValueType) -> unit
    /// <summary>
    ///     <p>The initial memory state of the interpreter.</p>
    /// </summary>
    memory:       Memory
    /// <summary>
    ///     <p>The optional library to pre-evaluate for the <b>Diorite</b> runtime.</p>
    /// </summary>
    library:      unit
}
