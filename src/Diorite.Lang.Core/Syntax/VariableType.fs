// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    VariableType.fs
// Summary: The type definition for the VariableType type, which is the most basic form of storage in Diorite
// Author:  Arsngrobg
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Syntax

/// <summary>
///     <p>The structured representation of a <c>Variable</c> in the <b>Diorite</b> mathematics language.</p>
///     <p><b>1.</b> The first value (<c>char</c>) is the character which is the variable name (e.g. 'x').</p>
///     <p><b>2.</b> The second value (<c>int</c>) is the encoded subscript of the variable - this value is
///        optional, where a subscript of <c>0</c> internally represents the plain character (e.g. <c>'x'</c>) and
///        <c>10</c> internally represents the subscript-ed variable <c>"x9"</c>, which is the maximum amount of
///        subscript-ed permutations of the character.
///        <i>The encoded subscript is declared as an unsigned 8-bit integer.</i>
///     </p>
///     <p>For all characters of the alphabet (including lowercase &amp; uppercase), each with 11 unique
///        permutations, that means <b>Diorite</b> supports a total of <c>572</c> variables.
///     </p>
/// </summary>
type VariableType = char * uint8

/// <summary>
///     <p>a submodule, helper functions for the <c>VariableType</c>.</p>
/// </summary>
[<AutoOpen>]
module VariableTypeUtilities =
    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>VariableType</c>.</p>
    /// </summary>
    /// <param name="variableType"> the <c>VariableType</c> to obtain the <c>string</c> representation of </param>
    /// <returns> the <c>string</c> representation of the supplied <c>VariableType</c> </returns>
    let strVariableType (variableType: VariableType): string =
        let (c: char), (s: uint8) = variableType
        if s = 0uy then $"{c}" else $"{c}{s-1uy}"
