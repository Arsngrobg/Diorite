// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    VariableType.cs
// Summary: The type definition for the VariableType type, which is the most basic form of storage in Diorite
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax;

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
public class VariableType
{
    /// <summary>
    ///     <p>The maximum number of variables supported by <b>Diorite</b>.</p>
    ///     <p>Breakdown:
    ///        <p><b>1.</b> <c>26</c> lowercase + <c>26</c> uppercase characters</p>
    ///        <p><b>2.</b> <c>1</c> character + <c>10</c> subscript variations of the same character.</p>
    ///        <p>Therefore, <c>(26 + 26) * (1 + 10) = 572</c> unique variables.</p>
    ///     </p>
    /// </summary>
    public const int  MaxVariables = (26 + 26) * (1 + 10);
    /// <summary>
    ///     <p>The constant value, denoting no subscript.</p>
    /// </summary>
    public const byte NoSubscript  = 0;
    /// <summary>
    ///     <p>The constant value, denoting the maximum subscript possible for a <b>Diorite</b> variable.</p>
    /// </summary>
    public const byte MaxSubscript = 10;

    /// <summary>
    ///     <p>Creates a new <c>VariableType</c> object from the supplied alphabetical, upper-case or lower-case
    ///        <c>letter</c>.
    ///     </p>
    /// </summary>
    /// <param name="letter"> the alphabetical character </param>
    /// <returns> a new <c>VariableType</c> instance, provided that the <c>letter</c> is alphabetical </returns>
    /// <exception cref="ArgumentException"> if <c>letter</c> is not alphabetical </exception>
    public static VariableType OfCharacter(char letter) => Subscriptable(letter, NoSubscript);

    /// <summary>
    ///     <p>Creates a new <c>VariableType</c> object from the supplied alphabetical, upper-case or lower-case
    ///        <c>letter</c> and unsigned, 8-bit integer <c>subscript</c>.
    ///     </p>
    /// </summary>
    /// <param name="letter"> the alphabetical character </param>
    /// <param name="subscript"> the unsigned 8-bit integer, denoting the encoded subscript </param>
    /// <returns> a new <c>VariableType</c> instance, provided that the arguments are valid </returns>
    /// <exception cref="ArgumentException">
    ///     if <c>letter</c> is not alphabetical, or <c>subscript</c> is greater than <c>10</c>
    /// </exception>
    public static VariableType Subscriptable(char letter, byte subscript)
    {
        if (!char.IsUpper(letter) && !char.IsLower(letter))
            throw new ArgumentException(
                "letter must be an alphabetical uppercase or lowercase character",
                nameof(letter)
            );
        
        if (subscript > MaxSubscript)
            throw new ArgumentException(
                $"subscript must be less than MAX_SUBSCRIPT ({MaxSubscript})",
                nameof(subscript)
            );

        return new VariableType(letter, subscript);
    }

    /// <summary>
    ///     <p>The alphabetical character which denotes the variable name <i>(e.g. 'x')</i>.</p>
    /// </summary>
    private char Letter    { get; }
    /// <summary>
    ///     <p>The encoded subscript of the variable - this value is optional, where a subscript of <c>0</c> internally
    ///        represents the plain character (e.g. <c>'x'</c>) and  <c>10</c> internally represents the subscript-ed
    ///        variable <c>"x9"</c>, which is the maximum amount of subscript-ed permutations of the character.
    ///        <i>The encoded subscript is declared as an unsigned 8-bit integer.</i>
    ///     </p>
    /// </summary>
    private byte Subscript { get; }

    private VariableType(char letter, byte subscript)
    {
        Letter    = letter;
        Subscript = subscript;
    }

    /// <summary>
    ///     <p>Produces the representation of this <c>VariableType</c> as a <c>tuple</c>.</p>
    ///     <p>Used for interop between the <b>API</b> and the <b>core</b> library.</p>
    /// </summary>
    /// <returns> the structured representation of this <c>VariableType</c> (<c>tuple</c> form) </returns>
    public Tuple<char, byte> AsTuple()
    {
        return new Tuple<char, byte>(Letter, Subscript);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(Letter, Subscript);
    }

    public override bool Equals(object? obj)
    {
        if (obj is not VariableType other) return false;
        return (Letter == other.Letter) && (Subscript == other.Subscript);
    }

    public override string ToString()
    {
        return (Subscript == 0) ? $"{Letter}" : $"{Letter}{Subscript - 1}";
    }
}