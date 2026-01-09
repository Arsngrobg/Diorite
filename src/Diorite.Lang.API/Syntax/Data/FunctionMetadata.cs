// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionMetadata.cs
// Summary: The type definition for the FunctionMetadata type, which is additional data about a Diorite function
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Microsoft.FSharp.Core;

namespace Diorite.Lang.API.Syntax.Data;

/// <summary>
///     <p>The <c>FunctionMetadata</c> type are options relating to a function definition in <b>Diorite</b>.</p>
/// </summary>
public sealed class FunctionMetadata
{
    /// <summary>
    ///     <p>Creates a new <c>FunctionMetadata</c> from its equivalent core type.</p>
    /// </summary>
    /// <param name="coreMetadata"> the equivalent core type </param>
    /// <returns> a new <c>FunctionMetadata</c>, derived from its equivalent core type </returns>
    internal static FunctionMetadata OfCoreType(Core.Syntax.FunctionMetadata coreMetadata) =>
        new (
            FSharpOption<string>.get_IsNone(coreMetadata.symbol)
            ? null
            : coreMetadata.symbol.Value,
            coreMetadata.inlined,
            coreMetadata.memoized
        );

    /// <summary>
    ///     <p>The optional alias for the function.</p>
    /// </summary>
    public string? Symbol   { get; }
    /// <summary>
    ///     <p>Whether function calls should be inlined by the compiler.</p>
    ///     <p><i>This is a compile-time feature only.</i></p>
    /// </summary>
    public bool    Inlined  { get; }
    /// <summary>
    ///     <p>Whether previous function calls should be cached by</p>
    /// </summary>
    public bool    Memoized { get; }

    private FunctionMetadata(string? symbol, bool inlined, bool memoized) =>
        (Symbol, Inlined, Memoized) = (symbol, inlined, memoized);

    /// <summary>
    ///     <p>Checks whether this <c>FunctionMetadata</c> has a symbolic alias or not.</p>
    /// </summary>
    /// <returns> if this <c>FunctionMetadata</c> has a symbolic alias </returns>
    public bool HasSymbolicAlias() =>
        Symbol != null;

    public override int GetHashCode() =>
        HashCode.Combine(Symbol, Inlined, Memoized);

    public override bool Equals(object? obj) =>
        obj is FunctionMetadata other &&
        Symbol == other.Symbol && Inlined == other.Inlined && Memoized == other.Memoized;

    public override string ToString() =>
        Symbol == null
        ? $"FunctionMetadata[Inlined: {Inlined}, Memoized: {Memoized}]"
        : $"FunctionMetadata[Symbol: {Symbol}, Inlined: {Inlined}, Memoized: {Memoized}]";
}