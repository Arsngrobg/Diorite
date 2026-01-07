// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionReference.cs
// Summary: The type definition of the FunctionReference type, which describes the reference types for a Diorite
//          function  
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using System.Runtime;
using Diorite.Lang.API.Syntax.Views;

namespace Diorite.Lang.API.Syntax.Structure;

/// <summary>
///     <p>A <c>FunctionReferenceType</c> is all cases in which a <b>Diorite</b> function can be referenced with. All
///        functions can be referenced via the variable (e.g. <c>f(...)</c>), or its alias (symbol).
///     </p>
///     <p><i>This is a <b>Read-Only</b> type, meaning they cannot and should not be instantiated, rather returned by
///           API functions.
///     </i></p>
/// </summary>
public class FunctionReference
{
    /// <summary>
    ///     <p>The tag attribute to determine how this <c>FunctionReference</c> references the function.</p>
    /// </summary>
    public enum Type
    {
        /// <summary>
        ///     <p>A variable reference to a function.</p>
        ///     <p>This is a reference to the function via the variable table.</p>
        /// </summary>
        OfVariable,
        /// <summary>
        ///     <p>A reserved string symbol reference to a function.</p>
        ///     <p>This is a reference to the function via the symbol dictionary.</p>
        /// </summary>
        OfSymbol
    }

    public Type      Tag      { get; }
    public string?   Symbol   { get; }
    public Variable? Variable { get; }

    private FunctionReference(Type tag, string? symbol, Variable? variable) =>
        (Tag, Symbol, Variable) = (tag, symbol, variable);

    public override int GetHashCode() =>
        HashCode.Combine(Tag, Symbol, Variable);

    public override bool Equals(object? obj)
    {
        if (obj is not FunctionReference other)
            return false;

        return (Tag, other.Tag) switch
        {
            (Type.OfVariable, Type.OfVariable) => EqualityComparer<Variable>.Default.Equals(Variable, other.Variable),
            (Type.OfSymbol,   Type.OfSymbol  ) => EqualityComparer<string  >.Default.Equals(Symbol,   other.Symbol  ),
            _                                  => false
        };
    }

    public override string ToString() => Tag switch
    {
        Type.OfVariable => 
            Variable == null
            ? throw new AmbiguousImplementationException("Symbolic FunctionReference - but Symbol is null")
            : $"VariableFnRef({Variable})",
        Type.OfSymbol   =>
            Symbol == null
            ? throw new AmbiguousImplementationException("Variable FunctionReference - but Variable is null")
            : $"SymbolicFnRef({Symbol})",
        _                =>
            throw new AmbiguousImplementationException("FunctionReference has null tag")
    };
}