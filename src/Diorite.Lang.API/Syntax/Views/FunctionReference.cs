// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionReference.cs
// Summary: The type definition of the FunctionReferenceType type, which describes the reference types for a Diorite
//          function  
// Author:  Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Diorite.Lang.API.Traits;

namespace Diorite.Lang.API.Syntax.Views;

public abstract class FunctionReference : ICoreView<Core.Syntax.FunctionReferenceType>,
                                          ICoreConverter<Core.Syntax.FunctionReferenceType, FunctionReference>
{
    public static FunctionReference OfCoreType(Core.Syntax.FunctionReferenceType coreRef) => coreRef switch
    {
        Core.Syntax.FunctionReferenceType.OfVariable varRef => new OfVariable(Variable.OfCoreType(varRef.Item)),
        Core.Syntax.FunctionReferenceType.OfSymbol   symRef => new OfSymbol(symRef.Item),
        _ => throw new InvalidOperationException("All match cases were not covered")
    };

    /// <summary>
    ///     <p>Performs pattern matching on this <c>FunctionReference</c>.</p>
    /// </summary>
    /// <param name="onVariable"> the callback to execute if this <c>FunctionReference</c> is of a variable </param>
    /// <param name="onSymbol"> the callback to execute if this <c>FunctionReference</c> is of  symbol </param>
    /// <typeparam name="T"> the result of this pattern matching operation </typeparam>
    /// <returns> the result of the pattern match, bound by the type <c>T</c> </returns>
    public T Match<T>(Func<Variable, T> onVariable, Func<string, T> onSymbol) => this switch
    {
        OfVariable varRef => onVariable(varRef.Variable),
        OfSymbol symRef   => onSymbol  (symRef.Symbol),
        _                 => throw new InvalidOperationException($"Invalid case {GetType()}")
    };
    
    private FunctionReference() {}

    public abstract Core.Syntax.FunctionReferenceType AsCoreType();
    public abstract override int    GetHashCode();
    public abstract override bool   Equals(object? obj);
    public abstract override string ToString();

    /// <summary>
    ///     <p>The case of <c>FunctionReference</c> where a <i>variable reference</i> is used when invoking a
    ///        <b>Diorite</b> function.
    ///     </p>
    /// </summary>
    private sealed class OfVariable : FunctionReference
    {
        /// <summary>
        ///     <p>The variable this <c>FunctionReference</c> is describing.</p>
        /// </summary>
        public Variable Variable { get; }

        internal OfVariable(Variable variable) =>
            Variable = variable;

        public override Core.Syntax.FunctionReferenceType AsCoreType() =>
            Core.Syntax.FunctionReferenceType.NewOfVariable(Variable.AsCoreType());

        public override int GetHashCode() =>
            Variable.GetHashCode();

        public override bool Equals(object? obj) =>
            obj is OfVariable other &&
            Variable.Equals(other.Variable);

        public override string ToString() =>
            $"VariableFunctionReference[{Variable}]";
    }

    /// <summary>
    ///     <p>The case of <c>FunctionReference</c> where a <i>symbolic reference</i> is used when invoking a
    ///        <b>Diorite</b> function.
    ///     </p>
    /// </summary>
    private sealed class OfSymbol : FunctionReference
    {
        /// <summary>
        ///     <p>The symbol this <c>FunctionReference</c> is describing.</p>
        /// </summary>
        public string Symbol { get; }

        internal OfSymbol(string symbol) =>
            Symbol = symbol;

        public override Core.Syntax.FunctionReferenceType AsCoreType() =>
            Core.Syntax.FunctionReferenceType.NewOfSymbol(Symbol);

        public override int GetHashCode() =>
            Symbol.GetHashCode();

        public override bool Equals(object? obj) =>
            obj is OfSymbol other &&
            Symbol.Equals(other.Symbol);

        public override string ToString() =>
            $"SymbolicFunctionReference[{Symbol}]";
    }
}