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

using Diorite.Lang.API.Syntax.Views;
using Diorite.Lang.API.Traits;

namespace Diorite.Lang.API.Syntax.Data;

/// <summary>
///     <p>The <c>FunctionParameter</c> is a parameter in a function in <b>Diorite</b>.</p>
///     <p><b>1.</b> The first value (<c>VariableType</c>), which is the identifier for the parameter.</p>
///     <p><b>2.</b> The second value (<c>NumberSet</c>), which denotes the number set which the parameter must comply
///        with in order for the function to accept it.
///     </p>
///     <p><i>This is a <b>Read-Only</b> type, meaning they cannot and should not be instantiated outside of API
///           functions.
///     </i></p>
/// </summary>
public sealed class FunctionParameter : ICoreView<Tuple<Tuple<char, byte>, Core.Syntax.NumberSet>>,
                                        ICoreConverter<Tuple<Tuple<char, byte>, Core.Syntax.NumberSet>, FunctionParameter>
{
    public static FunctionParameter OfCoreType(Tuple<Tuple<char, byte>, Core.Syntax.NumberSet> coreParameter)
    {
        var variable  = Variable.OfCoreType(coreParameter.Item1);
        var numberSet = (NumberSet) coreParameter.Item2.Tag; // STABLE AS LONG AS THE ABI IS ALSO STABLE
        return new FunctionParameter(variable, numberSet);
    }

    public Variable  Identifier { get; }
    public NumberSet Set        { get; }

    private FunctionParameter(Variable identifier, NumberSet set) =>
        (Identifier, Set) = (identifier, set);

    public Tuple<Tuple<char, byte>, Core.Syntax.NumberSet> AsCoreType()
    {
        var mappedSet = Set switch
        {
            NumberSet.Natural    => Core.Syntax.NumberSet.Natural,
            NumberSet.Integer    => Core.Syntax.NumberSet.Integer,
            NumberSet.Real       => Core.Syntax.NumberSet.Real,
            NumberSet.Rational   => Core.Syntax.NumberSet.Rational,
            NumberSet.Irrational => Core.Syntax.NumberSet.Irrational,
            NumberSet.Complex    => Core.Syntax.NumberSet.Complex,
            _ => throw new InvalidOperationException("Not all NumberSet cases were complete")
        };
        return new Tuple<Tuple<char, byte>, Core.Syntax.NumberSet>(Identifier.AsCoreType(), mappedSet);
    }

    /// <summary>
    ///     <p>Checks whether this <c>FunctionParameter</c> is expected to be a <c>Natural</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionParameter</c> is expected to be a <c>Natural</c> number. </returns>
    public bool ShouldBeNatural() =>
        Set is NumberSet.Natural;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionParameter</c> is expected to be a <c>Integer</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionParameter</c> is expected to be a <c>Integer</c> number. </returns>
    public bool ShouldBeInteger() =>
        Set is NumberSet.Integer;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionParameter</c> is expected to be a <c>Real</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionParameter</c> is expected to be a <c>Real</c> number. </returns>
    public bool ShouldBeReal() =>
        Set is NumberSet.Real;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionParameter</c> is expected to be a <c>Rational</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionParameter</c> is expected to be a <c>Rational</c> number. </returns>
    public bool ShouldBeRational() =>
        Set is NumberSet.Rational;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionParameter</c> is expected to be a <c>Irrational</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionParameter</c> is expected to be a <c>Irrational</c> number. </returns>
    public bool ShouldBeIrrational() =>
        Set is NumberSet.Irrational;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionParameter</c> is expected to be a <c>Complex</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionParameter</c> is expected to be a <c>Complex</c> number. </returns>
    public bool ShouldBeComplex() =>
        Set is NumberSet.Complex;

    public override int GetHashCode() =>
        HashCode.Combine(Identifier, Set);

    public override bool Equals(object? obj) =>
        obj is FunctionParameter other &&
        Identifier.Equals(other.Identifier) && Set == other.Set;

    public override string ToString() =>
        $"{Identifier}: {Set}";
}