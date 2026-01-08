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

namespace Diorite.Lang.API.Syntax.Data;

/// <summary>
///     <p>The <c>FunctionParameter</c> is a parameter in a function in <b>Diorite</b>.</p>
///     <p><b>1.</b> The first value (<c>VariableType</c>), which is the identifier for the parameter.</p>
///     <p><b>2.</b> The second value (<c>NumberSet</c>), which denotes the number set which the parameter must comply
///        with in order for the function to accept it.
///     </p>
/// </summary>
public sealed class FunctionParameter
{
    /// <summary>
    ///     <p>Creates a new <c>FunctionParameter</c> from its equivalent core type.</p>
    /// </summary>
    /// <param name="coreParameter"> the equivalent core type </param>
    /// <returns> a new <c>FunctionParameter</c>, derived from its equivalent core type </returns>
    internal static FunctionParameter OfCoreType(Tuple<Tuple<char, byte>, Core.Syntax.NumberSet> coreParameter)
    {
        var variable  = Variable.OfCoreType(coreParameter.Item1);
        var numberSet = (NumberSet) coreParameter.Item2.Tag; // STABLE AS LONG AS THE ABI IS ALSO STABLE
        return new FunctionParameter(variable, numberSet);
    }

    public Variable  Identifier { get; }
    public NumberSet Set        { get; }

    private FunctionParameter(Variable identifier, NumberSet set) =>
        (Identifier, Set) = (identifier, set);

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