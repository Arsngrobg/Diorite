// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionAttributes.cs
// Summary: The type definition for the FunctionAttributes type, which contains essential data for identifying, and
//          executing the body of the function 
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Diorite.Lang.API.Syntax.Views;
using Diorite.Lang.API.Traits;

namespace Diorite.Lang.API.Syntax.Data;

/// <summary>
///     <p>The <c>FunctionMetadata</c> record type contains type information about a function definition.</p>
///     <p>It also maintains the identifier which this function is bound to, and its <c>FunctionMetadata</c>.</p>
///     <p><i>This is a concrete field, meaning it cannot be modified after the function definition.</i></p>
///     <p><i>This is a <b>Read-Only</b> type, meaning they cannot and should not be instantiated outside of API
///           functions.
///     </i></p>
/// </summary>
public class FunctionAttributes : ICoreConverter<Core.Syntax.FunctionAttributes, FunctionAttributes>
{
    public static FunctionAttributes OfCoreType(Core.Syntax.FunctionAttributes coreAttributes)
    {
        var identifier = Variable.OfCoreType(coreAttributes.identifier);
        var parameters = coreAttributes.parameters
                                       .Select(FunctionParameter.OfCoreType)
                                       .ToArray();
        var range      = (NumberSet) coreAttributes.range.Tag; // STABLE AS LONG AS THE ABI IS ALSO STABLE
        var metadata   = FunctionMetadata.OfCoreType(coreAttributes.metadata);
        return new FunctionAttributes(identifier, parameters, range, metadata);
    }

    public Variable                         Identifier { get; }
    public IReadOnlyList<FunctionParameter> Parameters { get; }
    public NumberSet                        Range      { get; }
    public FunctionMetadata                 Metadata   { get; }

    private FunctionAttributes(Variable identifier, IReadOnlyList<FunctionParameter> parameters, NumberSet range,
        FunctionMetadata metadata) =>
        (Identifier, Parameters, Range, Metadata) = (identifier, parameters, range, metadata);

    /// <summary>
    ///     <p>Checks whether this <c>FunctionAttributes</c> is expected to return a <c>Natural</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionAttributes</c> is expected to return a <c>Natural</c> number. </returns>
    public bool ShouldReturnNatural() =>
        Range is NumberSet.Natural;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionAttributes</c> is expected to return a <c>Integer</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionAttributes</c> is expected to return a <c>Integer</c> number. </returns>
    public bool ShouldBeInteger() =>
        Range is NumberSet.Integer;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionAttributes</c> is expected to return a <c>Real</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionAttributes</c> is expected to return a <c>Real</c> number. </returns>
    public bool ShouldBeReal() =>
        Range is NumberSet.Real;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionAttributes</c> is expected to return a <c>Rational</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionAttributes</c> is expected to return a <c>Rational</c> number. </returns>
    public bool ShouldBeRational() =>
        Range is NumberSet.Rational;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionAttributes</c> is expected to return a <c>Irrational</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionAttributes</c> is expected to return a <c>Irrational</c> number. </returns>
    public bool ShouldBeIrrational() =>
        Range is NumberSet.Irrational;

    /// <summary>
    ///     <p>Checks whether this <c>FunctionAttributes</c> is expected to return a <c>Complex</c> number.</p>
    /// </summary>
    /// <returns> if this <c>FunctionAttributes</c> is expected to return a <c>Complex</c> number. </returns>
    public bool ShouldBeComplex() =>
        Range is NumberSet.Complex;

    public override int GetHashCode() =>
        HashCode.Combine(Identifier, Parameters, Range, Metadata);

    public override bool Equals(object? obj) =>
        obj is FunctionAttributes other &&
        Identifier.Equals(other.Identifier) && Parameters.Equals(other.Parameters) && Range == other.Range &&
        Metadata.Equals(other.Metadata);

    public override string ToString() =>
        $"FunctionAttributes" +
        $"[Identifier: {Identifier}, Parameters: {Parameters.Count}, Range: {Range}, Metadata: {Metadata}]";
}