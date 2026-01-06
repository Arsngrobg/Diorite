// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Value.cs
// Summary: The definition for the Value, which is the fundamental (atomic) value in Diorite
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Diorite.Lang.API.Traits;

namespace Diorite.Lang.API.Syntax.Views;

/// <summary>
///     <p>The union type which describe the cases in which a <c>Value</c> is represented as in <b>Diorite</b>.</p>
///     <p><b>1.</b> <c>Number</c>: a 64-bit, floating-point decimal.</p>
///     <p><b>2.</b> <c>Complex</c>: a pair of real values, <c>a + bi</c>, where <c>a</c> &amp; <c>b</c> are
///        <c>Real</c> numbers.
///     </p>
///     <p><b>3.</b> <c>Undefined</c>: denoting that a <b>variable</b> or operation is not properly defined.</p>
/// </summary>
public abstract class Value : ICoreView<Core.Syntax.ValueType>
{
    /// <summary>
    ///     <p>The constant mapping of the <c>Undefined</c> core type.</p>
    ///     <p>It can be compared to <c>null</c>. However, rather than the <i>"absence"</i> of a value it declares that
    ///        a result or value is not defined.
    ///     </p>
    /// </summary>
    public static readonly Value Undefined = new UndefinedValue();

    /// <summary>
    ///     <p>A constant value for the <c>Number</c> core type.</p>
    ///     <p>It is a <c>Number</c> carrying a value of <c>double.PositiveInfinity</c>.</p>
    /// </summary>
    public static readonly Value PInfinity = new NumberValue(double.PositiveInfinity);
    /// <summary>
    ///     <p>A constant value for the <c>Number</c> core type.</p>
    ///     <p>It is a <c>Number</c> carrying a value of <c>double.NegativeInfinity</c>.</p>
    /// </summary>
    public static readonly Value NInfinity = new NumberValue(double.NegativeInfinity);

    /// <summary>
    ///     <p>A constant value for the <c>Complex</c> core type.</p>
    ///     <p>It is a <c>Complex</c> value carrying a value of the imaginary unit (<c>i</c>).</p>
    /// </summary>
    public static readonly Value ImUnit    = new ComplexValue(0, 1);

    /// <summary>
    ///     <p>Creates a <c>DioriteValue</c> that stores a numerical value.</p>
    ///     <p>It is not possible for a <c>DioriteValue.NumberValue</c> to store a value of <c>NaN</c> is it is required
    ///        for <c>PiecewiseCondition</c> evaluation.
    ///     </p>
    /// </summary>
    /// <param name="value"> the raw 64-bit decimal value </param>
    /// <returns> a <c>DioriteValue</c>, if the supplied <c>value</c> is not <c>NaN</c> </returns>
    /// <exception cref="ArgumentException"> if <c>value</c> is <c>NaN</c> </exception>
    public static Value OfNumber(double value)
    {
        if (double.IsPositiveInfinity(value))
            return PInfinity;
        if (double.IsNegativeInfinity(value))
            return NInfinity;

        return double.IsNaN(value)
             ? throw new ArgumentException("Cannot represent double.NaN using DioriteValue.NumberValue", nameof(value))
             : new NumberValue(value);
    }

    /// <summary>
    ///     <p>Creates a <c>DioriteValue</c> that stores a complex value.</p>
    ///     <p>A complex number of the form <c>a + bi</c>.</p>
    ///     <p>Where <c>a</c> &amp; <c>b</c> are <c>Real</c> numbers.</p>
    /// </summary>
    /// <param name="real"> the real component of the complex number </param>
    /// <param name="imaginary"> the imaginary component of the complex number </param>
    /// <returns> a <c>DioriteValue</c> that stores a complex value </returns>
    public static Value OfComplex(double real, double imaginary)
    {
        if (double.IsNaN(real) || double.IsNaN(imaginary))
            throw new ArgumentException("Cannot represent double.NaN using DioriteValue.ComplexValue");
        
        if (real.Equals(0) && imaginary.Equals(1))
            return ImUnit;

        return new ComplexValue(real, imaginary);
    }

    // for string representations
    private const string DecimalFormat = "G8";

    // A 64-bit, floating-point decimal
    private sealed class NumberValue : Value
    {
        public double Value { get; }

        internal NumberValue(double value) =>
            Value = value;

        public override Core.Syntax.ValueType AsCoreType() =>
            Core.Syntax.ValueType.NewNumber(Value);

        public override int GetHashCode() =>
            HashCode.Combine(Value, 0); // silent upcast to complex (value + 0i) to maintain safe hash

        public override bool Equals(object? obj) => obj switch
        {
            ComplexValue z => Value.Equals(z.RealComponent) && z.ImaginaryComponent.Equals(0),
            NumberValue  x => Value.Equals(x.Value),
            _              => false
        };

        public override string ToString()
        {
            if (double.IsPositiveInfinity(Value))
                return "inf";
            return (double.IsNegativeInfinity(Value))
                 ? "-inf"
                 : Value.ToString(DecimalFormat);
        }
    }

    // A complex number of the form a + bi
    private sealed class ComplexValue : Value
    {
        public double RealComponent      { get; }
        public double ImaginaryComponent { get; }

        internal ComplexValue(double real, double imaginary) =>
            (RealComponent, ImaginaryComponent) = (real, imaginary);

        public override Core.Syntax.ValueType AsCoreType() =>
            Core.Syntax.ValueType.NewComplex(RealComponent, ImaginaryComponent);

        public override int GetHashCode() =>
            HashCode.Combine(RealComponent, ImaginaryComponent);

        public override bool Equals(object? obj) => obj switch
        {
            ComplexValue z => RealComponent.Equals(z.RealComponent)
                              && ImaginaryComponent.Equals(z.ImaginaryComponent),
            NumberValue  x => RealComponent.Equals(x.Value)
                              && ImaginaryComponent.Equals(0),
            _              => false
        };

        public override string ToString()
        {
            var stringBuilder = new System.Text.StringBuilder();

            var shouldAddPlus = RealComponent != 0;
            if (RealComponent != 0 || ImaginaryComponent == 0)
                stringBuilder.Append(RealComponent.ToString(DecimalFormat));

            switch (ImaginaryComponent)
            {
                case  1:
                    if (shouldAddPlus) stringBuilder.Append(" + ");
                    stringBuilder.Append('i');
                    break;
                case -1:
                    if (shouldAddPlus) stringBuilder.Append(" - ");
                    stringBuilder.Append('i');
                    break;
                case  0:
                    break;
                default:
                    if (shouldAddPlus) stringBuilder.Append(" + ");
                    stringBuilder.Append(ImaginaryComponent.ToString(DecimalFormat))
                                 .Append('i');
                    break;
            }

            return stringBuilder.ToString();
        }
    }

    // An undetermined value - singleton
    private sealed class UndefinedValue : Value
    {
        public override Core.Syntax.ValueType AsCoreType() =>
            Core.Syntax.ValueType.Undefined;

        public override int GetHashCode() =>
            0;

        public override bool Equals(object? obj) =>
            obj is UndefinedValue;

        public override string ToString() =>
            "undefined";
    }

    private Value() {}

    public abstract Core.Syntax.ValueType AsCoreType();
}
