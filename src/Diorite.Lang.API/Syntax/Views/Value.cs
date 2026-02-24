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
public abstract class Value : ICoreView<Core.Syntax.ValueType>, ICoreConverter<Core.Syntax.ValueType, Value>
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
    ///     <p>A constant value for the <c>Number</c> core type.</p>
    ///     <p>It is a <c>Number</c> carrying a value of <c>double.Pi</c>.</p>
    /// </summary>
    public static readonly Value Pi        = new NumberValue(double.Pi);
    /// <summary>
    ///     <p>A constant value for the <c>Number</c> core type.</p>
    ///     <p>It is a <c>Number</c> carrying a value of <c>double.Tau</c>.</p>
    /// </summary>
    public static readonly Value Tau       = new NumberValue(double.Tau);
    /// <summary>
    ///     <p>A constant value for the <c>Number</c> core type.</p>
    ///     <p>It is a <c>Number</c> carrying a value of <c>double.E</c>.</p>
    /// </summary>
    public static readonly Value E         = new NumberValue(double.E);

    /// <summary>
    ///     <p>A constant value for the <c>Complex</c> core type.</p>
    ///     <p>It is a <c>Complex</c> value carrying a value of the imaginary unit (<c>i</c>).</p>
    /// </summary>
    public static readonly Value ImUnit    = new ComplexValue(0, 1);

    /// <summary>
    ///     <p>Whether <c>NumberValue</c> produces a fractional representation over a decimal representation, wherever
    ///        possible.
    ///     </p>
    ///     <p><i>Default: <c>false</c></i></p>
    /// </summary>
    public static bool FractionalRepresentation { get; set; } = false;

    private const string DecimalFormat      = "G10"; // for string representations
    private const double FractionResolution = 1e-10; // for determining fractional representation
    private const int    FractionIterations = 1000;  // for capping the number of iterations of the CF

    /// <summary>
    ///     <p>Helper method to check whether the supplied <c>Value</c> is a <c>ComplexValue</c>.</p>
    /// </summary>
    /// <param name="value"> the <c>Value</c> to validate </param>
    /// <returns> <c>true</c> if <c>ComplexType</c>, <c>false</c> otherwise </returns>
    public static bool IsComplex(Value value) =>
        value is ComplexValue;

    /// <summary>
    ///     <p>Helper method to check whether the supplied <c>Value</c> is a <c>NumberType</c>.</p>
    ///     <p>This also includes <c>ComplexValue</c>s with no imaginary component.</p>
    /// </summary>
    /// <param name="value"> the <c>Value</c> to validate </param>
    /// <returns> <c>true</c> if <c>NumberType</c>, <c>false</c> otherwise </returns>
    public static bool IsNumber(Value value) =>
        value is NumberValue || (value is ComplexValue && value.Im().Equals(0));

    /// <summary>
    ///     <p>Creates a <c>Value</c> that stores a numerical value.</p>
    ///     <p>It is not possible for a <c>DioriteValue.NumberValue</c> to store a value of <c>NaN</c> is it is required
    ///        for <c>PiecewiseCondition</c> evaluation.
    ///     </p>
    /// </summary>
    /// <param name="value"> the raw 64-bit decimal value </param>
    /// <returns> a <c>Value</c>, if the supplied <c>value</c> is not <c>NaN</c> </returns>
    /// <exception cref="ArgumentException"> if <c>value</c> is <c>NaN</c> </exception>
    public static Value OfNumber(double value)
    {
        if (double.IsPositiveInfinity(value))
            return PInfinity;
        if (double.IsNegativeInfinity(value))
            return NInfinity;
        
        return value switch
        {
            double.Pi  => Pi,
            double.Tau => Tau,
            double.E   => E,
            _          =>
                double.IsNaN(value)
                ? throw new ArgumentException(
                    "Cannot represent double.NaN using DioriteValue.NumberValue",
                    nameof(value)
                )
                : new NumberValue(value)
        };
    }

    /// <summary>
    ///     <p>Creates a <c>Value</c> that stores a complex value.</p>
    ///     <p>A complex number of the form <c>a + bi</c>.</p>
    ///     <p>Where <c>a</c> &amp; <c>b</c> are <c>Real</c> numbers.</p>
    /// </summary>
    /// <param name="real"> the real component of the complex number </param>
    /// <param name="imaginary"> the imaginary component of the complex number </param>
    /// <returns> a <c>Value</c> that stores a complex value </returns>
    public static Value OfComplex(double real, double imaginary)
    {
        if (double.IsNaN(real) || double.IsNaN(imaginary))
            throw new ArgumentException("Cannot represent double.NaN using DioriteValue.ComplexValue");
        
        if (real.Equals(0) && imaginary.Equals(1))
            return ImUnit;

        return new ComplexValue(real, imaginary);
    }

    /// <summary>
    ///     <p>Creates a new <c>Value</c> from its equivalent core type.</p>
    /// </summary>
    /// <param name="coreValueType"> the equivalent core type </param>
    /// <returns> a new <c>Value</c>, derived from its equivalent core type </returns>
    public static Value OfCoreType(Core.Syntax.ValueType coreValueType) => coreValueType switch
    {
        Core.Syntax.ValueType.Float   x => OfNumber (x.Item),
        Core.Syntax.ValueType.Integer x => OfNumber (x.Item),
        Core.Syntax.ValueType.Complex z => OfComplex(z.Item1, z.Item2),
        _                               => Undefined
    };

    private Value() {}

    /// <summary>
    ///     <p>Extracts the real component from this <c>Value</c>.</p>
    /// </summary>
    /// <returns> the real component of this <c>Value</c> </returns>
    /// <exception cref="ArithmeticException"> if the <c>Value</c> is <c>UndefinedValue</c> </exception>
    public abstract double Re();
    /// <summary>
    ///     <p>Extracts the imaginary component from this <c>Value</c>.</p>
    /// </summary>
    /// <returns> the imaginary component of this <c>Value</c> </returns>
    /// <exception cref="ArithmeticException"> if the <c>Value</c> is <c>UndefinedValue</c> </exception>
    public abstract double Im();

    public abstract Core.Syntax.ValueType AsCoreType();

    public abstract override int GetHashCode();

    public abstract override bool Equals(object? obj);
    
    public abstract override string ToString();

    /// <summary>
    ///     <p>A 64-bit, floating-point decimal.</p>
    /// </summary>
    private sealed class NumberValue : Value
    {
        /// <summary>
        ///     <p>The value stored by this <c>NumberValue</c>.</p>
        /// </summary>
        private readonly double _value;

        internal NumberValue(double value) =>
            _value = value;
        
        /// <summary>
        ///     <p>Returns</p>
        /// </summary>
        /// <returns></returns>
        // https://en.wikipedia.org/wiki/Simple_continued_fraction
        private string AsFractionString()
        {
            var unsigned = Math.Abs(Re());
            
            // a(0) = floor(x);
            // r(0) = x - floor(x);
            // h(n) = a(n) * h(n-1) + h(n-2)
            // k(n) = a(n) * k(n-1) + k(n-2)
            
            // for h(0):
            //     h(0) = a(0)
            // ... h(0) = a(0) * h(n-1) + h(n-2)
            // ... h(0) = a(0) * ( 1  ) + ( 0  )
            // ... h(-1) = 1 & h(-2) = 0

            // for k(0):
            //     k(0) = 1
            // ... k(0) = a(0) * k(n-1) + k(n-2)
            // ... k(0) = a(0) * ( 0  ) + ( 1  )
            // ... k(-1) = 0 & k(-2) = 0

            var a = Math.Floor(unsigned);
            var r = unsigned - a;

            var h1 = a;
            var k1 = 1.0;
            var h2 = 1.0;
            var k2 = 0.0;

            var i = 1;
            while (r >= FractionResolution && i < FractionIterations)
            {
                // handle error buildup
                var reciprocal = 1 / r;
                var closest    = Math.Round(reciprocal);
                a = Math.Abs(reciprocal - closest) < FractionResolution
                    ? closest
                    : Math.Floor(reciprocal);

                r = reciprocal - a;

                var hn = a * h1 + h2;
                var kn = a * k1 + k2;
                h2 = h1;
                k2 = k1;
                h1 = hn;
                k1 = kn;

                i++;
            }

            if (i == FractionIterations || k1.Equals(1))
                return $"{Re().ToString(DecimalFormat)}";

            return Re() < 0
                   ? $"-{h1}/{k1}"
                   : $"{h1}/{k1}";
        }

        public override double Re() =>
            _value;

        public override double Im() =>
            0;

        public override Core.Syntax.ValueType AsCoreType() =>
            Core.Syntax.ValueType.NewFloat(Re());

        public override int GetHashCode() =>
            HashCode.Combine(Re(), Im()); // silent upcast to complex (value + 0i) to maintain safe hash

        public override bool Equals(object? obj) => obj switch
        {
            ComplexValue z => Re().Equals(z.Re()) && z.Im().Equals(0),
            NumberValue  x => Re().Equals(x.Re()),
            _              => false
        };

        public override string ToString()
        {
            if (double.IsPositiveInfinity(Re()))
                return "inf";
            if (double.IsNegativeInfinity(Re()))
                return "-inf";
            
            return FractionalRepresentation
                   ? AsFractionString()
                   : Re().ToString(DecimalFormat);
        }
    }

    /// <summary>
    ///     <p>A complex number of the form <c>a + bi</c>.</p>
    /// </summary>
    private sealed class ComplexValue : Value
    {
        /// <summary>
        ///     <p>The real component of this <c>ComplexValue</c>.</p>
        /// </summary>
        private readonly double _realComponent;

        /// <summary>
        ///     <p>The imaginary component of this <c>ComplexValue</c>.</p>
        /// </summary>
        private readonly double _imaginaryComponent;

        internal ComplexValue(double real, double imaginary) =>
            (_realComponent, _imaginaryComponent) = (real, imaginary);

        public override double Re() =>
            _realComponent;

        public override double Im() =>
            _imaginaryComponent;

        public override Core.Syntax.ValueType AsCoreType() =>
            Core.Syntax.ValueType.NewComplex(Re(), Im());

        public override int GetHashCode() =>
            HashCode.Combine(Re(), Im());

        public override bool Equals(object? obj) => obj switch
        {
            ComplexValue z => Re().Equals(z.Re())
                              && Im().Equals(z.Im()),
            NumberValue  x => Re().Equals(x.Re())
                              && Im().Equals(0),
            _              => false
        };

        public override string ToString()
        {
            var stringBuilder = new System.Text.StringBuilder();

            var shouldAddPlus = Re() != 0;
            if (Re() != 0 || Im() == 0)
                stringBuilder.Append(Re().ToString(DecimalFormat));

            switch (Im())
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
                    stringBuilder.Append(Im().ToString(DecimalFormat))
                                 .Append('i');
                    break;
            }

            return stringBuilder.ToString();
        }
    }
    /// <summary>
    ///     <p>An undetermined value - singleton.</p>
    /// </summary>
    private sealed class UndefinedValue : Value
    {
        public override double Re() =>
            throw new ArithmeticException("Undefined has no stateful value.");

        public override double Im() =>
            throw new ArithmeticException("Undefined has no stateful value.");

        public override Core.Syntax.ValueType AsCoreType() =>
            Core.Syntax.ValueType.Undefined;

        public override int GetHashCode() =>
            0;

        public override bool Equals(object? obj) =>
            obj is UndefinedValue;

        public override string ToString() =>
            "undefined";
    }
}
