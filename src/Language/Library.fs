// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Library.fs
// Summary: Standard library of built-in mathematical functions and other helpful utilities
// Author:  Borngle
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>Library</c> module provides built-in functions that can utilized by the user when writing <b>Diorite</b>
///     code.
/// </summary>
[<RequireQualifiedAccess>]
module Library =
    let terms: int = 10 // number of terms in the Taylor series for trigonometric functions
    
    /// <summary>
    ///     Returns the magnitude of <c>x</c>.
    /// </summary>
    /// <param name='x'> the number value </param>
    /// <returns> the magnitude of <c>x</c> </returns>
    let abs (x: int): int =
        if x < 0 then -x
        else x
    
    /// <summary>
    ///     Returns the sign of <c>x</c>.
    /// </summary>
    /// <param name='x'> the number value </param>
    /// <returns> the sign of <c>x</c> </returns>
    let sign (x: float): float =
        if   x < 0.0 then -1.0
        elif x > 0.0 then  1.0
        else 0.0
    
    /// <summary>
    ///     Raises <c>x</c> to the power of <c>y</c>.
    /// </summary>
    /// <param name='x'> the base </param>
    /// <param name='y'> the exponent </param>
    /// <returns> <c>x</c> to the power of <c>y</c> </returns>
    let pow (x: float, y: float): float =
        x ** y
    
    /// <summary>
    ///     Returns the greatest number less-than or equal-to <c>x</c>.
    /// </summary>
    /// <param name='x'> the number value </param>
    /// <returns> the greatest number less-than or equal-to <c>x</c> </returns>
    let floor (x: float) : float =
        let y = float (int x) // truncates
        if x < 0.0 && x <> y then y - 1.0 // if x is negative and not an integer
        else y
    
    /// <summary>
    ///     Returns the lowest number greater-than or equal-to <c>x</c>.
    /// </summary>
    /// <param name='x'> the number value </param>
    /// <returns> the lowest number greater-than or equal-to <c>x</c> </returns>
    let ceil (x: float): float =
        let y = float (int x) // truncates
        if x > 0.0 && x <> y then y + 1.0 // if x is positive and not an integer
        else y
        
    /// <summary>
    ///     Converts <c>r</c> (which is in radians) to degrees.
    /// </summary>
    /// <param name='r'> the radian value </param>
    /// <returns> <c>r</c> in degrees </returns>
    let deg (r: float): float =
        r * (180.0 / System.Math.PI)
        
    /// <summary>
    ///     Converts <c>d</c> (which is in degrees) to radians.
    /// </summary>
    /// <param name='d'> the degree value </param>
    /// <returns> <c>d</c> in radians </returns>
    let rad (d: float): float =
        d * (System.Math.PI / 180.0)
        
    /// <summary>
    ///     Returns the greatest common denominator of <c>a</c> and <c>b</c>.
    /// </summary>
    /// <param name='a'> the first number </param>
    /// <param name='b'> the second number </param>
    /// <returns> the greatest common denominator of <c>a</c> and <c>b</c> </returns>
    let rec gcd (a: int, b: int): int =
        if b = 0 then abs(a)
        else gcd(b, a % b)
        
    /// <summary>
    ///     Returns the lowest common multiple of <c>a</c> and <c>b</c>.
    /// </summary>
    /// <param name='a'> the first number </param>
    /// <param name='b'> the second number </param>
    /// <returns> the lowest common multiple of <c>a</c> and <c>b</c> </returns>
    let lcm (a: int, b: int): int =
        abs(a * b) / gcd(a, b)
        
    /// <summary>
    ///     Approximates the sine of <c>x</c> using the Taylor series.
    /// </summary>
    /// <param name='x'> the radian value </param>
    /// <returns> the approximate value of <c>sin(x)</c> </returns>
    let sin (x: float): float =
        let rec loop n term sum = // n is term index, term is current term, and sum is the cumulative sum of all terms
            if n >= terms then sum // checks if number of terms computed exceeds number of terms in the series
            else
                let nextTerm = term * -x * x / (float (2 * n * (2 * n + 1))) // * -1.0 alternates sign
                loop (n + 1) nextTerm (sum + nextTerm)
        loop 1 x x // first term and sum is initialised as x

    /// <summary>
    ///     Approximates the cosine of <c>x</c> using the Taylor series.
    /// </summary>
    /// <param name='x'> the radian value </param>
    /// <returns> the approximate value of <c>cos(x)</c> </returns>
    let cos (x: float): float =
        let rec loop n term sum =
            if n >= terms then sum
            else
                let nextTerm = term * -x * x / (float (2 * n * (2 * n - 1)))
                loop (n + 1) nextTerm (sum + nextTerm)
        loop 1 1.0 1.0 // first term in cosine and sum is 1.0
      
    /// <summary>
    ///     Approximates the tangent of <c>x</c> using the quotient identity.
    /// </summary>
    /// <param name='x'> the radian value </param>
    /// <returns> the approximate value of <c>tan(x)</c> </returns>
    let tan (x: float) : float =
        sin x / cos x
    
    /// <summary>
    ///     Approximates the inverse sine of <c>x</c> using the Taylor series.
    /// </summary>
    /// <param name='x'> the radian value </param>
    /// <returns> the approximate value of <c>asin(x)</c> </returns>
    let asin (x: float) : float =
        let rec loop n coefficient power sum = // factorial-esque coefficient for current term and current power of x
            if n >= terms then sum
            else
                let coefficient' = coefficient * float(2 * n - 1) / float(2 * n)
                let power' = power * x * x // next odd power of x
                loop (n + 1) coefficient' power' (sum + coefficient' * power' * x / float (2 * n + 1))
        loop 1 1.0 x x // power starts as x^1
        
    /// <summary>
    ///     Approximates the inverse cosine of <c>x</c> using the complementary angle identity.
    /// </summary>
    /// <param name='x'> the radian value </param>
    /// <returns> the approximate value of <c>acos(x)</c> </returns>
    let acos (x: float): float =
        System.Math.PI / 2.0 - asin x
    
    
    /// <summary>
    ///     Approximates the inverse tangent of <c>x</c> using the Taylor series.
    /// </summary>
    /// <param name='x'> the radian value </param>
    /// <returns> the approximate value of <c>atan(x)</c> </returns>
    let atan (x: float) : float =
        let rec loop n term sum =
            if n >= terms then sum
            else
                let nextTerm = term * -x * x / (float (2 * n + 1))
                loop (n + 1) nextTerm (sum + nextTerm)
        loop 0 x x