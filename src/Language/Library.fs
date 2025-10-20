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
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>Maths</c> module provides a built-in set of commonly used mathematical functions
/// </summary>
[<RequireQualifiedAccess>]
module Maths =
    let terms: int = 10 // number of terms in the Taylor series for trigonometric functions
    
    /// <summary>
    /// Returns the magnitude of a number value
    /// </summary>
    /// <param name="x"> number value </param>
    let abs (x : int): int =
        if x < 0 then -x
        else x
    
    /// <summary>
    /// Returns the sign of a number value
    /// </summary>
    /// <param name="x"> number value </param>
    let sign (x : float): float =
        if x < 0.0 then -1.0
        elif x > 0.0 then 1.0
        else 0.0
    
    /// <summary>
    /// Computes the result of a power operation
    /// </summary>
    /// <param name="x"> base value </param>
    /// <param name="y"> power value </param>
    let pow (x : float, y : float): float =
        x ** y
    
    /// <summary>
    /// Returns the greatest float less-than or equal-to a number value
    /// </summary>
    /// <param name="x"> number value </param>
    let floor (x: float) : float =
        let y = float (int x) // truncates
        if x < 0.0 && x <> y then y - 1.0 // if x is negative and not an integer
        else y
    
    /// <summary>
    /// Returns the lowest float greater-than or equal-to a number value
    /// </summary>
    /// <param name="x"> number value </param>
    let ceil (x: float) : float =
        let y = float (int x) // truncates
        if x > 0.0 && x <> y then y + 1.0 // if x is positive and not an integer
        else y
        
    /// <summary>
    /// Converts radians to degrees
    /// </summary>
    /// <param name="r"> radian value </param>
    let deg (r: float) : float =
        r * (180.0 / System.Math.PI)
        
    /// <summary>
    /// Converts degrees to radians
    /// </summary>
    /// <param name="d"> degree value </param>
    let rad (d: float) : float =
        d * (System.Math.PI / 180.0)
        
    /// <summary>
    /// Returns the greatest common denominator of two values
    /// </summary>
    /// <param name="a"> number value </param>
    /// <param name="b"> number value </param>
    let rec gcd (a: int, b : int) : int =
        if b = 0 then abs(a)
        else gcd(b, a % b)
        
    /// <summary>
    /// Returns the lowest common multiple of two values
    /// </summary>
    /// <param name="a"> number value </param>
    /// <param name="b"> number value </param>
    let lcm (a: int, b: int) : int =
        abs(a * b) / gcd(a, b)
        
    /// <summary>
    /// Approximates the sin of a value using the Taylor series
    /// </summary>
    /// <param name="x"> radian value </param>
    let sin (x: float) : float =
        let rec loop n term sum = // n is term index, term is current term, and sum is the cumulative sum of all terms
            if n >= terms then sum // checks if number of terms computed exceeds number of terms in the series
            else
                let nextTerm = term * -x * x / (float (2 * n * (2 * n + 1))) // * -1.0 alternates sign
                loop (n + 1) nextTerm (sum + nextTerm)
        loop 1 x x // first term and sum is initialised as x

    /// <summary>
    /// Approximates the cosine of a value using the Taylor series
    /// </summary>
    /// <param name="x"> radian value </param>
    let cos (x: float): float =
        let rec loop n term sum =
            if n >= terms then sum
            else
                let nextTerm = term * -x * x / (float (2 * n * (2 * n - 1)))
                loop (n + 1) nextTerm (sum + nextTerm)
        loop 1 1.0 1.0 // first term in cosine and sum is 1.0
      
    /// <summary>
    /// Approximates the tangent of a value using the quotient identity
    /// </summary>
    /// <param name="x"> radian value </param>
    let tan (x: float) : float =
        sin x / cos x
    
    /// <summary>
    /// Approximates the inverse sine of a value using the Taylor series
    /// </summary>
    /// <param name="x"> radian value </param>
    let asin (x: float) : float =
        let rec loop n coefficient power sum = // factorial-esque coefficient for current term and current power of x
            if n >= terms then sum
            else
                let coefficient' = coefficient * float(2 * n - 1) / float(2 * n)
                let power' = power * x * x // next odd power of x
                loop (n + 1) coefficient' power' (sum + coefficient' * power' * x / float (2 * n + 1))
        loop 1 1.0 x x // power starts as x^1
        
    /// <summary>
    /// Approximates the inverse cosine of a value using the complementary angle identity
    /// </summary>
    /// <param name="x"></param>
    let acos (x: float) : float =
        System.Math.PI / 2.0 - asin x
    
    
    /// <summary>
    /// Approximates the inverse tangent of a value using the Taylor series
    /// </summary>
    /// <param name="x"> radian value </param>
    let atan (x: float) : float =
        let rec loop n term sum =
            if n >= terms then sum
            else
                let nextTerm = term * -x * x / (float (2 * n + 1))
                loop (n + 1) nextTerm (sum + nextTerm)
        loop 0 x x