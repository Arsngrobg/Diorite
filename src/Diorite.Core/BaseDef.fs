// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    BaseDef.fs
// Summary: Definitions for the core types in the Diorite mathematics language
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The <c>BaseTypes</c> module contains bindings for the common types and their public-facing API functions.</p>
/// </summary>
// marked as AutoOpen as used across the entire project
[<AutoOpen>]
module BaseDef =
    /// <summary>
    ///     <p>The <c>IO</c> type is inspired by the IO functor found in <b>Haskell</b>.</p>
    ///     <p>In simple terms, the <c>IO</c> type wraps a computation which, when executed, may produce or perform side
    ///        effects and then produce a result of type <c>'a</c>.
    ///     </p>
    ///     <p>It is a <b>pure</b> container for an otherwise <b>impure</b> computation.</p>
    /// </summary>
    /// <typeparam name="'a"> the type of the result returned by the suspended computation </typeparam>
    type IO<'a> = IO of (unit -> 'a)

    /// <summary>
    ///     <p>The IO module defines the helper functions for executing, binding, mapping, and uplifting values to
    ///        <c>IO</c> monadic types.
    ///     </p>
    ///     <p>For example, the <c>IO.run</c> helper function executes the inner function bound by the supplied
    ///        <c>IO</c> value, and returns the inner value of type <c>'a</c>.
    ///     </p>
    /// </summary>
    module IO =
        /// <summary>
        ///     <p>Runs the suspended computation within the supplied <c>IO</c> value.</p>
        /// </summary>
        /// <typeparam name='IO (fn)'> the <c>IO</c> type and its inner function <c>fn</c> </typeparam>
        /// <returns> the result of the inner function <c>fn</c> </returns>
        let run (IO (fn: unit -> 'a)): 'a =
            fn ()

        /// <summary>
        ///     <p>Maps the supplied <c>IO</c> functor into a new <c>IO</c> functor that extends the original delayed
        ///        computation. The supplied function is described as the 'mapping' function which applies some
        ///        operation on the <c>'a</c> type to the type <c>'b</c>.
        ///     </p>
        /// </summary>
        /// <param name='io'> the original <c>IO</c> functor </param>
        /// <param name='fn'> the extended computation to perform on the return result of the <c>IO</c> </param>
        /// <typeparam name="'a"> the type bound of the <c>IO</c> functor </typeparam>
        /// <typeparam name="'b"> the new type of the extended <c>IO</c> functor </typeparam>
        /// <returns> a new <c>IO</c> functor that executes an operation on the original <c>IO</c> </returns>
        let map (io: IO<'a>) (fn: 'a -> 'b): IO<'b> =
            let ioFn: unit -> 'a = match io with IO ioFn -> ioFn
            IO (fun () ->
                let a: 'a = ioFn ()
                let b: 'b = fn a
                b
            )

        /// <summary>
        ///     <p>Sequences two <c>IO</c> computations.</p>
        ///     <p>This is equivalent to the <c>flatmap</c> operation.</p>
        ///     <p>The supplied function is described as the 'uplifting' function which applies some transformation on
        ///        the type <c>'a</c> into another <c>IO</c> functor bound to the type <c>'b</c>.
        ///     </p>
        /// </summary>
        /// <param name='io'> the first <c>IO</c> functor </param>
        /// <param name='fn'> the 'uplifting' function to transform into <c>'b IO</c> </param>
        /// <typeparam name="'a"> the type bound of the original <c>IO</c> functor </typeparam>
        /// <typeparam name="'b"> the type bound of the second <c>IO</c> functor </typeparam>
        /// <returns> a new <c>IO</c> functor that executes the <c>'a IO</c> functor as well </returns>
        let bind (io: IO<'a>) (fn: 'a -> IO<'b>): IO<'b> =
            let ioFn: unit -> 'a = match io with IO ioFn -> ioFn
            IO (fun () ->
                let a:   'a     = ioFn()
                let ioB: IO<'b> = fn a
                let b:   'b     = run ioB
                b
            )

    /// <summary>
    ///     <p>The structured representation of a <c>Variable</c> in the <b>Diorite</b> mathematics language.</p>
    ///     <p><b>1.</b> The first value (<c>char</c>) is the character which is the variable name (e.g. 'x').</p>
    ///     <p><b>2.</b> The second value (<c>int</c>) is the encoded subscript of the variable - this value is
    ///        optional, where a subscript of <c>0</c> internally represents the plain character (e.g. <c>'x'</c>) and
    ///        <c>10</c> internally represents the subscript-ed variable <c>"x9"</c>, which is the maximum amount of
    ///        subscript-ed permutations of the character.
    ///     </p>
    ///     <p>For all characters of the alphabet (including lowercase &amp; uppercase), each with 11 unique
    ///        permutations, that means <b>Diorite</b> supports a total of <c>572</c> variables.
    ///     </p>
    /// </summary>
    type VariableType = char * int

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>VariableType</c>.</p>
    ///     <p>It returns the string representation of all valid variables with subscripts between <c>0</c> and
    ///        <c>10</c>, where the <c>10</c>th subscript is the <c>9</c>th subscript-ed .
    ///     </p>
    /// </summary>
    /// <param name='variable'> the <c>Variable</c> to derive the <c>string</c> representation </param>
    /// <returns> the <c>string</c> representation of this <c>VariableType</c> </returns>
    let strVariable (variable: VariableType): string =
        let (character: char), (subscript: int) = variable
        if subscript < 0  then (invalidArg "subscript") "encoded subscript value cannot be negative"
        if subscript > 10 then (invalidArg "subscript") "encoded subscript value cannot be greater than 10"
        if subscript = 0  then $"{character}" else $"{character}{subscript - 1}"
