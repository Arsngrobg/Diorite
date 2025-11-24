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
// Version: v1.4
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

/// <summary>
///     <p>The <c>BaseTypes</c> module contains bindings for common definitions across the <b>Diorite</b> project.</p>
///     <p>All types and functions defined here are said to be common utilities or required uniformly across the entire
///        project.
///     </p>
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
        ///     <p>An <c>IO</c> functor that does nothing.</p>
        /// </summary>
        let nil: IO<unit> = IO id

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
        ///     <p>Sequences two <c>IO</c> functors.</p>
        ///     <p>It produces a new <c>IO</c> functor that executes both operations, where the return value of the
        ///        first <c>IO</c> functor is ignored and the return value of the second <c>IO</c> functor is returned.
        ///     </p>
        /// </summary>
        /// <param name='ioA'> the first <c>IO</c> functor </param>
        /// <param name='ioB'> the second <c>IO</c> functor </param>
        /// <typeparam name="'a"> the return type of the first <c>IO</c> functor </typeparam>
        /// <typeparam name="'b"> the return type of the second <c>IO</c> functor </typeparam>
        /// <returns> the composite of the two <c>IO</c> functors into a single <c>IO</c> functor </returns>
        let seq (ioA: IO<'a>) (ioB: IO<'b>): IO<'b> =
            let ioAFn: unit -> 'a = match ioA with IO ioFn -> ioFn
            let ioBFn: unit -> 'b = match ioB with IO ioFn -> ioFn
            IO (fun () ->
                ioAFn () |> ignore
                ioBFn ()
            )

    /// <summary>
    ///     <p>The structured representation of a <c>Variable</c> in the <b>Diorite</b> mathematics language.</p>
    ///     <p><b>1.</b> The first value (<c>char</c>) is the character which is the variable name (e.g. 'x').</p>
    ///     <p><b>2.</b> The second value (<c>int</c>) is the encoded subscript of the variable - this value is
    ///        optional, where a subscript of <c>0</c> internally represents the plain character (e.g. <c>'x'</c>) and
    ///        <c>10</c> internally represents the subscript-ed variable <c>"x9"</c>, which is the maximum amount of
    ///        subscript-ed permutations of the character.
    ///        <i>The encoded subscript is declared as an unsigned 8-bit integer.</i>
    ///     </p>
    ///     <p>For all characters of the alphabet (including lowercase &amp; uppercase), each with 11 unique
    ///        permutations, that means <b>Diorite</b> supports a total of <c>572</c> variables.
    ///     </p>
    /// </summary>
    type VariableType = char * uint8

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>VariableType</c>.</p>
    ///     <p>It returns the string representation of all valid variables with subscripts between <c>0</c> and
    ///        <c>10</c>, where the <c>10</c>th subscript is the <c>9</c>th subscript-ed .
    ///     </p>
    /// </summary>
    /// <param name='variable'> the <c>Variable</c> to derive the <c>string</c> representation </param>
    /// <returns> the <c>string</c> representation of this <c>VariableType</c> </returns>
    let strVariable (variable: VariableType): string =
        let (character: char), (subscript: uint8) = variable
        if subscript > 10uy then (invalidArg "subscript") "encoded subscript value cannot be greater than 10"
        if subscript = 0uy  then $"{character}" else $"{character}{subscript - 1uy}"

    /// <summary>
    ///     <p>The union type which describe the cases in which a <c>Value</c> is represented as in <b>Diorite</b>.</p>
    ///     <p>This union type captures the different ways a <c>Value</c> may be expressed, ranging from concrete
    ///        numeric data to conceptual placeholders such as <c>Infinity</c> or the absence of any value
    ///        (<c>Undefined</c>).
    ///     </p> 
    /// </summary>
    type ValueType =
        | Number    of float
        | Infinity
        | Undefined

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>ValueType</c>.</p>
    /// </summary>
    /// <param name='value'> the <c>ValueType</c> to get the <c>string</c> representation </param>
    /// <returns> the <c>string</c> representation of the supplied <c>ValueType</c> </returns>
    let strValue (value: ValueType): string =
        match value with
         | Number    value -> $"{value}"
         | Undefined       ->  "undefined"
         | Infinity        ->  "infinity"

    /// <summary>
    ///     <p>A <c>FunctionName</c> is a value denoting the name of a function.</p>
    ///     <p>It is either denoted by a <c>Variable</c> or a <c>Symbolic</c> representation.</p>
    /// </summary>
    type FunctionReference =
        | OfVariable of VariableType
        | OfSymbolic of string

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>FunctionReference</c>.</p>
    /// </summary>
    /// <param name='functionReference'> the <c>FunctionReference</c> to get the <c>string</c> representation </param>
    let strFunctionReference (functionReference: FunctionReference): string =
        match functionReference with
         | OfVariable var -> strVariable var
         | OfSymbolic sym -> sym

    /// <summary>
    ///     <p>The number sets supported in the <b>Diorite</b> language.</p>
    ///     <p>These sets define the domain of a function.</p>
    /// </summary>
    type NumberSet =
        | Natural    // N = {0, ..., ∞}
        | Integer    // Z = {-∞, ..., 0, ..., ∞}
        | Real       // R = {Q & I}
        | Rational   // Q = {x where x = a/b & b != 0}
        | Irrational // I = {x where x != a/b & a != b}
        | Complex    // C = {x where x = a + bi}

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>NumberSet</c>.</p>
    /// </summary>
    /// <param name='set'> the <c>NumberSet</c> </param>
    /// <returns> the <c>string</c> representation of the supplied <c>NumberSet</c> </returns>
    let strNumberSet (set: NumberSet): string =
        match set with
         | Natural    -> "N"
         | Integer    -> "Z"
         | Real       -> "R"
         | Rational   -> "Q"
         | Irrational -> "I"
         | Complex    -> "C"

    /// <summary>
    ///     <p>The <c>ParameterType</c> is a parameter in a function in <b>Diorite</b>.</p>
    ///     <p><b>1.</b> The first value (<c>VariableType</c>), which is the identifier for the parameter.</p>
    ///     <p><b>2.</b> The second value (<c>NumberSet</c>), which denotes the number set which the parameter must
    ///        comply with in order for the function to accept it.
    ///     </p>
    /// </summary>
    type ParameterType = VariableType * NumberSet

    /// <summary>
    ///     <p>Produces the <c>string</c> representation of the supplied <c>ParameterType</c>.</p>
    /// </summary>
    /// <param name='param'> the <c>ParameterType</c> </param>
    /// <returns> the <c>string</c> representation of the supplied <c>ParameterType</c> </returns>
    let strParameterType (param: ParameterType): string =
        let (identifier: VariableType), (set: NumberSet) = param
        $"{strVariable identifier} -> {strNumberSet set}"

    /// <summary>
    ///     <p>The <c>FunctionMetadata</c> record type encompasses data about a function in <b>Diorite</b>.</p>
    ///     <p>It retains data such as: the <c>symbol</c>ic name it may have, whether the return value should be inlined
    ///        (a compile-time optimization), or whether the function should maintain a cache that reduces the number of
    ///        repeat computations - for example, computing the fibonacci number at the 5th place, then the 4th place.
    ///     </p>
    /// </summary>
    type FunctionMetadata = {
        symbol:   string option
        inlined:  bool
        memoized: bool
    }

    /// <summary>
    ///     <p>The <c>FunctionAttributes</c> record type stores data related to a function definition in the
    ///        <b>Diorite</b>. It maintains a reference to the variable which references this function, hence a cyclical
    ///        reference; a list of parameters (<c>ParameterType</c>); its return type (<c>NumberSet</c> - its
    ///        range); and the metadata associated with this function definition (<c>FunctionMetadata</c>).
    ///     </p>
    /// </summary>
    type FunctionAttributes = {
        identifier: VariableType
        parameters: ParameterType list
        returns:    NumberSet
        metadata:   FunctionMetadata
    }
