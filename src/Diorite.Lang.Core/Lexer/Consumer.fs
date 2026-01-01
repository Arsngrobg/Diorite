// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Consumer.fs
// Summary: The type definition of the Consumer type and essential factory functions
// Author:  Arsngrobg, Borngle
// Version: v1.1
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Lexer

/// <summary>
///     <p>A <c>Consumer</c> is a curried application of the <c>consume</c> function that has been preloaded with a
///        <c>predicate</c>. It can be simplified to a function that splits a list into two chunks: the consumed
///        characters, and the remaining characters.
///     </p>
/// </summary>
type Consumer = char list -> char list * char list

/// <summary>
///     <p> submodule, the factory functions for the <c>Consumer</c> type.</p>
/// </summary>
[<AutoOpen>]
module ConsumerFactories =
    /// <summary>
    ///     <p>Recursively consumes the characters that satisfy the <c>predicate</c> until it no longer can do so.</p>
    ///     <p>It returns the consumed characters and the remaining characters as a result of this operation.</p>
    /// </summary>
    /// <param name="predicate"> the predicate which determines if the characters consumed </param>
    /// <param name="source"> the source characters </param>
    /// <returns> the consumed characters and the remaining characters </returns>
    let rec Consume (predicate: char -> bool) (source: char list): char list * char list =
        match source with
         | c :: tail when predicate c ->
            let (consumed: char list), (remaining: char list) = Consume predicate tail
            (c :: consumed, remaining)
         | source                     -> ([], source)
