// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    DSL.fs
// Summary: The DSL for the parsing stage for Diorite
// Author:  Arsngrobg
// Version: v1.6
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Parser

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Lexer
open Diorite.Lang.Core.Errors

/// <summary>
///     <p>The Domain Specific Language (DSL) for the parsing stage in <b>Diorite</b>.</p>
/// </summary>
module DSL =
    /// <summary>
    ///     <p>The value that a <c>Parser</c> produces upon successful evaluation of an arbitrary number of
    ///        <c>Token</c>s.
    ///     </p>
    /// </summary>
    type ParseState<'a>    = 'a * TokenStream
    /// <summary>
    ///     <p>A <c>ParseResult</c> is a <c>Result</c> value that is produced from a <c>Parser</c>.</p>
    /// </summary>
    type ParseResult<'a>   = ParseState<'a> Result
    /// <summary>
    ///     <p>A chain of executable operations to apply a specific rule of the grammar.</p>
    ///     <p>On Successful parse, it returns a <c>ParseState</c>, or a <c>SyntaxError</c> on failure.</p>
    /// </summary>
    type Parser<'a>        = TokenStream -> ParseResult<'a>
    /// <summary>
    ///     <p>A special superset type of <c>Parser</c> that is lazily produced.</p>
    ///     <p>This is to prevent recursive parsers from infinitely reproducing.</p>
    /// </summary>
    type DeferredParser<'a>  = unit -> Parser<'a>

    /// <summary>
    ///     <p>Produces a <c>Parser</c>, that returns <c>'a</c>.</p>
    /// </summary>
    /// <param name="a"> the value the <c>Parser</c> will return </param>
    /// <returns> a <c>Parser</c> that returns <c>a</c> </returns>
    let OfParser (a: 'a): Parser<'a> =
        (fun tokens ->
            Ok (a, tokens)
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c>, that executes an <c>action</c> if all tokens are exhausted.</p>
    /// </summary>
    /// <param name="action"> the action to perform if no tokens are available </param>
    /// <returns> a <c>Parser</c> that may execute this <c>action</c> </returns>
    let IfEmpty (action: unit -> 'a): Parser<'a> =
        (fun tokens ->
            match tokens with
             | []        -> Ok (action (), [])
             | head :: _ ->
                 ("Token stream is not empty when expected to be", Some (head.line, head.column))
                 ||> SyntaxError
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c>, that accepts the <c>TokenType</c> at the head of the
    ///        <c>TokenStream</c>.
    ///     </p>
    /// </summary>
    /// <param name="id"> the <c>TokenType</c> to check at the head of the <c>TokenStream</c> </param>
    /// <returns> a <c>Parser</c> that attempts to consume </returns>
    let Accept (id: TokenType): Parser<Token> =
        (fun tokens ->
            let inline GetErrorMessage (token: Token option): string * (uint * uint) option =
                match token with
                 | Some t -> $"Expected {id} - got \"{t.lexeme}\" instead", Some (t.line, t.column)
                 | None   -> $"Expected {id}", None

            match tokens with
             | head :: tail when head.id = id -> (head, tail) |> Ok
             | head :: _                      -> (head |> (Some >> GetErrorMessage)) ||> SyntaxError
             | []                             ->  None |>          GetErrorMessage   ||> SyntaxError
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c>, that tries to evaluate the sequence of parsers from left-to-right
    ///        (or top-to-bottom), and returns the value of the <c>Parser</c> that succeeds first.
    ///     </p>
    /// </summary>
    /// <param name="parsers"> the list of one-or-more <c>Parser</c>s to evaluate </param>
    /// <returns> a <c>Parser</c> that evaluates the sequence of <c>Parser</c>s </returns>
    let rec Any (parsers: Parser<'a> list): Parser<'a> =
        (fun tokens ->
            match parsers with
             | []           -> failwith "Cannot have an Any of an empty list of Parsers"
             | [parser]     -> parser tokens
             | head :: tail ->
                 match (head tokens) with
                  | Error _ -> (Any tail) tokens
                  | Ok    s -> Ok s
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c>, that is not required to parse the specific sequence of <c>Token</c>s.</p>
    /// </summary>
    /// <param name="parser"> the <c>Parser</c> to optionally execute </param>
    /// <returns> a <c>Parser</c> that returns the <c>option</c> value of the original <c>Parser</c> type </returns>
    let inline Optional (parser: Parser<'a>): Parser<'a option> =
        (fun tokens ->
            match (parser tokens) with
             | Ok    (a, remaining) -> Ok (Some a, remaining)
             | Error _              -> Ok (None,   tokens   )
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c>, that tries to evaluate the supplied <c>Parser</c> one or more times.</p>
    ///     <p>This returns a <c>Parser</c> that produces a list of the type of the input <c>Parser</c>.</p>
    ///     <p><i>Be careful, as it nullifies any proper error messages that may be returned by sub parsers.</i></p>
    /// </summary>
    /// <param name="parser"> the <c>Parser</c> to evaluate one-or-more times </param>
    /// <returns> a <c>Parser</c> that evaluates the supplied <c>Parser</c> one-or-more times </returns>
    let inline ZeroOrMore (parser: Parser<'a>): Parser<'a list> =
        let rec Accumulate (accum: 'a list): Parser<'a list> =
            (fun tokens ->
                match (parser tokens) with
                 | Ok    (a, remaining) -> (Accumulate (accum @ [a])) remaining
                 | Error _              -> Ok (accum, tokens)
            )

        (fun tokens ->
            (Accumulate []) tokens
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c>, that evaluates the first <c>Parser</c> and feeds the result to the next
    ///        parser.
    ///     </p>
    /// </summary>
    /// <param name="parser"> the <c>Parser</c> </param>
    /// <param name="producer"> the function that produces a new <c>Parser</c> that curries the return val </param>
    /// <returns> a <c>Parser</c> that feeds the return value of the first <c>Parser</c> to the next one </returns>
    let inline Bind (parser: Parser<'a>) (producer: 'a -> Parser<'b>): Parser<'b> =
        (fun tokens ->
            (parser tokens) |> Result.bind (fun (l, tail) -> (producer l) tail)
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c>, that evaluates both the left and right <c>Parser</c>s and returns a paired
    ///        tuple that contains both values.
    ///     </p>
    /// </summary>
    /// <param name="left"> the left <c>Parser</c> </param>
    /// <param name="right"> the right <c>Parser</c> </param>
    /// <returns> a <c>Parser</c> that evaluates and returns the result of both <c>Parser</c>s </returns>
    let inline Then (left: Parser<'a>) (right: Parser<'b>): Parser<'a * 'b> =
        (fun tokens ->
            (left  tokens) |> Result.bind (fun (a, tail     ) ->
            (right tail  ) |> Result.bind (fun (b, remaining) ->
                Ok ((a, b), remaining)
            ))
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c> that evaluates both <c>Parser</c>s, and returns the result of the right-most
    ///        <c>Parser</c>. It ignores the value of the <c>left</c> <c>Parser</c>.
    ///     </p>
    /// </summary>
    /// <param name="left"> the left <c>Parser</c> <i>(ignored upon evaluation)</i> </param>
    /// <param name="right">the left <c>Parser</c> </param>
    /// <returns> a <c>Parser</c> that combines and returns the value of the right <c>Parser</c> </returns>
    let inline IgnoreThen (left: Parser<'a>) (right: Parser<'b>): Parser<'b> =
        (fun tokens ->
            (left tokens) |> Result.bind (fun (_, tail) -> (right tail))
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c> that evaluates both <c>Parser</c>s, and returns the result of the left-most
    ///        <c>Parser</c>. It ignores the value of the <c>right</c> <c>Parser</c>.
    ///     </p>
    /// </summary>
    /// <param name="left"> the left <c>Parser</c> </param>
    /// <param name="right">the left <c>Parser</c> <i>(ignored upon evaluation)</i> </param>
    /// <returns> a <c>Parser</c> that combines and returns the value of the left <c>Parser</c> </returns>
    let inline ThenIgnore (left: Parser<'a>) (right: Parser<'b>): Parser<'a> =
        (fun tokens ->
            (left  tokens) |> Result.bind (fun (l, tail     ) ->
            (right tail  ) |> Result.bind (fun (_, remaining) ->
                Ok (l, remaining)
            ))
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c> which transforms the value of the <c>Parser</c> into another type.</p>
    /// </summary>
    /// <param name="mapper"> the mapping function that transforms the result of the previous <c>Parser</c> </param>
    /// <param name="parser"> the <c>Parser</c> </param>
    /// <returns> a <c>Parser</c> that maps the return value of the original <c>Parser</c> to another </returns>
    let inline Map (mapper: 'a -> 'b) (parser: Parser<'a>): Parser<'b> =
        (fun tokens ->
            (parser tokens) |> Result.bind (fun (a, remaining) ->
                let b: 'b = mapper a
                Ok (b, remaining)
            )
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c> which returns the <c>value</c> upon successful parse.</p>
    ///     <p>This is semantically the same as:
    ///        <code>
    ///           parser |> Map (fun _ -> value)
    ///        </code>
    ///     </p>
    /// </summary>
    /// <param name="value"> the value to return upon successful parse </param>
    /// <param name="parser"> the <c>Parser</c> to evaluate </param>
    /// <returns> a <c>Parser</c> that may return the <c>value</c> provided </returns>
    let inline As (value: 'b) (parser: Parser<'a>): Parser<'b> =
        parser |> Map (fun _ -> value)

    /// <summary>
    ///     <p>Produces a <c>Parser</c> derived from the <c>DeferredParser</c>.</p>
    ///     <p>It also signals that the <c>DeferredParser</c> is to be evaluated later and not during initial
    ///        evaluation of the parent/calling <c>Parser</c>.
    ///     </p>
    /// </summary>
    /// <param name="parser"> the <c>DeferredParser</c> to translate into a <c>Parser</c> </param>
    /// <returns> a <c>DeferredParser</c>, in the form of a <c>Parser</c> </returns>
    let inline Deferred (parser: DeferredParser<'a>): Parser<'a> =
        (fun tokens ->
            (parser ()) tokens
        )

    /// <summary>
    ///     <p>Produces a <c>Parser</c> that is supposed to fail.</p>
    ///     <p>It returns a <c>SyntaxError</c> when invoked.</p>
    /// </summary>
    /// <param name="msg"> the message to be carried by the <c>SyntaxError</c> </param>
    /// <returns> a <c>Parser</c> that specifically returns a <c>SyntaxError</c> with the <c>msg</c> </returns>
    let inline Fail (msg: string): Parser<'a> =
        (fun _ ->
            (msg, None) ||> SyntaxError
        )
