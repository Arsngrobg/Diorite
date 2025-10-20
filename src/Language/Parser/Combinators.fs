// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Combinators.fs
// Summary:
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>Combinators</c> module defines the bindings that allow for a sort of Domain Specific Language (DSL) for
///     defining the parsing behaviour for the parser.
/// </summary>
[<AutoOpen>]
module Combinators =
    type Parser = TokenStream -> (ASTNode option * TokenStream) Result

    // atomic unit/operation
    let acceptAs (expected: Token) (mapper: Token -> ASTNode): Parser =
        fun tokens ->
            match tokens with
             | head :: tail when head = expected -> Success (Some (mapper head), tail  )
             | _                                 -> Success (None,               tokens)

    let orAccept (p1: Parser, p2: Parser): Parser = fun tokens ->
        match p1 <| tokens with
         | Success (Some node, remaining) -> Success (Some node, remaining)
         | Success (None,      _        ) -> p2 tokens
         | Failure err                    -> Failure err

    let parseUndefined: Parser =
        (Token.Undefined) |> acceptAs <| (fun _ -> ASTNode.Undefined)

    let parseInfinity: Parser =
        (Token.Infinity) |> acceptAs <| (fun _ -> ASTNode.Infinity)

    let parseConstant: Parser =
        let parsePi = acceptAs (Token.Pi) (fun _ -> ASTNode.Number)
