// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Parser.fs
// Summary: The lexical analyzer for the Diorite language
// Author:  Arsngrobg, Borngle
// Version: v1.8
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The <c>Parser</c> module groups up related bindings for parsing a token stream.
/// </summary>
[<RequireQualifiedAccess>]
module Parser =
    /// <summary>
    ///     This is the type that is returned by parsing functions.
    /// </summary>
    type ParseResult = (ASTNode * TokenStream) Result

    // <Value>     ::= <Undefined>
    //              |  <Infinity>
    //              |  <Pi>
    //              |  <Tau>
    //              |  <Euler>
    //              |  <Identifier>
    //              |  <Number>
    // let parseValue (tokens: TokenStream): ParseResult =
    //     match tokens with
    //      | Token.Undefined            :: tail -> Success (ASTNode.Undefined,                tail)
    //      | Token.Infinity             :: tail -> Success (ASTNode.Infinity,                 tail)
    //      | Token.Pi                   :: tail -> Success (ASTNode.Number     3.14159265358, tail)
    //      | Token.Tau                  :: tail -> Success (ASTNode.Number     6.28318530717, tail)
    //      | Token.Euler                :: tail -> Success (ASTNode.Number     2.71828182845, tail)
    //      | Token.Identifier (ch, sub) :: tail -> Success (ASTNode.Identifier (ch, sub),     tail)
    //      | Token.Number      num      :: tail -> Success (ASTNode.Number     num,           tail)
    //      | token                      :: _    -> SyntaxError $"Unexpected token: {token}"
    //      | []                                 -> SyntaxError "Expected token yet no value found"

    let parser (tokens: TokenStream): TokenStream =
        let rec E (tokens: TokenStream): TokenStream = (T >> Eopt) tokens
        and Eopt (tokens: TokenStream): TokenStream =
            match tokens with
            | Token.Plus   :: tail -> (T >> Eopt) tail
            | Token.Hyphen :: tail -> (T >> Eopt) tail
            | _                    -> tokens
        and T (tokens: TokenStream): TokenStream = (NR >> Topt) tokens
        and Topt (tokens: TokenStream): TokenStream =
            match tokens with
            | Asterisk     :: tail -> (NR >> Topt) tail
            | ForwardSlash :: tail -> (NR >> Topt) tail
            | _ -> tokens
        and NR (tokens: TokenStream): TokenStream =
            match tokens with
            | Token.Number value :: tail -> Token.Number value :: tail
            | LeftParenthesis    :: tail ->
                match E tail with
                 | RightParenthesis :: tail -> tail
                 | _ -> raise (System.Exception("SyntaxError"))
            | _ -> raise (System.Exception("SyntaxError"))
        E tokens

    let eval (tokens: TokenStream) =
        let rec E (tokens: TokenStream) = (T >> Eopt) (tokens: TokenStream)
        and Eopt (tokens, value) =
            match tokens with
            | Plus   :: tail -> let (remaining, current) = T tail
                                Eopt (remaining, value + current)
            | Hyphen :: tail -> let (remaining, current) = T tail
                                Eopt (remaining, value - current)
            | _ -> (tokens, value)
        and T (tokens: TokenStream) = (NR >> Topt) (tokens: TokenStream)
        and Topt (tokens, value) =
            match tokens with
            | Asterisk    :: tail -> let (remaining, current) = NR tail
                                     Topt (remaining, value * current)
            | ForwardSlash :: tail -> let (remaining, current) = NR tail
                                      Topt (remaining, value / current)
            | _ -> ((tokens: TokenStream), value)
        and NR (tokens: TokenStream) =
            match (tokens: TokenStream) with
            | Token.Number value :: tail -> (tail, value)
            | LeftParenthesis    :: tail -> let (remaining, current) = E tail
                                            match remaining with
                                             | RightParenthesis :: tail -> (tail, current)
                                             | _ -> raise (System.Exception("SyntaxError"))
            | _ -> raise (System.Exception("SyntaxError"))
        E tokens
