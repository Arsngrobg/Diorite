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
    ///     The <c>ParseResult</c> describes a <c>Result</c> that holds a tuple of <c>ASTNode</c> and the resulting
    ///     <c>Token</c> stream from the parsing operation.
    /// </summary>
    type ParseResult = (ASTNode * Token list) Result

    /// <summary>
    ///     attempts to accept the <c>expected</c> token and convert it to its equivalent <c>Parser.ASTNode</c>
    ///     representation using the supplied <c>mapper</c> function.
    /// </summary>
    /// <param name='tokens'> the token stream </param>
    /// <param name='expected'> the token that is to be accepted </param>
    /// <param name='mapper'> the function to map the <c>Token</c> to its equivalent <c>Parser.ASTNode</c> </param>
    let accept (tokens: Token list) (expected: Token) (mapper: Token -> ASTNode): ParseResult =
        match tokens with
         | head :: tail when head = expected -> Success (mapper head, tail)
         | head :: _                         -> Failure (SyntaxError $"Unexpected token: {head.GetType()}")
         | _                                 -> Failure (SyntaxError "Missing expected token.")

    let parseExpression (tokens: Token list): ParseResult =
        Success (Addition, [])


    /// <summary>
    ///     Analyses the provided token stream and parses it into a structured AST (Abstract Syntax Tree).
    /// </summary>
    /// <param name='tokens'> the token stream to parse </param>
    /// <returns> a <c>Result</c> that may contain the successful result of the parse, or <c>Failure</c> </returns>
    let parse(tokens: Token): unit Result =
        Success ()

// the parse stages (precedence: top-bottom
// <Value>        ::= <Undefined>
//                 |  <Infinity>
//                 |  <Pi>
//                 |  <Tau>
//                 |  <Euler>
//                 |  <Identifier>
//                 |  <Number>
// let value (tokens: Token list): Parser.ParseResult =
//     match tokens with
//      | Token.Undefined              :: tail -> Success (Parser.Undefined,                tail)
//      | Token.Infinity               :: tail -> Success (Parser.Infinity,                 tail)
//      | Token.Pi                     :: tail -> Success (Parser.Number 3.141592653589793, tail)
//      | Token.Tau                    :: tail -> Success (Parser.Number 6.283185307179586, tail)
//      | Token.Euler                  :: tail -> Success (Parser.Number 2.718281828459045, tail)
//      | Token.Identifier (char, sub) :: tail -> Success (Parser.Identifier (char, sub),   tail)
//      | Token.Number      num        :: tail -> Success (Parser.Number num,               tail)
//      | illegal                      :: _    -> Failure (SyntaxError $"Illegal token: {illegal.GetType()}"      )
//      | unexpected                           -> Failure (SyntaxError $"Unexpected token: {unexpected.GetType()}")
