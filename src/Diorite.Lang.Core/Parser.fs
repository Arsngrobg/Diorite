// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Parser.fs
// Summary: The parsing logic for the Diorite language
// Author:  Arsngrobg
// Version: v1.14
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

open Diorite.Lang.Core

#nowarn 40 // for recursive value definitions

/// <summary>
///     <p>The <c>Parser</c> module.</p>
/// </summary>
[<RequireQualifiedAccess>]
module Parser =
    let GetNumberValue (token: Token): ValueType =
        match token.value with
         | TokenValue.Number n -> ValueType.Number n
         | _                   -> failwith $"Lexer.GetNumberValue - getting Number value from {token.id}"

    let GetVariableValue (token: Token): VariableType =
        match token.value with
         | TokenValue.Variable v -> v
         | _                     -> failwith $"Parser.GetVariableValue - getting Variable value from {token.id}"

    [<AutoOpen>]
    module DSL =
        // domain types
        type ParseState<'a>  = 'a * Lexer.TokenStream
        type ParseResult<'a> = ParseState<'a> Result
        type Parser<'a>      = Lexer.TokenStream -> ParseResult<'a>

        /// <summary>
        ///     <p>Produces a <c>Parser</c>, that returns <c>'a</c>.</p>
        /// </summary>
        /// <param name="a"> the value the <c>Parser</c> will return </param>
        /// <returns> a <c>Parser</c> that returns <c>a</c> </returns>
        let OfParser (a: 'a): Parser<'a> =
            (fun tokens -> Ok (a, tokens))

        /// <summary>
        ///     <p>Produces a <c>Parser</c>, that accepts the <c>TokenType</c> at the head of the <c>TokenStream</c>.</p>
        /// </summary>
        /// <param name="id"> the <c>TokenType</c> to check at the head of the <c>TokenStream</c> </param>
        /// <returns> a <c>Parser</c> that attempts to consume </returns>
        let Accept (id: TokenType): Parser<Token> =
            (fun tokens ->
                match tokens with
                 | head :: tail when head.id = id -> (head, tail) |> Ok
                 | head :: _                      -> SyntaxError $"Expected {id} - got \"{head.lexeme}\" instead"
                 | []                             -> SyntaxError $"Expected {id}"
            )

        /// <summary>
        ///     <p>Produces a <c>Parser</c>, that tries to evaluate the sequence of parsers from left-to-right
        ///        (or top-to-bottom), and returns the value of the <c>Parser</c> that succeeds first.
        ///     </p>
        /// </summary>
        /// <param name="parsers"> the list of one-or-more <c>Parser</c>s to evaluate </param>
        /// <returns> a <c>Parser</c> that evaluates the sequence of <c>Parser</c>s </returns>
        let rec Choice (parsers: Parser<'a> list): Parser<'a> =
            (fun tokens ->
                match parsers with
                 | []           -> failwith "Cannot have a choice of an empty list of Parsers"
                 | [parser]     -> parser tokens
                 | head :: tail ->
                     match (head tokens) with
                      | Error _ -> (Choice tail) tokens
                      | Ok    s -> Ok s
            )

        /// <summary>
        ///     <p>Produces a <c>Parser</c>, that evaluates the first <c>Parser</c> and feeds the result to the next parser.</p>
        /// </summary>
        /// <param name="parser"> the <c>Parser</c> </param>
        /// <param name="producer"> the function that produces a new <c>Parser</c> that curries the return value </param>
        /// <returns> a <c>Parser</c> that feeds the return value of the first <c>Parser</c> to the next one </returns>
        let Feed (parser: Parser<'a>) (producer: 'a -> Parser<'b>): Parser<'b> =
            (fun tokens ->
                (parser tokens) ?=> fun (l, tail) -> (producer l) tail
            )

        /// <summary>
        ///     <p>Produces a <c>Parser</c>, that evaluates both the left and right <c>Parser</c>s and returns a paired
        ///        tuple that contains both values.
        ///     </p>
        /// </summary>
        /// <param name="left"> the left <c>Parser</c> </param>
        /// <param name="right"> the right <c>Parser</c> </param>
        /// <returns> a <c>Parser</c> that evaluates and returns the result of both <c>Parser</c>s </returns>
        let Then (left: Parser<'a>) (right: Parser<'b>): Parser<'a * 'b> =
            (fun tokens ->
                (left  tokens) ?=> fun (a, tail     ) ->
                (right tail  ) ?=> fun (b, remaining) ->
                    Ok ((a, b), remaining)
            )

        /// <summary>
        ///     <p>Produces a <c>Parser</c> that evaluates both <c>Parser</c>s, and returns the result of the right-most
        ///        <c>Parser</c>. It ignores the value of the <c>left</c> <c>Parser</c>.
        ///     </p>
        /// </summary>
        /// <param name="left"> the left <c>Parser</c> <i>(ignored upon evaluation)</i> </param>
        /// <param name="right">the left <c>Parser</c> </param>
        /// <returns> a <c>Parser</c> that combines and returns the value of the right <c>Parser</c> </returns>
        let IgnoreThen (left: Parser<'a>) (right: Parser<'b>): Parser<'b> =
            (fun tokens ->
                (left tokens) ?=> fun (_, tail) -> (right tail)
            )

        /// <summary>
        ///     <p>Produces a <c>Parser</c> that evaluates both <c>Parser</c>s, and returns the result of the left-most
        ///        <c>Parser</c>. It ignores the value of the <c>right</c> <c>Parser</c>.
        ///     </p>
        /// </summary>
        /// <param name="left"> the left <c>Parser</c> </param>
        /// <param name="right">the left <c>Parser</c> <i>(ignored upon evaluation)</i> </param>
        /// <returns> a <c>Parser</c> that combines and returns the value of the left <c>Parser</c> </returns>
        let ThenIgnore (left: Parser<'a>) (right: Parser<'b>): Parser<'a> =
            (fun tokens ->
                (left  tokens) ?=> fun (l, tail     ) ->
                (right tail  ) ?=> fun (_, remaining) ->
                    Ok (l, remaining)
            )

        /// <summary>
        ///     <p>Produces a parser which transforms the value of the <c>Parser</c> into another type.</p>
        /// </summary>
        /// <param name="mapper"> the mapping function that transforms the result of the previous <c>Parser</c> </param>
        /// <param name="parser"> the <c>Parser</c> </param>
        /// <returns> a <c>Parser</c> that maps the return value of the original <c>Parser</c> to another type </returns>
        let Map (mapper: 'a -> 'b) (parser: Parser<'a>): Parser<'b> =
            (fun tokens ->
                (parser tokens) ?=> fun (a, remaining) ->
                    let b: 'b = mapper a
                    Ok (b, remaining)
            )

    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Undefined</c>.</p>
    /// </summary>
    let UndefinedParser: Parser<ValueType> = (Accept TokenType.Undefined) |> Map (fun _ -> ValueType.Undefined)
    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Infinity</c>.</p>
    /// </summary>
    let InfinityParser: Parser<ValueType> = (Accept TokenType.Infinity) |> Map (fun _ -> ConstantInfinity)
    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Pi</c>.</p>
    /// </summary>
    let PiParser: Parser<ValueType> = (Accept TokenType.Pi) |> Map (fun _ -> ConstantPi)
    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Tau</c>.</p>
    /// </summary>
    let TauParser: Parser<ValueType> = (Accept TokenType.Tau) |> Map (fun _ -> ConstantTau)
    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Euler</c>.</p>
    /// </summary>
    let EulerParser: Parser<ValueType> = (Accept TokenType.Euler) |> Map (fun _ -> ConstantEuler)
    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Number</c>.</p>
    /// </summary>
    let NumberParser: Parser<ValueType> = (Accept TokenType.Number) |> Map GetNumberValue

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> value.</p>
    /// </summary>
    let ValueParser: Parser<ValueType> =
        Choice [
            UndefinedParser
            InfinityParser
            PiParser
            TauParser
            EulerParser
            NumberParser
        ]

    /// <summary>
    ///     <p>The parser that accepts a <b>Diorite</b> expression.</p>
    ///     <code>
    ///        &lt;Expression&gt;  ::= &lt;Term&gt; &lt;Expression'&gt;
    ///        &lt;Expression'&gt; ::= ε
    ///                       |  "+" &lt;Term&gt; &lt;Expression'&gt;
    ///                       |  "-" &lt;Term&gt; &lt;Expression'&gt;
    ///     </code>
    /// </summary>
    let rec ExpressionParser: Parser<Expression> =
        // tail-end parser
        let rec Expression'Parser (head: Expression): Parser<Expression> =
            Choice [
                // <Expression'> ::= "+" <Term> <Expression'>
                (((Accept TokenType.Plus) |> IgnoreThen <| TermParser) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Addition, e)
                ) |> Feed <| Expression'Parser)

                // <Expression'> ::= "+" <Term> <Expression'>
                (((Accept TokenType.Hyphen) |> IgnoreThen <| TermParser) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Subtraction, e)
                ) |> Feed <| Expression'Parser)

                // <Expression'> ::= ε
                (OfParser head)
            ]

        // <Expression> ::= <Term> <Expression'>
        (TermParser |> Feed <| Expression'Parser)

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> term.</p>
    ///     <code>
    ///        &lt;Term&gt;  ::= &lt;Factor&gt; &lt;Term'&gt;
    ///        &lt;Term'&gt; ::= ε
    ///                 |  "*"  &lt;Factor&gt; &lt;Term'&gt;
    ///                 |  "/"  &lt;Factor&gt; &lt;Term'&gt;
    ///                 |  "%"  &lt;Factor&gt; &lt;Term'&gt;
    ///                 |  "//" &lt;Factor&gt; &lt;Term'&gt;
    ///     </code>
    /// </summary>
    and TermParser: Parser<Expression> =
        // tail-end parser
        let rec Term'Parser (head: Expression): Parser<Expression> =
            Choice [
                // <Term'> ::= "*" <Factor> <Term'>
                (((Accept TokenType.Asterisk) |> IgnoreThen <| FactorParser) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Multiplication, e)
                ) |> Feed <| Term'Parser)

                // <Term'> ::= "/" <Factor> <Term'>
                (((Accept TokenType.ForwardSlash) |> IgnoreThen <| FactorParser) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Division, e)
                ) |> Feed <| Term'Parser)

                // <Term'> ::= "%" <Factor> <Term'>
                (((Accept TokenType.Percentage) |> IgnoreThen <| FactorParser) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Modulo, e)
                ) |> Feed <| Term'Parser)

                // <Term'> ::= "//" <Factor> <Term'>
                (((Accept TokenType.DoubleForwardSlash) |> IgnoreThen <| FactorParser) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.FloorDivision, e)
                ) |> Feed <| Term'Parser)

                // <Term'> ::= ε
                (OfParser head)
            ]

        // <Term'> ::= <Factor> <Term'>
        (FactorParser |> Feed <| Term'Parser)

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> factor.</p>
    ///     <code>
    ///        &lt;Factor&gt; ::= &lt;Exponent&gt;
    ///                  |  "+" &lt;Factor&gt;
    ///                  |  "-" &lt;Factor&gt;
    ///     </code>
    /// </summary>
    and FactorParser: Parser<Expression> =
        Choice [
            // <Factor> ::= "+" <Factor>
            ((Accept TokenType.Plus) |> IgnoreThen <| (fun tokens -> FactorParser tokens)) |> Map (
                fun e -> Expression.UnaryOperation (e, UnaryOperator.Positive)
            )

            // <Factor> ::= "-" <Factor>
            ((Accept TokenType.Hyphen) |> IgnoreThen <| (fun tokens -> FactorParser tokens)) |> Map (
                fun e -> Expression.UnaryOperation (e, UnaryOperator.Negative)
            )

            // <Factor> ::= <Exponent>    
            ExponentParser
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> factor.</p>
    ///     <code>
    ///        &lt;Exponent&gt; ::= &lt;Factorial&gt;
    ///                    |  &lt;Factorial&gt; "^" &lt;Factorial&gt;
    ///     </code>
    /// </summary>
    and ExponentParser: Parser<Expression> =
        Choice [
            // <Exponent> ::= <Factorial> "^" <Factorial>
            (FactorialParser |> Then <| ((Accept TokenType.Hat) |> IgnoreThen <| FactorialParser)) |> Map (
                fun (l, r) -> Expression.BinaryOperation (l, BinaryOperator.Exponent, r)
            )

            // <Exponent> ::= <Factorial>
            FactorialParser
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> factorial.</p>
    ///     <code>
    ///        &lt;Factorial&gt;  ::= &lt;SubExpression&gt; &lt;Factorial'&gt;
    ///        &lt;Factorial'&gt; ::= ε
    ///                      |  "!" &lt;Factorial'&gt;
    ///     </code>
    /// </summary>
    and FactorialParser: Parser<Expression> =
        // tail-end parser
        let rec Factorial'Parser (head: Expression): Parser<Expression> =
            Choice [
                // <Factorial'> ::= "!" &lt;Factorial'>
                (Accept TokenType.Exclamation) |> Map (
                    fun _ -> Expression.UnaryOperation (head, UnaryOperator.Factorial)
                ) |> Feed <| Factorial'Parser

                // <Factorial'> ::= ε
                OfParser(head);
            ]

        // <Factorial> ::= <SubExpression> <Factorial'>
        (SubExpressionParser |> Feed <| Factorial'Parser)

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> subexpression.</p>
    ///     <code>
    ///        &lt;SubExpression&gt; ::= &lt;Value&gt;
    ///                         |  "(" &lt;Expression&gt; ")"
    ///                         |  "|" &lt;Expression&gt; "|"
    ///     </code>
    /// </summary>
    and SubExpressionParser: Parser<Expression> =
        Choice [
            // <SubExpression> ::= "(" <Expression> ")"
            (Accept TokenType.LeftParenthesis |> IgnoreThen <| (fun tokens -> ExpressionParser tokens))

            // <SubExpression> ::= <Value>
            ValueParser |> Map Expression.Value
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> function call.</p>
    ///     <code>
    ///        &lt;FunctionCall&gt; ::= &lt;Variable&gt; "(" &lt;Args&gt; ")"
    ///                        |  &lt;Symbol&gt;   "(" &lt;Args&gt; ")"
    ///     </code>
    /// </summary>
    let rec FunctionCallParser: Parser<Expression> =
        Choice [
            (Accept TokenType.Symbol)   |> Map (fun t -> FunctionReferenceType.OfSymbol(t.lexeme))
            (Accept TokenType.Variable) |> Map (fun t -> FunctionReferenceType.OfVariable(GetVariableValue(t)))
        ] |> Then <|
        (
            (Accept TokenType.LeftParenthesis) |> IgnoreThen <|
            ArgsParser                         |> ThenIgnore <|
            (Accept TokenType.RightParenthesis)
        ) |> Map Expression.FunctionCall

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a sequence of <b>Diorite</b> function arguments.</p>
    ///     <code>
    ///        &lt;Args&gt; ::= &lt;Expression&gt;
    ///                |  &lt;Expression&gt; "," &lt;Args&gt;
    ///     </code>
    /// </summary>
    and ArgsParser: Parser<Expression list> =
        Choice [
            // <Args> ::= <Expression> "," <Args>
            ExpressionParser |> Then <| (Accept TokenType.Comma |> IgnoreThen <| (fun t -> ArgsParser t)) |> Map (
                fun (arg, args) -> arg :: args
            )
            // <Args> ::= <Expression>
            ExpressionParser |> Map (fun e -> [e])
        ]
