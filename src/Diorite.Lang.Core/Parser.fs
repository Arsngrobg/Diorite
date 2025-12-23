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

//#nowarn 40 // remove recursive objects at runtime warning

/// <summary>
///     <p>The <c>Parser</c> module.</p>
/// </summary>
[<RequireQualifiedAccess>]
module Parser =
    /// <summary>
    ///     <p>Tries to obtain the <c>TokenValue.Number</c> stored by the supplied <c>Token</c>.</p>
    ///     <p>If the <c>Token</c> does not contain a value, or does not hold a <c>TokenValue.Number</c>, then this
    ///        function will throw a fatal error.
    ///     </p>
    /// </summary>
    /// <param name="token"> the <c>Token</c> to extract a number value from </param>
    /// <returns> the number value held by this token </returns>
    let GetNumberValue (token: Token): ValueType =
        match token.value with
         | TokenValue.Number n -> ValueType.Number n
         | _                   -> failwith $"Lexer.GetNumberValue - getting Number value from {token.id}"

    /// <summary>
    ///     <p>Tries to obtain the <c>TokenValue.Variable</c> stored by the supplied <c>Token</c>.</p>
    ///     <p>If the <c>Token</c> does not contain a value, or does not hold a <c>TokenValue.Variable</c>, then this
    ///        function will throw a fatal error.
    ///     </p>
    /// </summary>
    /// <param name="token"> the <c>Token</c> to extract a variable value from </param>
    /// <returns> the variable value held by this token </returns>
    let GetVariableValue (token: Token): VariableType =
        match token.value with
         | TokenValue.Variable v -> v
         | _                     -> failwith $"Parser.GetVariableValue - getting Variable value from {token.id}"

    [<AutoOpen>]
    module DSL =
        /// <summary>
        ///     <p>The value that a <c>Parser</c> produces upon successful evaluation of an arbitrary number of
        ///        <c>Token</c>s.
        ///     </p>
        /// </summary>
        type ParseState<'a>    = 'a * Lexer.TokenStream
        /// <summary>
        ///     <p>A <c>ParseResult</c> is a <c>Result</c> value that is produced from a <c>Parser</c>.</p>
        /// </summary>
        type ParseResult<'a>   = ParseState<'a> Result
        /// <summary>
        ///     <p>A chain of executable operations to apply a specific rule of the grammar.</p>
        ///     <p>On Successful parse, it returns a <c>ParseState</c>, or a <c>SyntaxError</c> on failure.</p>
        /// </summary>
        type Parser<'a>        = Lexer.TokenStream -> ParseResult<'a>
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
            (fun tokens -> Ok (a, tokens))

        /// <summary>
        ///     <p>Produces a <c>Parser</c>, that accepts the <c>TokenType</c> at the head of the
        ///        <c>TokenStream</c>.
        ///     </p>
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
        let rec Any (parsers: Parser<'a> list): Parser<'a> =
            (fun tokens ->
                match parsers with
                 | []           -> failwith "Cannot have a Any of an empty list of Parsers"
                 | [parser]     -> parser tokens
                 | head :: tail ->
                     match (head tokens) with
                      | Error _ -> (Any tail) tokens
                      | Ok    s -> Ok s
            )

        /// <summary>
        ///     <p>Produces a <c>Parser</c>, that evaluates the first <c>Parser</c> and feeds the result to the next
        ///        parser.
        ///     </p>
        /// </summary>
        /// <param name="parser"> the <c>Parser</c> </param>
        /// <param name="producer"> the function that produces a new <c>Parser</c> that curries the return val </param>
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
        ///     <p>Produces a <c>Parser</c> which transforms the value of the <c>Parser</c> into another type.</p>
        /// </summary>
        /// <param name="mapper"> the mapping function that transforms the result of the previous <c>Parser</c> </param>
        /// <param name="parser"> the <c>Parser</c> </param>
        /// <returns> a <c>Parser</c> that maps the return value of the original <c>Parser</c> to another </returns>
        let Map (mapper: 'a -> 'b) (parser: Parser<'a>): Parser<'b> =
            (fun tokens ->
                (parser tokens) ?=> fun (a, remaining) ->
                    let b: 'b = mapper a
                    Ok (b, remaining)
            )

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
                SyntaxError msg
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
    ///     <code>
    ///         &lt;Value&gt; ::= &lt;Undefined&gt;
    ///                  |  &lt;Infinity&gt;
    ///                  |  &lt;Pi&gt;
    ///                  |  &lt;Tau&gt;
    ///                  |  &lt;Euler&gt;
    ///                  |  &lt;Number&gt;
    ///     </code>
    /// </summary>
    let ValueParser: Parser<ValueType> =
        Any [
            UndefinedParser
            InfinityParser
            PiParser
            TauParser
            EulerParser
            NumberParser
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> variable.</p>
    ///     <code>
    ///         &lt;Variable&gt; ::= &lt;Letter&gt;
    ///                     |  &lt;Letter&gt; &lt;Digit&gt;
    ///     </code>
    /// </summary>
    let VariableParser: Parser<VariableType> = (Accept TokenType.Variable) |> Map GetVariableValue

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> number set.</p>
    ///     <code>
    ///         &lt;NumberSet&gt; ::= "N"
    ///                      |  "Z"
    ///                      |  "R"
    ///                      |  "Q"
    ///                      |  "I"
    ///                      |  "C"
    ///     </code>
    /// </summary>
    let NumberSetParser: Parser<NumberSet> =
        // reuse the variable parsing logic as number sets are tokenised as variables
        VariableParser |> Feed <| (fun (c, s) ->
            if s <> 0uy then
                Fail $"Expected a number set, got variable {c}{s-1uy} instead"
            else
                match c with
                 | 'N' -> OfParser NumberSet.Natural
                 | 'Z' -> OfParser NumberSet.Integer
                 | 'R' -> OfParser NumberSet.Real
                 | 'Q' -> OfParser NumberSet.Rational
                 | 'I' -> OfParser NumberSet.Irrational
                 | 'C' -> OfParser NumberSet.Complex
                 |  c  -> Fail     $"Expected a number set, got variable {c} instead"
        )

    /// <summary>
    ///     <p>The parser that accepts a <b>Diorite</b> expression.</p>
    ///     <code>
    ///        &lt;Expression&gt;  ::= &lt;Term&gt; &lt;Expression'&gt;
    ///        &lt;Expression'&gt; ::= ε
    ///                       |  "+" &lt;Term&gt; &lt;Expression'&gt;
    ///                       |  "-" &lt;Term&gt; &lt;Expression'&gt;
    ///     </code>
    /// </summary>
    let rec ExpressionParser: DeferredParser<Expression> = fun () ->
        // tail-end parser
        let rec Expression'Parser (head: Expression): Parser<Expression> =
            Any [
                // <Expression'> ::= "+" <Term> <Expression'>
                (((Accept TokenType.Plus) |> IgnoreThen <| (Deferred TermParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Addition, e)
                ) |> Feed <| Expression'Parser)

                // <Expression'> ::= "+" <Term> <Expression'>
                (((Accept TokenType.Hyphen) |> IgnoreThen <| (Deferred TermParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Subtraction, e)
                ) |> Feed <| Expression'Parser)

                // <Expression'> ::= ε
                (OfParser head)
            ]

        // <Expression> ::= <Term> <Expression'>
        ((Deferred TermParser) |> Feed <| Expression'Parser)

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
    and TermParser: DeferredParser<Expression> = fun () ->
        // tail-end parser
        let rec Term'Parser (head: Expression): Parser<Expression> =
            Any [
                // <Term'> ::= "*" <Factor> <Term'>
                (((Accept TokenType.Asterisk) |> IgnoreThen <| (Deferred FactorParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Multiplication, e)
                ) |> Feed <| Term'Parser)

                // <Term'> ::= "/" <Factor> <Term'>
                (((Accept TokenType.ForwardSlash) |> IgnoreThen <| (Deferred FactorParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Division, e)
                ) |> Feed <| Term'Parser)

                // <Term'> ::= "%" <Factor> <Term'>
                (((Accept TokenType.Percentage) |> IgnoreThen <| (Deferred FactorParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Modulo, e)
                ) |> Feed <| Term'Parser)

                // <Term'> ::= "//" <Factor> <Term'>
                (((Accept TokenType.DoubleForwardSlash) |> IgnoreThen <| (Deferred FactorParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.FloorDivision, e)
                ) |> Feed <| Term'Parser)

                // <Term'> ::= ε
                (OfParser head)
            ]

        // <Term'> ::= <Factor> <Term'>
        ((Deferred FactorParser) |> Feed <| Term'Parser)

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> factor.</p>
    ///     <code>
    ///        &lt;Factor&gt; ::= &lt;Exponent&gt;
    ///                  |  "+" &lt;Factor&gt;
    ///                  |  "-" &lt;Factor&gt;
    ///     </code>
    /// </summary>
    and FactorParser: DeferredParser<Expression> = fun () ->
        Any [
            // <Factor> ::= "+" <Factor>
            ((Accept TokenType.Plus) |> IgnoreThen <| (Deferred FactorParser)) |> Map (
                fun e -> Expression.UnaryOperation (e, UnaryOperator.Positive)
            )

            // <Factor> ::= "-" <Factor>
            ((Accept TokenType.Hyphen) |> IgnoreThen <| (Deferred FactorParser)) |> Map (
                fun e -> Expression.UnaryOperation (e, UnaryOperator.Negative)
            )

            // <Factor> ::= <Exponent>    
            (Deferred ExponentParser)
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> factor.</p>
    ///     <code>
    ///        &lt;Exponent&gt; ::= &lt;Factorial&gt;
    ///                    |  &lt;Factorial&gt; "^" &lt;Factorial&gt;
    ///     </code>
    /// </summary>
    and ExponentParser: DeferredParser<Expression> = fun () ->
        let factorial: Parser<Expression> = Deferred FactorialParser

        Any [
            // <Exponent> ::= <Factorial> "^" <Factorial>
            (factorial |> Then <|((Accept TokenType.Hat) |> IgnoreThen <| factorial)) |> Map (
                fun (l, r) -> Expression.BinaryOperation (l, BinaryOperator.Exponent, r)
            )

            // <Exponent> ::= <Factorial>
            (Deferred FactorialParser)
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> factorial.</p>
    ///     <code>
    ///        &lt;Factorial&gt;  ::= &lt;SubExpression&gt; &lt;Factorial'&gt;
    ///        &lt;Factorial'&gt; ::= ε
    ///                      |  "!" &lt;Factorial'&gt;
    ///     </code>
    /// </summary>
    and FactorialParser: DeferredParser<Expression> = fun () ->
        // tail-end parser
        let rec Factorial'Parser (head: Expression): Parser<Expression> =
            Any [
                // <Factorial'> ::= "!" &lt;Factorial'>
                (Accept TokenType.Exclamation) |> Map (
                    fun _ -> Expression.UnaryOperation (head, UnaryOperator.Factorial)
                ) |> Feed <| Factorial'Parser

                // <Factorial'> ::= ε
                OfParser(head);
            ]

        // <Factorial> ::= <SubExpression> <Factorial'>
        ((Deferred SubExpressionParser) |> Feed <| Factorial'Parser)

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> subexpression.</p>
    ///     <code>
    ///        &lt;SubExpression&gt; ::= &lt;Value&gt;
    ///                         |  "(" &lt;Expression&gt; ")"
    ///                         |  "|" &lt;Expression&gt; "|"
    ///                         |  &lt;FunctionCall&gt;
    ///     </code>
    /// </summary>
    and SubExpressionParser: DeferredParser<Expression> = fun () ->
        Any [
            // <SubExpression> ::= <FunctionCall>
            (Deferred FunctionCallParser)

            // <SubExpression> ::= "(" <Expression> ")"
            (Accept TokenType.LeftParenthesis |> IgnoreThen <|
            (Deferred ExpressionParser))       |> ThenIgnore <|
            (Accept TokenType.RightParenthesis)

            // <SubExpression> ::= "|" <Expression> "|"
            (Accept TokenType.Bar       |> IgnoreThen <|
            (Deferred ExpressionParser)) |> ThenIgnore <|
            (Accept TokenType.Bar)      |> Map (fun e -> Expression.UnaryOperation (e, UnaryOperator.Absolute))

            // <SubExpression> ::= <Value>
            (ValueParser |> Map Expression.Value)
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> function call.</p>
    ///     <code>
    ///        &lt;FunctionCall&gt; ::= &lt;Variable&gt; "(" &lt;Args&gt; ")"
    ///                        |  &lt;Symbol&gt;   "(" &lt;Args&gt; ")"
    ///     </code>
    /// </summary>
    and FunctionCallParser: DeferredParser<Expression> = fun () ->
        Any [
            (Accept TokenType.Symbol)   |> Map (fun t -> FunctionReferenceType.OfSymbol(t.lexeme))
            (Accept TokenType.Variable) |> Map (fun t -> FunctionReferenceType.OfVariable(GetVariableValue(t)))
        ] |> Then <|
        (
            (Accept TokenType.LeftParenthesis) |> IgnoreThen <|
            (Deferred ArgsParser)               |> ThenIgnore <|
            (Accept TokenType.RightParenthesis)
        ) |> Map Expression.FunctionCall

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a sequence of <b>Diorite</b> function arguments.</p>
    ///     <code>
    ///        &lt;Args&gt; ::= &lt;Expression&gt;
    ///                |  &lt;Expression&gt; "," &lt;Args&gt;
    ///     </code>
    /// </summary>
    and ArgsParser: DeferredParser<Expression list> = fun () ->
        Any [
            // <Args> ::= <Expression> "," <Args>
            ((Deferred ExpressionParser) |> Then <|
             ((Accept TokenType.Comma) |> IgnoreThen <|
              (Deferred ArgsParser)
             ) |> Map (
                fun (arg, args) -> arg :: args
             )
            )

            // <Args> ::= <Expression>
            ((Deferred ExpressionParser) |> Map (fun e -> [e]))
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> function parameter.</p>
    ///     <code>
    ///         &lt;Parameter&gt; ::= &lt;Letter&gt;
    ///                      |  &lt;Letter&gt; &lt;NumberSet&gt;
    ///     </code>
    /// </summary>
    let ParameterParser: Parser<FunctionParameter> =
        Any [
            // <Parameter> ::= <Letter> <NumberSet>
            (VariableParser |> Then <| (Accept TokenType.Colon |> IgnoreThen <| NumberSetParser))

            // <Parameter> ::= <Letter>
            (VariableParser |> Map (fun v -> (v, DefaultNumberSet)))
        ]
