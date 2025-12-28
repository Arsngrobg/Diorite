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
        let inline Then (left: Parser<'a>) (right: Parser<'b>): Parser<'a * 'b> =
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
        let inline IgnoreThen (left: Parser<'a>) (right: Parser<'b>): Parser<'b> =
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
        let inline ThenIgnore (left: Parser<'a>) (right: Parser<'b>): Parser<'a> =
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
        let inline Map (mapper: 'a -> 'b) (parser: Parser<'a>): Parser<'b> =
            (fun tokens ->
                (parser tokens) ?=> fun (a, remaining) ->
                    let b: 'b = mapper a
                    Ok (b, remaining)
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

    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the end-of-file token (<c>";"</c>).</p>
    /// </summary>
    let EOF: Parser<Token> = (Accept TokenType.SemiColon)

    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Undefined</c>.</p>
    /// </summary>
    let UndefinedParser: Parser<ValueType> = (Accept TokenType.Undefined) |> As ValueType.Undefined
    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Infinity</c>.</p>
    /// </summary>
    let InfinityParser: Parser<ValueType> = (Accept TokenType.Infinity) |> As ConstantInfinity
    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Pi</c>.</p>
    /// </summary>
    let PiParser: Parser<ValueType> = (Accept TokenType.Pi) |> As ConstantPi
    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Tau</c>.</p>
    /// </summary>
    let TauParser: Parser<ValueType> = (Accept TokenType.Tau) |> As ConstantTau
    /// <summary>
    ///     <p>A <c>Parser</c> that accepts the token of type <c>Euler</c>.</p>
    /// </summary>
    let EulerParser: Parser<ValueType> = (Accept TokenType.Euler) |> As ConstantEuler
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
        VariableParser |> Bind <| (fun (c, s) ->
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
                ) |> Bind <| Expression'Parser)

                // <Expression'> ::= "+" <Term> <Expression'>
                (((Accept TokenType.Hyphen) |> IgnoreThen <| (Deferred TermParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Subtraction, e)
                ) |> Bind <| Expression'Parser)

                // <Expression'> ::= ε
                (OfParser head)
            ]

        // <Expression> ::= <Term> <Expression'>
        ((Deferred TermParser) |> Bind <| Expression'Parser)

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
                ) |> Bind <| Term'Parser)

                // <Term'> ::= "/" <Factor> <Term'>
                (((Accept TokenType.ForwardSlash) |> IgnoreThen <| (Deferred FactorParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Division, e)
                ) |> Bind <| Term'Parser)

                // <Term'> ::= "%" <Factor> <Term'>
                (((Accept TokenType.Percentage) |> IgnoreThen <| (Deferred FactorParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.Modulo, e)
                ) |> Bind <| Term'Parser)

                // <Term'> ::= "//" <Factor> <Term'>
                (((Accept TokenType.DoubleForwardSlash) |> IgnoreThen <| (Deferred FactorParser)) |> Map (
                    fun e -> Expression.BinaryOperation (head, BinaryOperator.FloorDivision, e)
                ) |> Bind <| Term'Parser)

                // <Term'> ::= ε
                (OfParser head)
            ]

        // <Term'> ::= <Factor> <Term'>
        ((Deferred FactorParser) |> Bind <| Term'Parser)

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
                ) |> Bind <| Factorial'Parser

                // <Factorial'> ::= ε
                OfParser(head);
            ]

        // <Factorial> ::= <SubExpression> <Factorial'>
        ((Deferred SubExpressionParser) |> Bind <| Factorial'Parser)

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> subexpression.</p>
    ///     <code>
    ///        &lt;SubExpression&gt; ::= &lt;Value&gt;
    ///                         |  &lt;Variable&gt;
    ///                         |  "("  &lt;Expression&gt; ")"
    ///                         |  "|"  &lt;Expression&gt; "|"
    ///                         |  "im" &lt;SubExpression&gt;
    ///                         |  "re" &lt;SubExpression&gt;
    ///                         |  &lt;FunctionCall&gt;
    ///     </code>
    /// </summary>
    and SubExpressionParser: DeferredParser<Expression> = fun () ->
        Any [
            // <SubExpression> ::= "im" <SubExpression>
            ((Accept TokenType.Im) |> IgnoreThen <| (Deferred SubExpressionParser)) |> Map (
                fun e -> Expression.UnaryOperation (e, UnaryOperator.GetImaginary)
            )

            // <SubExpression> ::= "re" <SubExpression>
            ((Accept TokenType.Re) |> IgnoreThen <| (Deferred SubExpressionParser)) |> Map (
                fun e -> Expression.UnaryOperation (e, UnaryOperator.GetReal)
            )

            // <SubExpression> ::= <FunctionCall>
            (Deferred FunctionCallParser)

            // <SubExpression> ::= "(" <Expression> ")"
            (Accept TokenType.LeftParenthesis  |> IgnoreThen <|
            (Deferred ExpressionParser))       |> ThenIgnore <|
            (Accept TokenType.RightParenthesis)

            // <SubExpression> ::= "|" <Expression> "|"
            (Accept TokenType.Bar        |> IgnoreThen <|
            (Deferred ExpressionParser)) |> ThenIgnore <|
            (Accept TokenType.Bar) |> Map (fun e -> Expression.UnaryOperation (e, UnaryOperator.Absolute))

            // <SubExpression> ::= <Variable>
            (VariableParser |> Map Expression.Variable)

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
        // <FunctionCall> ::= <Variable> ...
        //                 |  <Symbol>   ...
        Any [
            (Accept TokenType.Symbol)   |> Map (fun t -> FunctionReferenceType.OfSymbol(t.lexeme))
            (Accept TokenType.Variable) |> Map (GetVariableValue >> FunctionReferenceType.OfVariable)
        ] |> Then <|
        // <FunctionCall> ::= ... "(" <Args> ")"
        (
            (Accept TokenType.LeftParenthesis)  |> IgnoreThen <|
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
            ((Deferred ExpressionParser) |> Then <| (
                (Accept TokenType.Comma) |> IgnoreThen <| (Deferred ArgsParser)
            )) |> Map (fun (arg, args) -> arg :: args)

            // <Args> ::= <Expression>
            ((Deferred ExpressionParser) |> Map (fun e -> [e]))
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> assignment statement.</p>
    ///     <code>
    ///         &lt;Assignment&gt; ::= &lt;Variable&gt; "=" &lt;Expression&gt;
    ///     </code>
    /// </summary>
    let AssignmentParser: Parser<ASTNode> =
        (VariableParser |> Then <| (
            (Accept TokenType.Equals) |> IgnoreThen <| (Deferred ExpressionParser)
        )) |> Map ASTNode.Assignment

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> function result.</p>
    ///     <code>
    ///         &lt;FunctionResult&gt; ::= &lt;Expression&gt;
    ///                           |  "error" &lt;DQString&gt;
    ///                           |  "error"
    ///     </code>
    /// </summary>
    let FunctionResultParser: Parser<FunctionResult> =
        Any [
            // <FunctionResult> ::= "error" <DQString>
            (((Accept TokenType.Error) |> IgnoreThen <| (Accept TokenType.StringLiteral)) |> Map (
                fun lit -> lit.lexeme |> (Some >> FunctionResult.Error)
            ))

            // <FunctionResult> ::= "error"
            ((Accept TokenType.Error) |> Map (fun _ -> None |> FunctionResult.Error))

            // <FunctionResult> ::= <Expression>
            ((Deferred ExpressionParser) |> Map FunctionResult.Expression)
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> condition operator.</p>
    ///     <code>
    ///         &lt;ComparisonOperator&gt; ::= "="
    ///                               |  "!="
    ///                               |  "&lt;"
    ///                               |  "&lt;="
    ///                               |  ">"
    ///                               |  ">="
    ///     </code>
    /// </summary>
    let ComparisonOperatorParser: Parser<ComparisonOperator> =
        Any [
            (Accept TokenType.Equals)             |> As ComparisonOperator.Equality
            (Accept TokenType.NotEqual)           |> As ComparisonOperator.Inequality
            (Accept TokenType.LessThan)           |> As ComparisonOperator.StrictLessThan
            (Accept TokenType.LessThanOrEqual)    |> As ComparisonOperator.NonStrictLessThan
            (Accept TokenType.GreaterThan)        |> As ComparisonOperator.StrictGreaterThan
            (Accept TokenType.GreaterThanOrEqual) |> As ComparisonOperator.NonStrictGreaterThan
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> comparison operation.</p>
    ///     <code>
    ///         &lt;ComparisonOperation&gt; ::= &lt;Expression&gt; &lt;ComparisonOperator&gt; &lt;Expression&gt;
    ///     </code>
    /// </summary>
    let ComparisonOperationParser: Parser<ComparisonOperation> =
        (Deferred ExpressionParser) |> Then <| ComparisonOperatorParser |> Then <| (Deferred ExpressionParser)
        |> Map (fun ((lhs, op), rhs) -> (lhs, op, rhs))

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> piecewise if condition.</p>
    ///     <code>
    ///         &lt;PiecewiseIf&gt; ::= &lt;Expression&gt; "if" &lt;ComparisonOperation&gt;
    ///     </code>
    /// </summary>
    let PiecewiseIfParser: Parser<PiecewiseCondition> =
        (FunctionResultParser |> ThenIgnore <| (Accept TokenType.If)) |> Then <| ComparisonOperationParser

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> piecewise otherwise condition.</p>
    ///     <code>
    ///         &lt;PiecewiseOtherwise&gt; ::= &lt;Expression&gt; "otherwise"
    ///     </code>
    /// </summary>
    let PiecewiseOtherwiseParser: Parser<PiecewiseCondition> =
        (FunctionResultParser |> ThenIgnore <| (Accept TokenType.Otherwise)) |> Map PiecewiseBaseCase

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> piecewise operation sequence.</p>
    ///     <code>
    ///         &lt;PiecewiseConditions&gt; ::= &lt;PiecewiseIf&gt; ";" &lt;PiecewiseConditions&gt;
    ///                                |  &lt;PiecewiseIf&gt; ";" &lt;PiecewiseOtherwise&gt;  ";"
    ///     </code>
    /// </summary>
    let rec PiecewiseConditionsParser: DeferredParser<PiecewiseCondition list> = fun () ->
        // <PiecewiseConditions> ::= <PiecewiseIf> ";" ...
        (PiecewiseIfParser |> ThenIgnore <| EOF) |> Then <| (
            Any [
                // <PiecewiseConditions> ::= ... <PiecewiseOtherwise> ";"
                ((PiecewiseOtherwiseParser |> ThenIgnore <| EOF) |> Map (
                    fun pother -> [pother])
                )
                // <PiecewiseConditions> ::= ... <PiecewiseConditions>
                (Deferred PiecewiseConditionsParser)
            ]
        ) |> Map (fun (pif, ps) -> pif :: ps)

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> function body.</p>
    ///     <code>
    ///         &lt;FunctionBody&gt; ::= &lt;Expression&gt; ";"
    ///                         |  "{" &lt;PiecewiseConditions&gt; "}"
    ///     </code>
    /// </summary>
    let FunctionBodyParser: Parser<FunctionBody> =
        Any [
            // <FunctionBody> ::= <Expression> ";"
            ((Deferred ExpressionParser) |> ThenIgnore <| EOF) |> Map FunctionBody.Expression
    
            // <FunctionBody> ::= "{" <PiecewiseConditions> "}"
            ((Accept TokenType.LeftBrace)            |> IgnoreThen <|
                (Deferred PiecewiseConditionsParser) |> ThenIgnore <|
                    (Accept TokenType.RightBrace)
            ) |> Map FunctionBody.PiecewiseConditions
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
            (VariableParser |> Then <|
                ((Accept TokenType.Colon) |> IgnoreThen <| NumberSetParser)
            )

            // <Parameter> ::= <Letter>
            (VariableParser |> Map (fun v -> (v, DefaultNumberSet)))
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a sequence of <b>Diorite</b> parameters.</p>
    ///     <code>
    ///         &lt;Parameters&gt; ::= &lt;Parameter&gt; "," &lt;Parameters&gt;
    ///                       |  &lt;Parameter&gt;
    ///     </code>
    /// </summary>
    let rec ParametersParser: DeferredParser<FunctionParameter list> = fun () ->
        Any [
            // <Parameters> ::= <Parameter> "," <Parameters>
            (ParameterParser |> Then <| (
                (Accept TokenType.Comma) |> IgnoreThen <| (Deferred ParametersParser)
            )) |> Map (fun (p, ps) -> p :: ps)

            // <Parameters> ::= <Parameter>
            (ParameterParser |> Map (fun p -> [p]))
        ]

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts an optional return set for a <b>Diorite</b> function.</p>
    ///     <code>
    ///         &lt;ReturnSet&gt; ::= ε
    ///                      |  "->" &lt;NumberSet&gt;
    ///     </code>
    ///     <p><i>This parser returns <c>Syntax.DefaultNumberSet</c> if no match is found</i></p>
    /// </summary>
    let ReturnSetParser: Parser<NumberSet> =
        Optional ((Accept TokenType.Arrow) |> IgnoreThen <| NumberSetParser) |> Map (
            fun ns ->
                match ns with
                 | Some ns -> ns
                 | None    -> DefaultNumberSet
        )

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts an optional sequence of function metadata attributes for a <b>Diorite</b>
    ///        function.
    ///     </p>
    ///     <code>
    ///         &lt;FunctionMeta&gt; ::= "[" "symbol" ":" &lt;Letters&gt; "]" &lt;FunctionMeta&gt;
    ///                         |  "[" "inline"               "]" &lt;FunctionMeta&gt;
    ///                         |  "[" "memoized"             "]" &lt;FunctionMeta&gt;
    ///     </code>
    /// </summary>
    let FunctionMetaParser: Parser<FunctionMetadata> =
        ZeroOrMore (
            (Accept TokenType.LeftBracket) |> IgnoreThen <|
            (Accept TokenType.Symbol) |> Bind <| (fun sym ->
                match sym.lexeme with
                 | "symbol"   -> ((Accept TokenType.Colon) |> IgnoreThen <| (Accept TokenType.Symbol)) |> Map (
                                     fun sym -> {
                                         symbol   = Some sym.lexeme
                                         inlined  = false
                                         memoized = false
                                     }
                                 )
                 | "inlined"  -> OfParser {
                                     symbol   = DefaultFunctionMetadata.symbol
                                     inlined  = true
                                     memoized = DefaultFunctionMetadata.memoized
                                 }
                 | "memoized" -> OfParser {
                                     symbol   = DefaultFunctionMetadata.symbol
                                     inlined  = DefaultFunctionMetadata.inlined
                                     memoized = true
                                 }
                 | unknown -> Fail $"Unknown function meta attribute \"{unknown}\""
            )|> ThenIgnore <|
            (Accept TokenType.RightBracket)
        ) |> Map (List.fold (fun accum elem -> {
                 // since all values are separate applications of function metadata - they have to be unionised
                 symbol   = match accum.symbol with Some sym -> Some sym | None -> elem.symbol
                 inlined  = if accum.inlined  then accum.inlined  else elem.inlined
                 memoized = if accum.memoized then accum.memoized else elem.memoized
             }
        ) DefaultFunctionMetadata)

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> function head.</p>
    ///     <code>
    ///         &lt;FunctionHead&gt; ::= &lt;Variable&gt; "(" &lt;Parameters&gt; ")" &lt;ReturnSet&gt;
    ///     </code>
    /// </summary>
    let FunctionHeadParser: Parser<FunctionAttributes> =
        ((FunctionMetaParser |> Then <| VariableParser) |> Then <|
         (
          ((Accept TokenType.LeftParenthesis)  |> IgnoreThen <| (Deferred ParametersParser)) |> Then <|
          ((Accept TokenType.RightParenthesis) |> IgnoreThen <| ReturnSetParser)
         )
        ) |> Map (fun ((meta, fnId), (domain, range)) -> {
              identifier = fnId
              parameters = domain
              range      = range
              metadata   = meta
          }
        )

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> function definition</p>
    ///     <code>
    ///         &lt;FunctionDefinition&gt; ::= &lt;FunctionHead&gt; "=" &lt;FunctionBody&gt;
    ///     </code>
    /// </summary>
    let FunctionDefinitionParser: Parser<ASTNode> =
        (FunctionHeadParser |> Then <| (Accept TokenType.Equals |> IgnoreThen <| FunctionBodyParser)) |> Map
            ASTNode.FunctionDefinition

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> plot expression statement.</p>
    ///     <code>
    ///         &lt;PlotExpression&gt; ::= "plot" &lt;Expression&gt;
    ///     </code>
    /// </summary>
    let PlotExpressionParser: Parser<ASTNode> =
        ((Accept TokenType.Plot) |> IgnoreThen <| (Deferred ExpressionParser)) |> Map ASTNode.PlotFunction

    /// <summary>
    ///     <p>The <c>Parser</c> that accepts a <b>Diorite</b> statement.</p>
    ///     <code>
    ///         &lt;Statement&gt; ::= ε
    ///                      |  ";"                      &lt;Statement&gt;
    ///                      |  &lt;Expression&gt;         ";" &lt;Statement&gt;
    ///                      |  &lt;PlotExpression&gt;     ";" &lt;Statement&gt;
    ///                      |  &lt;Assignment&gt;         ";" &lt;Statement&gt;
    ///                      |  &lt;FunctionDefinition&gt;     &lt;Statement&gt;
    ///     </code>
    /// </summary>
    let rec StatementParser: DeferredParser<AST> = fun () ->
        (Any [
             (IfEmpty                                                   (fun _ -> None))
             (EOF                                                |> Map (fun _ -> None))
             (FunctionDefinitionParser                           |> Map Some)
             ((AssignmentParser            |> ThenIgnore <| EOF) |> Map Some)
             ((PlotExpressionParser        |> ThenIgnore <| EOF) |> Map Some)
             (((Deferred ExpressionParser) |> ThenIgnore <| EOF) |> Map (ASTNode.Expression >> Some))
         ] |> Bind <| (
            fun s ->
                // only retry statement parse if more tokens available
                Any [
                    IfEmpty                           (fun _  -> match s with Some s -> [s]     | None -> [])
                    (Deferred StatementParser) |> Map (fun ss -> match s with Some s -> s :: ss | None -> ss)
                ]
        ))

    /// <summary>
    ///     <p>Parses the supplied <c>TokenStream</c>, and returns the error status. This may be a successful parse,
    ///        which contains the resulting Abstract Syntax Tree (AST), or the relevant error message if the syntax is
    ///        invalid.
    ///     </p>
    /// </summary>
    /// <param name="tokens"> the <c>TokenStream</c> to parse </param>
    /// <returns> a <c>Result</c>, which may, or may not, contain the resulting AST </returns>
    let ParseTokens (tokens: Lexer.TokenStream): Result<AST> =
        match (Deferred StatementParser) tokens with
         | Ok    (root, []           ) -> Ok root
         | Ok    (_,    trailing :: _) ->
             ($"Unexpected trailing \"{trailing.lexeme}\" token", Some (trailing.line, trailing.column))
             ||> SyntaxError
         | Error err                   -> Error err

    /// <summary>
    ///     <p>Parses the supplied <c>string</c>, which is interpreted as <b>Diorite</b> source code. It returns the
    ///        error status. This may be a successful parse, which contains the resulting Abstract Syntax Tree (AST), or
    ///        the relevant error message if the syntax is invalid.
    ///     </p>
    /// </summary>
    /// <param name="str"> the <b>Diorite</b> source code to parse </param>
    /// <returns> a <c>Result</c>, which may, or may not, contain the resulting AST </returns>
    let ParseString (str: string): Result<AST> =
        let rec ListErrors (errors: DioriteError list): string =
            match errors with
             | []           -> ""
             | [err]        -> StrError err
             | head :: tail -> $"{StrError head}; {ListErrors tail}"

        let tokens: Lexer.TokenStream = Lexer.Tokenise str
        match (Lexer.GetErrors tokens) with
         | []     -> ParseTokens tokens
         | errors -> (errors |> ListErrors, None) ||> SyntaxError
