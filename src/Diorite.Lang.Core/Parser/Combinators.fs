// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Combinators.fs
// Summary: The custom parser combinators
// Author:  Arsngrobg
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Parser

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Parser.DSL

/// <summary>
///     <p>The module containing the custom parsers defined using the Domain Specific Language (DSL).</p>
/// </summary>
module Combinators =
    /// <summary>
    ///     <p>A submodule containing all the helper functions for the parser combinators.</p>
    /// </summary>
    [<AutoOpen>]
    module Helpers =
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
        ///     <p>If the <c>Token</c> does not contain a value, or does not hold a <c>TokenValue.Variable</c>, then
        ///        this function will throw a fatal error.
        ///     </p>
        /// </summary>
        /// <param name="token"> the <c>Token</c> to extract a variable value from </param>
        /// <returns> the variable value held by this token </returns>
        let GetVariableValue (token: Token): VariableType =
            match token.value with
             | TokenValue.Variable v -> v
             | _                     -> failwith $"Parser.GetVariableValue - getting Variable value from {token.id}"

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
    ///                         |  "complex" "(" &lt;Expression&gt; "," &lt;Expression&gt; ")"
    ///                         |  &lt;FunctionCall&gt;
    ///     </code>
    /// </summary>
    and SubExpressionParser: DeferredParser<Expression> = fun () ->
        Any [
            // <SubExpression> ::= "complex" "(" <Expression> "," <Expression> ")"
            ((Accept TokenType.Complex) |> IgnoreThen <| (
                (Accept TokenType.LeftParenthesis) |> IgnoreThen <| (Deferred ExpressionParser)
            ) |> Then <| (
                (Accept TokenType.Comma) |> IgnoreThen <| (
                    (Deferred ExpressionParser) |> ThenIgnore <| (Accept TokenType.RightParenthesis)
                )
            )) |> Map (fun (a, b) -> Expression.BinaryOperation (a, BinaryOperator.OfComplex, b))

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
    ///                      |  &lt;Letter&gt; ":" &lt;NumberSet&gt;
    ///     </code>
    /// </summary>
    let ParameterParser: Parser<FunctionParameter> =
        Any [
            // <Parameter> ::= <Letter> ":" <NumberSet>
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
          (
           (Accept TokenType.LeftParenthesis)  |> IgnoreThen <| (Deferred ParametersParser) |> Bind <|
           // check duplicates
           (fun ps ->
               let duplicates: VariableType list =
                   ps
                   |> List.countBy fst
                   |> List.filter (fun (_, c) -> c > 1)
                   |> List.map fst
               
               match duplicates with
                | []         -> OfParser ps
                | duplicates ->
                    let asStr: string =
                        duplicates
                        |> List.map strVariableType
                        |> String.concat ", "
                    Fail $"Duplicate parameter identifiers: [{asStr}]"
           )
          ) |> Then <|
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
    ///                                 |  "plot" &lt;Expression&gt; "against" &lt;Variable&gt;
    ///     </code>
    /// </summary>
    let PlotExpressionParser: Parser<ASTNode> =
        Any [
            // <PlotExpression> ::= "plot" <Expression> "against" <Variable>
            ((Accept TokenType.Plot) |> IgnoreThen <| (Deferred ExpressionParser))
            |> Then <|
            ((Accept TokenType.Using) |> IgnoreThen <| VariableParser)
            |> Map (fun (exp, var) ->
                   ASTNode.PlotFunction {
                       parameter  = var
                       expression = exp
                   }
               )

            // <PlotExpression> ::= "plot" <Expression>
            ((Accept TokenType.Plot) |> IgnoreThen <| (Deferred ExpressionParser))
            |> Map (fun exp ->
                  ASTNode.PlotFunction {
                      parameter  = ('x', 0uy)
                      expression = exp
                  }
              )
        ]

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
             ((AssignmentParser            |> ThenIgnore <| EOF) |> Map Some)
             ((PlotExpressionParser        |> ThenIgnore <| EOF) |> Map Some)
             (((Deferred ExpressionParser) |> ThenIgnore <| EOF) |> Map (ASTNode.Expression >> Some))
             (FunctionDefinitionParser                           |> Map Some)
         ] |> Bind <| (
            fun s ->
                // only retry statement parse if more tokens available
                Any [
                    IfEmpty                           (fun _  -> match s with Some s -> [s]     | None -> [])
                    (Deferred StatementParser) |> Map (fun ss -> match s with Some s -> s :: ss | None -> ss)
                ]
        ))
