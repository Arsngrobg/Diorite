// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Parser.fs
// Summary: The parser for the Diorite mathematics language
// Author:  Arsngrobg, Borngle
// Version: v1.11
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

// typedefs
type private Token       = Lexer.Token
type private TokenStream = Lexer.TokenStream
type private TokenType   = Lexer.TokenType

/// <summary>
///     <p>The <c>Parser</c> module defines the core <c>AST</c> types, including the <b>unary</b>, <b>binary</b>, and
///        <b>comparison</b> operators.
///     </p>
/// </summary>
[<RequireQualifiedAccess>]
module Parser =
    /// <summary>
    ///     <p>A <b>BinaryOperator</b> is an operator which applies to a left and right value.</p>
    ///     <p>The operators are listed in order of precedence, where <c>Addition</c> &amp; <c>Subtraction</c> are equal;
    ///        and <c>Multiplication</c>, <c>Division</c>, <c>FloorDivision</c>, and <c>Modulo</c> are also equal.
    ///     </p>
    /// </summary>
    type BinaryOperator =
        | Addition
        | Subtraction
        | Multiplication
        | Division
        | FloorDivision
        | Modulo
        | Exponent

    /// <summary>
    ///     <p>A <c>UnaryOperator</c> is an operator which binds to a single operand, either left <i>or</i> right.</p>
    ///     <p>The operators are listed in order of precedence, where <c>Positive</c> &amp; <c>Negative</c> are equal.
    ///     </p>
    /// </summary>
    type UnaryOperator =
        | Positive
        | Negative
        | Factorial
        | Absolute

    /// <summary>
    ///     <p>A <c>ComparisonOperator</c> is an operator which applies to a left and right value.</p>
    ///     <p>These a present in conditional functions and allow for case-matching.
    ///     </p>
    /// </summary>
    type ComparisonOperator =
        | EqualTo
        | NotEqualTo
        | LessThan
        | GreaterThan
        | LessThanOrEqualTo
        | GreaterThanOrEqualTo

    /// <summary>
    ///     <p>The <c>AST</c> type is a structured union type for the deterministic structure of a <b>Diorite</b> source
    ///        file or context.
    ///     </p>
    ///     <p>The <c>parse</c> function should always return a root node that is the <c>Begin</c> type.</p>
    /// </summary>
    type AST =
        /// <summary>
        ///     <p>The root node of any successfully parsed <c>TokenStream</c>.</p>
        ///     <p>It contains <c>AST</c> nodes to be evaluated, as <b>Diorite</b> code can have many statements, or
        ///        none at all.
        ///     </p>
        ///     <p><i>It is also used in function comparisons.</i></p>
        /// </summary>
        /// <typeparam name='AST list'> the list of (potentially zero) statements to evaluate </typeparam>
        | NodeSequence       of AST list
        /// <summary>
        ///     <p>The <i>smallest</i> value of any given subtree.</p>
        ///     <p>It is a case wrapper for the <c>ValueType</c> base type.</p>
        /// </summary>
        /// <typeparam name='ValueType'> the type of this value </typeparam>
        | Value              of ValueType
        /// <summary>
        ///     <p>A reference to a <c>Value</c> or <c>FunctionDefinition</c>.</p>
        ///     <p>It is a case wrapper for the <c>VariableType</c> base type.</p>
        /// </summary>
        /// <typeparam name='VariableType'> the variable reference data </typeparam>
        | Variable           of VariableType
        /// <summary>
        ///     <p>An assignment operation in <b>Diorite</b>.</p>
        ///     <p>It is a tuple that holds the <c>VariableType</c> the assignment applies to, and the expression that
        ///        it should evaluate to.
        ///     </p>
        /// </summary>
        /// <typeparam name='VariableType'> the variable this assignment operation targets </typeparam>
        /// <typeparam name='AST'> the intermediate value that should be set to the <c>VariableType</c> </typeparam>
        | Assignment         of VariableType * AST
        /// <summary>
        ///     <p>A structured representation of a <b>binary</b> operation.</p>
        ///     <p>It is a tuple that indicates that the left sub-expression is operated on by the binary operator with
        ///        the right sub-expression.
        ///     </p>
        /// </summary>
        /// <typeparam name='AST'> the left/right sub-expression </typeparam>
        /// <typeparam name='BinaryOperator'> the operator </typeparam>
        | BinaryOperation    of AST * BinaryOperator * AST
        /// <summary>
        ///     <p>A structured representation of a <b>unary</b> operation.</p>
        ///     <p>It is a tuple that indicates that the operand that <b>bind</b>-ed to this <c>UnaryOperator</c> is
        ///        operated on by the operator.
        ///     </p>
        /// </summary>
        /// <typeparam name='AST'> the sub-expression (operand) </typeparam>
        /// <typeparam name='UnaryOperator'> the operator </typeparam>
        | UnaryOperation     of AST * UnaryOperator
        /// <summary>
        ///     <p>A <b>Diorite</b> function definition.</p>
        ///     <p>It is a tuple that holds the <c>FunctionAttributes</c>, and the function body.</p>
        ///     <p>The function body is either a sequence of comparisons, or an expression.</p>
        /// </summary>
        /// <typeparam name='FunctionAttributes'> the attributes of the defined function </typeparam>
        /// <typeparam name='AST'> the function body </typeparam>
        | FunctionDefinition of FunctionAttributes * AST
        /// <summary>
        ///     <p>A structured representation for a comparison in a function composed of case comparisons.</p>
        ///     <p>It is a tuple, consisting of the expression to return if the nested comparison operation evaluates to
        ///        <c>true</c>.
        ///     </p>
        /// </summary>
        /// <typeparam name='AST'> the expression to return (if <c>true</c>)/the left or right expressions </typeparam>
        /// <typeparam name='ComparisonOperator'> the operator to compare with the left/right expressions </typeparam>
        | CaseComparison     of AST * (AST * ComparisonOperator * AST)
        /// <summary>
        ///     <p>A <b>Diorite</b> function call.</p>
        ///     <p>It is a paired tuple that holds the potentially valid reference to a defined <b>Diorite</b> function,
        ///        and the <b>actual parameters</b>.
        ///     </p>
        /// </summary>
        /// <typeparam name='FunctionReference'> the reference to the function </typeparam>
        /// <typeparam name='AST list'> the <b>actual parameters</b> supplied to the function </typeparam>
        | FunctionCall       of FunctionReference * AST list

    /// <summary>
    ///     <p>A function for creating a base case from the supplied <c>expression</c>.</p>
    ///     <p>It produces an <c>AST.Comparison</c> that checks if a dummy value is equal to itself, meaning it is
    ///        always <c>true</c>.
    ///     </p>
    /// </summary>
    /// <param name='expression'> the expression to return if all case comparisons are exhausted </param>
    /// <returns> an <c>AST.CaseComparison</c> node type that is always true (base case) </returns>
    let baseCase (expression: AST): AST =
        let dummy: AST = ValueType.Undefined |> AST.Value
        AST.CaseComparison (expression, (dummy, ComparisonOperator.EqualTo, dummy))

    /// <summary>
    ///     <p>A <c>ParseState</c> represents a particular state in the parsing stage.</p>
    ///     <p>In short, it contains the return value (<c>'a</c>) and the remaining
    ///        <c>TokenStream</c> of a <c>Parser</c>.
    ///     </p>
    /// </summary>
    type ParseState<'a> = 'a * TokenStream

    /// <summary>
    ///     <p>A <i>parser</i> is a component in the parsing stage.</p>
    ///     <p>We can say that the whole parser is composed of smaller <c>Parser</c>s to build an <c>AST</c>.</p>
    ///     <p>This is because each area of the parsing does not always produce an <c>AST</c> node, but a smaller
    ///        piece of the tree, such as a list of function arguments.
    ///     </p>
    /// </summary>
    type Parser<'a> = TokenStream -> ParseState<'a> Result

    // helper function for producing an error message
    let private getErrorMsg (token: Token option) (expected: string): string =
        match token with
         | Some token -> $"Expected {expected} - got \"{token.lexeme}\" at line {token.line}, column {token.column}"
         | None       -> $"Expected {expected}"

    // dumb consumer that gets cranky if it doesn't match the id supplied to it
    let private consume (tokens: TokenStream) (id: Lexer.TokenType): ParseState<Lexer.Token> Result =
        match tokens with
         | token :: remaining when token.id = id -> Ok (token, remaining)
         | head  :: _                            -> SyntaxError (getErrorMsg (Some head) $"{id}")
         | []                                    -> SyntaxError (getErrorMsg None $"{id}")

    [<RequireQualifiedAccess>]
    module private Parsers =
        // <source> ::= ε
        //           |  <statement> <source>
        let rec source: Parser<AST list> = (fun tokens ->
            match tokens with
             // <source> ::= ε
             | []     -> Ok ([], [])
             // <source> ::= <statement> <source>
             | tokens ->
                 statement tokens ?=> (fun (stmt, tail) ->
                     match stmt with
                      | None      -> source tail
                      | Some stmt -> source tail ?=> (fun (stms, remaining) -> Ok (stmt :: stms, remaining))
             )
        )
        // <statement> ::= ";"
        //              |  <expression> ";"
        //              |  <variable>           "=" <expression> ";"
        //              |  <functionDefinition> "=" <functionBody>
        and statement: Parser<AST option> = (fun tokens ->
            // <statement> ::= <variable>           "=" <expression> ";"
            //              |  <functionDefinition> "=" <functionBody>
            if (Lexer.statementContainsToken tokens) TokenType.Equals then
                match tokens with
                 // <statement> ::= <variable> "=" <expression> ";"
                 | {id = TokenType.Variable v} :: {id = TokenType.Equals} :: tail ->
                      expression tail                  ?=> (fun (exp, tail     ) ->
                      consume tail TokenType.SemiColon ?=> (fun (_,   remaining) ->
                          let tree: AST = AST.Assignment (v, exp)
                          Ok (Some tree, remaining)
                      ))
                 // <statement> ::= <functionDefinition> "=" <functionBody>
                 | tokens ->
                      functionDefinition tokens     ?=> (fun (def,  tail     ) ->
                      consume tail TokenType.Equals ?=> (fun (_,    tail     ) ->
                      functionBody tail             ?=> (fun (body, remaining) ->
                          let tree: AST = AST.FunctionDefinition (def, body)
                          Ok (Some tree, remaining)
                      )))
            // <statement> ::= ";"
            //              |  <expression> ";"
            else
                match tokens with
                 // <statement> ::= ";"
                 | {id = TokenType.SemiColon} :: remaining -> Ok (None, remaining)
                 // <statement> ::= <expression> ";"
                 | tokens ->
                     expression tokens                ?=> (fun (exp, tail     ) ->
                     consume tail TokenType.SemiColon ?=> (fun (_,   remaining) ->
                         Ok (Some exp, remaining)
                     ))
        )
        // <expression> ::= <term> <expression'>
        and expression: Parser<AST> = (fun tokens ->
            // <expression'> ::= ε
            //                |  "+" <term> <expression'>
            //                |  "-" <term> <expression'>
            let rec expression' (accumulator: AST): Parser<AST> = (fun tokens ->
                match tokens with
                 // <expression'> ::= "+" <term> <expression'>
                 | {id = TokenType.Plus} :: tail ->
                     term tail ?=> (fun (term, tail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Addition, term)
                         expression' accumulator tail
                     )
                 // <expression'> ::= "+" <term> <expression'>
                 | {id = TokenType.Hyphen} :: tail ->
                     term tail ?=> (fun (term, tail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Subtraction, term)
                         expression' accumulator tail
                     )
                 // <expression'> ::= ε
                 | remaining -> Ok (accumulator, remaining)
            )

            // will either return just the term or a nested tree of addition/subtraction
            term tokens ?=> (fun (term, tail) ->
                expression' term tail
            )
        )
        // <term> ::= <factor> <term'>
        and term: Parser<AST> = (fun tokens ->
            // <term'> ::= ε
            //          |  "*"  <factor> <term'>
            //          |  "/"  <factor> <term'>
            //          |  "//" <factor> <term'>
            //          |  "%"  <factor> <term'>
            let rec term' (accumulator: AST): Parser<AST> = (fun tokens ->
                match tokens with
                 // <term'> ::= "*" <factor> <term'>
                 | {id = TokenType.Asterisk} :: tail ->
                     factor tail ?=> (fun (term, tail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Multiplication, term)
                         term' accumulator tail
                     )
                 // <term'> ::= "/" <factor> <term'>
                 | {id = TokenType.ForwardSlash} :: tail ->
                     factor tail ?=> (fun (factor, tail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Division, factor)
                         term' accumulator tail
                     )
                 // <term'> ::= "//" <factor> <term'>
                 | {id = TokenType.DoubleForwardSlash} :: tail ->
                     factor tail ?=> (fun (factor, tail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.FloorDivision, factor)
                         term' accumulator tail
                     )
                 // <term'> ::= "%" <factor> <term'>
                 | {id = TokenType.Percentage} :: tail ->
                     factor tail ?=> (fun (factor, tail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Modulo, factor)
                         term' accumulator tail
                  )
                 // <term'> ::= ε
                 | remaining -> Ok (accumulator, remaining)
            )

            factor tokens ?=> (fun (factor, tail) ->
                term' factor tail
            )
        )
        // <factor> ::= <signed>
        //           |  <signed> "^" <signed>
        and factor: Parser<AST> = (fun tokens ->
            signed tokens ?=> (fun (leftSigned, tail) ->
                match tail with
                 // <factor> ::= <signed> "^" <signed>
                 | {id = TokenType.Hat} :: tail ->
                     signed tail ?=> (fun (rightSigned, remaining) ->
                         let tree: AST = AST.BinaryOperation (leftSigned, BinaryOperator.Exponent, rightSigned)
                         Ok (tree, remaining)
                     )
                 // <factor> ::= <signed>
                 | remaining -> Ok (leftSigned, remaining)
            )
        )
        // <signed> ::= <subExpression> <factorials>
        //           |  "+" <signed>
        //           |  "-" <signed>
        and signed: Parser<AST> = (fun tokens ->
            match tokens with
             // <signed> ::= "+" <signed>
             | {id = TokenType.Plus} :: tail ->
                 signed tail ?=> (fun (ops, remaining) ->
                     let tree: AST = AST.UnaryOperation (ops, UnaryOperator.Positive)
                     Ok (tree, remaining)
                 )
             // <signed> ::= "-" <signed>
             | {id = TokenType.Hyphen} :: tail ->
                 signed tail ?=> (fun (ops, remaining) ->
                     let tree: AST = AST.UnaryOperation (ops, UnaryOperator.Negative)
                     Ok (tree, remaining)
                 )
             // <signed> ::= <subExpression> <factorials>
             | tokens ->
                 subExpression tokens   ?=> (fun (subExp, tail) ->
                 factorials subExp tail
                 )
        )

        // <factorials> ::= ε
        //               |  "!" <factorials>
        and factorials (accumulator: AST): Parser<AST> = (fun tokens ->
            match tokens with
             | {id = TokenType.Exclamation} :: tail ->
                 let tree: AST = AST.UnaryOperation (accumulator, UnaryOperator.Factorial)
                 factorials tree tail
             | remaining -> Ok (accumulator, remaining)
        )
        // <subExpression> ::= <value>
        //                  |  <variable>
        //                  |  "(" <expression> ")"
        //                  |  "|" <expression> "|"
        //                  |  <variable> "(" <functionArgs> ")"
        //                  |  <letters>  "(" <functionArgs> ")"
        and subExpression: Parser<AST> = (fun tokens ->
            match tokens with
             // <subExpression> ::= "(" <expression> ")"
             | {id = TokenType.LeftParenthesis} :: tail ->
                 expression tail                         ?=> (fun (exp, tail     ) ->
                 consume tail TokenType.RightParenthesis ?=> (fun (_,   remaining) ->
                     Ok (exp, remaining)
                 ))
             // <subExpression> ::= "|" <expression> "|"
             | {id = TokenType.Bar} :: tail ->
                 expression tail            ?=> (fun (exp, tail     ) ->
                 consume tail TokenType.Bar ?=> (fun (_,   remaining) ->
                     let tree: AST = AST.UnaryOperation (exp, UnaryOperator.Absolute)
                     Ok (tree, remaining)
                 ))
             // <subExpression> ::= <variable>
             //                  |  <variable> "(" <functionArgs> ")"
             | {id = TokenType.Variable v} :: tail ->
                 match tail with
                  // <subExpression> ::= <variable> "(" <functionArgs> ")"
                  | {id = TokenType.LeftParenthesis} :: tail ->
                      functionArgs tail                       ?=> (fun (args, tail     ) ->
                      consume tail TokenType.RightParenthesis ?=> (fun (_,    remaining) ->
                          let tree: AST = AST.FunctionCall (FunctionReference.OfVariable v, args)
                          Ok (tree, remaining)
                      ))
                  // <subExpression> ::= <variable>
                  | remaining -> Ok (AST.Variable v, remaining)
             // <subExpression> ::= <letters> "(" <functionArgs> ")"
             | {id = TokenType.Symbol} as t :: tail ->
                  consume tail TokenType.LeftParenthesis  ?=> (fun (_,    tail     ) ->
                 functionArgs tail                        ?=> (fun (args, tail     ) ->
                 consume tail TokenType.RightParenthesis  ?=> (fun (_,    remaining) ->
                    let tree: AST = AST.FunctionCall (FunctionReference.OfSymbolic t.lexeme, args)
                    Ok (tree, remaining)
                 )))
             // <subExpression> ::= <value>
             | tokens -> value tokens ?=> (fun (factor, remaining) ->
                   Ok (AST.Value factor, remaining)
               )
        )
        // <conditions> ::= <ifCondition> ";" <conditions>
        //               |  <ifCondition> ";" <otherwiseCondition> ";"
        and conditions: Parser<AST list> = (fun tokens ->
            ifCondition tokens               ?=> (fun (condition, tail) ->
            consume tail TokenType.SemiColon ?=> (fun (_,         tail) ->
                // <conditions> ::= <ifCondition> ";" <conditions>
                if (Lexer.statementContainsToken tail) TokenType.If then
                    conditions tail ?=> (fun (conditions, remaining) ->
                        Ok (condition :: conditions, remaining)
                    )
                // <conditions> ::= <ifCondition> ";" <otherwiseCondition> ";"
                else
                    otherwiseCondition tail          ?=> (fun (baseCase, tail     ) ->
                    consume tail TokenType.SemiColon ?=> (fun (_,        remaining) ->
                        Ok ([condition; baseCase], remaining)
                    ))
            ))
        )
        // <ifCondition> ::= <expression> "if" <expression> <comparison> <expression>
        and ifCondition: Parser<AST> = (fun tokens ->
            expression tokens         ?=> (fun (ifTrue,   tail     ) ->
            consume tail TokenType.If ?=> (fun (_,        tail     ) ->
            expression tail           ?=> (fun (leftCmp,  tail     ) ->
            comparison tail           ?=> (fun (cmpOp,    tail     ) ->
            expression tail           ?=> (fun (rightCmp, remaining) ->
                let tree: AST = AST.CaseComparison (ifTrue, (leftCmp, cmpOp, rightCmp))
                Ok (tree, remaining)
            )))))
        )
        // <otherwiseCondition> ::= <expression> "otherwise"
        and otherwiseCondition: Parser<AST> = (fun tokens ->
            expression tokens                ?=> (fun (defaultValue, tail     ) ->
            consume tail TokenType.Otherwise ?=> (fun (_,            remaining) ->
                Ok (baseCase defaultValue, remaining)
            ))
        )
        // <comparison> ::= "="
        //               |  "!="
        //               |  ">"
        //               |  "<"
        //               |  ">="
        //               |  "<="
        and comparison: Parser<ComparisonOperator> = (fun tokens ->
            match tokens with
             | {id = TokenType.Equals}             :: rem -> Ok (ComparisonOperator.EqualTo,              rem)
             | {id = TokenType.NotEqual}           :: rem -> Ok (ComparisonOperator.NotEqualTo,           rem)
             | {id = TokenType.GreaterThan}        :: rem -> Ok (ComparisonOperator.GreaterThan,          rem)
             | {id = TokenType.LessThan}           :: rem -> Ok (ComparisonOperator.LessThan,             rem)
             | {id = TokenType.GreaterThanOrEqual} :: rem -> Ok (ComparisonOperator.GreaterThanOrEqualTo, rem)
             | {id = TokenType.LessThanOrEqual   } :: rem -> Ok (ComparisonOperator.LessThanOrEqualTo,    rem)
             | head :: _ -> SyntaxError (getErrorMsg (Some head) "ComparisonOperator")
             | []        -> SyntaxError (getErrorMsg None "ComparisonOperator")
        )
        // <numberSet> ::= "N"  /* natural     */
        //              |  "Z"  /* integer     */
        //              |  "R"  /* reals       */
        //              |  "Q"  /* rationals   */
        //              |  "I"  /* irrationals */
        //              |  "C"  /* complex     */
        and numberSet: Parser<NumberSet> = (fun tokens ->
            match tokens with
             | {id = TokenType.Variable ('N', 0uy)} :: remaining -> Ok (NumberSet.Natural,    remaining)
             | {id = TokenType.Variable ('Z', 0uy)} :: remaining -> Ok (NumberSet.Integer,    remaining)
             | {id = TokenType.Variable ('R', 0uy)} :: remaining -> Ok (NumberSet.Real,       remaining)
             | {id = TokenType.Variable ('Q', 0uy)} :: remaining -> Ok (NumberSet.Rational,   remaining)
             | {id = TokenType.Variable ('I', 0uy)} :: remaining -> Ok (NumberSet.Irrational, remaining)
             | {id = TokenType.Variable ('C', 0uy)} :: remaining -> Ok (NumberSet.Complex,    remaining)
             | {id = TokenType.Variable v} as t :: _ ->
                 SyntaxError $"Expected valid NumberSet - got {strVariable v} at line {t.line}, column {t.column}"
             | head :: _ -> SyntaxError (getErrorMsg (Some head) "NumberSet")
             | []        -> SyntaxError (getErrorMsg None "NumberSet")
        )
        // <variable> ::= <letter>
        //             |  <letter> <digit>
        // (tokeniser already handles distinction)
        and variable: Parser<VariableType> = (fun tokens ->
            match tokens with
             | {id = TokenType.Variable v} :: remaining -> Ok (v, remaining)
             | head :: _ -> SyntaxError (getErrorMsg (Some head) "Variable")
             | []        -> SyntaxError (getErrorMsg None "Variable")
        )
        // <value> ::= "undefined"
        //          |  "infinity" | "inf"
        //          |  "pi"
        //          |  "tau"
        //          |  "euler"
        //          |  <number>
        and value: Parser<ValueType> = (fun tokens ->
            match tokens with
             | {id = TokenType.Undefined} :: remaining -> Ok (ValueType.Undefined,            remaining)
             | {id = TokenType.Infinity}  :: remaining -> Ok (ValueType.Infinity,             remaining)
             | {id = TokenType.Pi}        :: remaining -> Ok (ValueType.Number constantPi,    remaining)
             | {id = TokenType.Tau}       :: remaining -> Ok (ValueType.Number constantTau,   remaining)
             | {id = TokenType.Euler}     :: remaining -> Ok (ValueType.Number constantEuler, remaining)
             | {id = TokenType.Number n}  :: remaining -> Ok (ValueType.Number n,             remaining)
                 | head :: _ -> SyntaxError (getErrorMsg (Some head) "Number, Keyword, or Constant")
                 | []        -> SyntaxError (getErrorMsg None "Number, Keyword, or Constant")
        )
        // <functionDefinition> ::= <functionMetadata> <variable> "(" <functionParams> ")" <functionRange>
        and functionDefinition: Parser<FunctionAttributes> = (fun tokens ->
            functionMetadata defaultFunctionMetadata tokens ?=> (fun (meta,   tail     ) ->
            variable tail                                   ?=> (fun (v,      tail     ) ->
            consume tail TokenType.LeftParenthesis          ?=> (fun (_,      tail     ) ->
            functionParams tail                             ?=> (fun (paramz, tail     ) ->
            consume tail TokenType.RightParenthesis         ?=> (fun (_,      tail     ) ->
            functionRange tail                              ?=> (fun (set,    remaining) ->
                 let attr: FunctionAttributes = {
                     identifier = v
                     parameters = paramz
                     returns    = set
                     metadata   = meta
                 }
                 Ok (attr, remaining)
            ))))))
        )
        // <functionMetadata> ::= ε
        //                     |  "[" "symbol" ":" <letters> "]" <functionMetadata>
        //                     |  "[" "inlined"  "]"             <functionMetadata>
        //                     |  "[" "memoized" "]"             <functionMetadata>
        and functionMetadata (currentMeta: FunctionMetadata): Parser<FunctionMetadata> = (fun tokens ->
            printf "dfg\n"
            match tokens with
            // <functionMetadata> ::= "[" "symbol" ":" <letters> "]" <functionMetadata>
            //                     |  "[" "inlined"  "]"             <functionMetadata>
            //                     |  "[" "memoized" "]"             <functionMetadata>
             | {id = TokenType.LeftBracket} :: tail ->
                 consume tail TokenType.Symbol ?=> (fun (metaAttr, tail) ->
                     match metaAttr.lexeme with
                      // <functionMetadata> ::= "[" "symbol" ":" <letters> "]" <functionMetadata>
                      | "symbol" ->
                          consume tail TokenType.Colon        ?=> (fun (_,   tail) ->
                          consume tail TokenType.Symbol       ?=> (fun (sym, tail) ->
                          consume tail TokenType.RightBracket ?=> (fun (_,   tail) ->
                              let newMeta: FunctionMetadata = {
                                  symbol   = Some sym.lexeme
                                  inlined  = currentMeta.inlined
                                  memoized = currentMeta.memoized
                              }
                              functionMetadata newMeta tail
                          )))
                      // <functionMetadata> ::= "[" "inlined" "]" <functionMetadata>
                      | "inlined" ->
                          consume tail TokenType.RightBracket ?=> (fun (_, tail) ->
                              let newMeta: FunctionMetadata = {
                                  symbol   = currentMeta.symbol
                                  inlined  = true
                                  memoized = currentMeta.memoized
                              }
                              functionMetadata newMeta tail
                          )
                      // <functionMetadata> ::= "[" "memoized" "]" <functionMetadata>
                      | "memoized" ->
                          consume tail TokenType.RightBracket ?=> (fun (_, tail) ->
                              let newMeta: FunctionMetadata = {
                                  symbol   = currentMeta.symbol
                                  inlined  = currentMeta.inlined
                                  memoized = true
                              }
                              functionMetadata newMeta tail
                          )
                      | sym -> SyntaxError $"Invalid function meta attribute \"{sym}\""
                 )
             // <functionMetadata> ::= ε
             | remaining -> Ok (currentMeta, remaining)
        )
        // <functionParams> ::= <functionParam>
        //                   |  <functionParam> "," <functionParams>
        and functionParams: Parser<ParameterType list> = (fun tokens ->
            functionParam tokens ?=> (fun (param, tail) ->
                match tail with
                 // <functionParams> ::= <functionParam> "," <functionParams>
                 | {id = TokenType.Comma} :: tail ->
                     functionParams tail ?=> (fun (paramz, remaining) ->
                         Ok (param :: paramz, remaining)
                     )
                 // <functionParams> ::= <functionParam>
                 | remaining -> Ok ([param], remaining)
            )
        )
        // <functionParam> ::= <variable>
        //                  |  <variable> ":" <numberSet>
        and functionParam: Parser<ParameterType> = (fun tokens ->
            variable tokens ?=> (fun (v, tail) ->
                match tail with
                 // <functionParam> ::= <variable> ":" <numberSet>
                 | {id = TokenType.Colon} :: tail ->
                     numberSet tail ?=> (fun (set, remaining) ->
                         Ok ((v, set), remaining)
                     )
                 // <functionParam> ::= <variable>
                 | remaining -> Ok ((v, NumberSet.Real), remaining)
            )
        )
        // <functionArgs> ::= <expression>
        //                 |  <expression> "," <functionArgs>
        and functionArgs: Parser<AST list> = (fun tokens ->
            expression tokens ?=> (fun (arg, tail) ->
                match tail with
                 // <functionArgs> ::= <expression> "," <functionArgs>
                 | {id = TokenType.Comma} :: tail ->
                     functionArgs tail ?=> (fun (args, remaining) ->
                         Ok (arg :: args, remaining)
                     )
                 // <functionArgs> ::= <expression>
                 | remaining -> Ok ([arg], remaining)
            )
        )
        // <functionRange> ::= ε
        //                  |  "->" <numberSet>
        and functionRange: Parser<NumberSet> = (fun tokens ->
            match tokens with
             // <functionRange> ::= "->" <numberSet>
             | {id = TokenType.Arrow} :: tail -> numberSet tail
             // <functionRange> ::= ε
             | remaining -> Ok (NumberSet.Real, remaining)
        )
        // <functionBody> ::= <expression> ";"
        //                 |  "{" <conditions> "}"
        and functionBody: Parser<AST> = (fun tokens ->
            match tokens with
             // <functionBody> ::= "{" <conditions> "}"
             | {id = TokenType.LeftBrace} :: tail ->
                 conditions tail                   ?=> (fun (conditions, tail     ) ->
                 consume tail TokenType.RightBrace ?=> (fun (_,          remaining) ->
                     Ok (conditions |> AST.NodeSequence, remaining)
                 ))
             // <functionBody> ::= <expression> ";"
             | tail ->
                 expression tail                  ?=> (fun (exp, tail) ->
                 consume tail TokenType.SemiColon ?=> (fun (_, remaining) ->
                     Ok (exp, remaining)
                 ))
        )

    /// <summary>
    ///     <p>Performs lexical analysis on the supplied <c>TokenStream</c>, and returns a <c>Result</c> that may
    ///        contain the resulting <c>AST</c>, or an <c>DioriteError</c>.
    ///     </p>
    /// </summary>
    /// <param name='tokens'> the tokens to analyse </param>
    /// <returns> a <c>Result</c> that may or may not contain a valid tree </returns>
    let parse (tokens: TokenStream): AST Result =
        match Parsers.source tokens with
         | Error err                -> Error err
         | Ok    (_,     head :: _) -> SyntaxError $"Illegal trailing \"{head.lexeme}\""
         | Ok    (nodes, []       ) -> (AST.NodeSequence >> Ok) nodes
