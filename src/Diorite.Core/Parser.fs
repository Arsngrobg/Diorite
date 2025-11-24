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
// Version: v1.8
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

[<RequireQualifiedAccess>]
module Parser =
    type BinaryOperator =
        | Assignment
        | Addition
        | Subtraction
        | Multiplication
        | Division
        | FloorDivision
        | Modulo
        | Exponent

    type UnaryOperator =
        | Positive
        | Negative
        | Percentage
        | Factorial
        | Absolute

    type ComparisonOperator =
        | EqualTo
        | NotEqualTo
        | LessThan
        | GreaterThan
        | LessThanOrEqualTo
        | GreaterThanOrEqualTo

    type AST =
        | Begin              of AST list
        | Value              of ValueType
        | Variable           of VariableType
        | BinaryOperation    of AST * BinaryOperator * AST
        | UnaryOperation     of AST * UnaryOperator
        | FunctionDefinition of FunctionAttributes * AST
        | Comparison         of AST * (AST * ComparisonOperator * AST)
        | FunctionCall       of FunctionName * AST list

    type ParseState<'a> = 'a * Lexer.TokenStream

    type Parser<'a> = Lexer.TokenStream -> ParseState<'a> Result

    let (=>) (left: ParseState<'a> Result) (right: ): ParseState<'b>

    let consume (tokens: Lexer.TokenStream) (expected: Lexer.TokenType): ParseState<Lexer.Token> Result =
        match tokens with
         | token :: tail when token.id = expected -> Ok (token, tail)
         | token :: _                             -> SyntaxError $"Expected {expected} - got \"{token.lexeme}\""
         | []                                     -> SyntaxError $"Expected {expected}"

    // bind functionality for ParseStates
    let (>>=) (state: ParseState<'a> Result) (fn: ParseState<'a> -> ParseState<'b> Result): ParseState<'b> Result =
        match state with
         | Error err  -> Error err
         | Ok    node -> fn node
    
    let parse (tokens: Lexer.TokenStream): AST Result =
        // <source> ::= ε
        //           |  <statement> <source>
        let rec source: Parser<AST list> = (fun tokens ->
            match tokens with
             // <source> ::= ε
             | [] -> Ok ([], [])
             // <source> ::= <statement> <source>
             | tokens ->
                    statement tokens >>= (fun (statementNode, sourceTail) ->
                        source sourceTail >>= (fun (statementNodes, remaining) ->
                            let nodes: AST list =
                                match statementNode with
                                 | Some statementNode -> statementNode :: statementNodes
                                 | None               -> statementNodes
                            Ok (nodes, remaining)
                        )
                    )
        )
        // <statement> ::= ";"
        //              |  <expression> ";"
        //              |  <variable>           "=" <expression> ";"
        //              |  <functionDefinition> "=" <functionBody>
        and statement: Parser<AST option> = (fun tokens ->
            match tokens with
             // <statement> ::= ";"
             | {id = Lexer.SemiColon} :: remaining -> Ok (None, remaining)
             // <statement> ::= <expression> ";"
             //              |  <variable>           "=" <expression> ";"
             //              |  <functionDefinition> "=" <functionBody>
             | tokens ->
                 if (Lexer.statementContainsToken tokens) Lexer.Equals then
                     match tokens with
                      // <expression> ::= <functionDefinition> "=" <functionBody>
                      | {id = Lexer.Variable _} :: {id = Lexer.LeftParenthesis} :: _ ->
                          functionDefinition tokens >>= (fun (defNode, statementTail) ->
                              (consume statementTail Lexer.Equals) >>= (fun (_, statementTail) ->
                                  Ok ((Some << AST.Value << ValueType.Number) <| 0, statementTail)
                              )
                          )
                      // <expression> ::= <variable> "=" <expression> ";"
                      | {id = Lexer.Variable variable} :: statementTail ->
                          (consume statementTail Lexer.Equals) >>= (fun (_, statementTail) ->
                              expression statementTail >>= (fun (node, statementTail) ->
                                  consume statementTail Lexer.SemiColon >>= (fun (_, remaining) ->
                                      Ok ((AST.BinaryOperation >> Some) (
                                          AST.Variable variable, BinaryOperator.Assignment, node
                                         ),
                                      remaining)
                                  )
                              )
                          )
                      | token :: _ -> SyntaxError $"Expected variable identifier - got \"{token.lexeme}\""
                      | []         -> SyntaxError  "Expected variable identifier"
                 else
                     // <statement> ::= <expression> ";"
                     expression tokens >>= (fun (node, statementTail) ->
                         (consume statementTail Lexer.SemiColon) >>= (fun (_, remaining) ->
                            Ok (Some node, remaining)
                         )
                     )
        )
        // <expression> ::= <term> <expression'>
        and expression: Parser<AST> = (fun tokens ->
            // <expression'> ::= ε
            //                |  "+" <term> <expression'>
            //                |  "-" <term> <expression'>
            let rec expression' (accumulator: AST): Parser<AST> = (fun tokens ->
                match tokens with
                 // <expression'> ::= "+" <term> <expression'>
                 | {id = Lexer.Plus} :: expressionTail ->
                     term expressionTail >>= (fun (termNode, expressionTail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Addition, termNode)
                         (expression' accumulator) expressionTail
                     )
                 // <expression'> ::= "-" <term> <expression'>
                 | {id = Lexer.Hyphen} :: expressionTail ->
                     term expressionTail >>= (fun (termNode, expressionTail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Subtraction, termNode)
                         (expression' accumulator) expressionTail
                     )
                 // <expression'> ::= ε
                 | remaining -> Ok (accumulator, remaining)
            )

            // either will produce a singular term node or a term +/- [sub]expression
            term tokens >>= (fun (termNode, expressionTail) ->
                (expression' termNode) expressionTail
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
                 | {id = Lexer.Asterisk} :: termTail ->
                     term termTail >>= (fun (termNode, termTail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Multiplication, termNode)
                         (term' accumulator) termTail
                     )
                 // <term'> ::= "/" <factor> <term'>
                 | {id = Lexer.ForwardSlash} :: termTail ->
                     term termTail >>= (fun (termNode, termTail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Division, termNode)
                         (term' accumulator) termTail
                     )
                 // <term'> ::= "//" <factor> <term'>
                 | {id = Lexer.DoubleForwardSlash} :: termTail ->
                     term termTail >>= (fun (termNode, termTail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.FloorDivision, termNode)
                         (term' accumulator) termTail
                     )
                 // <term'> ::= "%" <factor> <term'>
                 | {id = Lexer.Percentage} :: termTail ->
                     term termTail >>= (fun (termNode, termTail) ->
                         let accumulator: AST = AST.BinaryOperation (accumulator, BinaryOperator.Modulo, termNode)
                         (term' accumulator) termTail
                     )
                 // <term'> ::= ε
                 | remaining -> Ok (accumulator, remaining)
            )

            // either will produce a singular term node or a term +/[/]/[//]/% [sub]expression
            factor tokens >>= (fun (factorNode, expressionTail) ->
                (term' factorNode) expressionTail
            )
        )
        // <factor> ::= <signed>
        //           |  <signed> "^" <signed>
        and factor: Parser<AST> = (fun tokens ->
            signed tokens >>= (fun (leftNode, factorTail) ->
                match factorTail with
                 // <factor> ::= <signed> "^" <signed>
                 | {id = Lexer.Hat} :: factorTail ->
                     signed factorTail >>= (fun (rightNode, remaining) ->
                         Ok (AST.BinaryOperation (leftNode, BinaryOperator.Exponent, rightNode), remaining)
                     )
                 // <factor> ::= <signed>
                 | remaining -> Ok (leftNode, remaining)
            )
        )
        // <signed> ::= <subExpression>
        //           |  "+" <signed>
        //           |  "-" <signed>
        and signed: Parser<AST> = (fun tokens ->
            match tokens with
             // <signed> ::= "+" <subExpression>
             | {id = Lexer.Plus} :: signedTail ->
                 signed signedTail >>= (fun (node, remaining) ->
                     Ok (AST.UnaryOperation (node, UnaryOperator.Positive), remaining)
                 )
             // <signed> ::= "-" <subExpression>
             | {id = Lexer.Hyphen} :: signedTail ->
                 signed signedTail >>= (fun (node, remaining) ->
                     Ok (AST.UnaryOperation (node, UnaryOperator.Negative), remaining)
                 )
             // <signed> ::= <subExpression>
             | tokens -> subExpression tokens
        )
        // <subExpression> ::= <value>
        //                  |  <variable>
        //                  |  "(" <expression> ")"
        //                  |  "|" <expression> "|"
        //                  |  <variable> "(" <functionArgs> ")"
        //                  |  <letters>  "(" <functionArgs> ")"
        and subExpression: Parser<AST> = (fun tokens ->
            match tokens with
             // <subExpression> ::= <variable>
             //                  |  <variable> "(" <functionArgs> ")"
             | {id = Lexer.Variable variable} :: subExpressionTail ->
                 match subExpressionTail with
                  // <subExpression> ::= <variable> "(" <functionArgs> ")"
                  | {id = Lexer.LeftParenthesis} :: subExpressionTail ->
                      functionArgs subExpressionTail >>= (fun (args, subExpressionTail) ->
                          (consume subExpressionTail Lexer.RightParenthesis) >>= (fun (_, remaining) ->
                              Ok (AST.FunctionCall (FunctionName.Variable variable, args), remaining)
                          )
                      )
                  // <subExpression> ::= <variable>
                  | remaining -> Ok (AST.Variable variable, remaining)
             // <subExpression> ::= <letters> "(" <functionArgs> ")"
             | {id = Lexer.Symbol} as sym :: {id = Lexer.LeftParenthesis} :: subExpressionTail ->
                  (functionArgs subExpressionTail) >>= (fun (args, subExpressionTail) ->
                      (consume subExpressionTail Lexer.RightParenthesis) >>= (fun (_, remaining) ->
                          Ok (AST.FunctionCall (FunctionName.Symbolic sym.lexeme, args), remaining)
                      )
                  )
             // <subExpression> ::= "(" <expression> ")"
             | {id = Lexer.LeftParenthesis} :: subExpressionTail ->
                 expression subExpressionTail >>= (fun (node, subExpressionTail) ->
                     (consume subExpressionTail Lexer.RightParenthesis) >>= (fun (_, remaining) ->
                         Ok (node, remaining)
                     )
                 )
             // <subExpression> ::= "|" <expression> "|"
             | {id = Lexer.Bar} :: subExpressionTail ->
                 expression subExpressionTail >>= (fun (node, subExpressionTail) ->
                     (consume subExpressionTail Lexer.Bar) >>= (fun (_, remaining) ->
                         Ok (AST.UnaryOperation (node, UnaryOperator.Absolute), remaining)
                     )
                 )
             // <subExpression> ::= <value>
             | tokens -> value tokens
        )
        // <functionDefinition> ::= <functionMetadata> <variable> "(" <functionParams> ")" <functionRange>
        and functionDefinition: Parser<AST> = (fun tokens ->
            match tokens with
             | {id = Lexer.Variable variable} :: functionDefinitionTail ->
                 (consume functionDefinitionTail Lexer.LeftParenthesis) >>= (fun (_, functionDefinitionTail) ->
                     (functionParams functionDefinitionTail) >>= (fun (paramz, functionDefinitionTail) ->
                         (consume functionDefinitionTail Lexer.RightParenthesis) >>= (fun (_, remaining) ->
                               let attributes: FunctionAttributes = {
                                   identifier = variable
                                   parameters = paramz
                                   returns    = Real
                               }
                               Ok (AST.FunctionDefinition (attributes, AST.Value <| ValueType.Undefined), remaining)
                         )
                     )
                 )
             | token :: _ -> SyntaxError $"Expected Variable for function definition - got \"{token.lexeme}\""
             | []         -> SyntaxError  "Expected Variable for function definition"
        )
        // <functionParams> ::= <functionParam>
        //                   |  <functionParam> "," <functionParams>
        and functionParams: Parser<ParameterType list> = (fun tokens ->
            functionParam tokens >>= (fun (param, functionParamsTail) ->
                match functionParamsTail with
                 | {id = Lexer.Comma} :: functionParamsTail ->
                     functionParams functionParamsTail >>= (fun (paramz, remaining) ->
                         Ok (param :: paramz, remaining)
                     )
                 | remaining -> Ok ([param], remaining)
            )
        )
        // <functionParam> ::= <variable>
        //                  |  <variable> ":" <numberSet>
        and functionParam: Parser<ParameterType> = (fun tokens ->
            match tokens with
             | {id = Lexer.Variable variable} :: functionParamTail ->
                 match functionParamTail with
                  // <functionParam> ::= <variable> ":" <numberSet>
                  | {id = Lexer.Colon} :: {id = Lexer.Variable numberSet} :: remaining ->
                      match numberSet with
                       | 'N', 0uy -> Ok ((variable, NumberSet.Natural   ), remaining)
                       | 'Z', 0uy -> Ok ((variable, NumberSet.Integer   ), remaining)
                       | 'R', 0uy -> Ok ((variable, NumberSet.Real      ), remaining)
                       | 'Q', 0uy -> Ok ((variable, NumberSet.Rational  ), remaining)
                       | 'I', 0uy -> Ok ((variable, NumberSet.Irrational), remaining)
                       | 'C', 0uy -> Ok ((variable, NumberSet.Complex   ), remaining)
                       | variable   -> SyntaxError $"Expected number set - got {variable |> strVariable}"
                  // <functionParam> ::= <variable>
                  | remaining -> Ok ((variable, NumberSet.Real), remaining)
             | token :: _ -> SyntaxError $"Expected variable identifier for function parameter - got \"{token.lexeme}\""
             | []         -> SyntaxError  "Expected variable identifier for function parameter"
        )
        // <functionArgs> ::= <expression>
        //                 |  <expression> "," <functionArgs>
        and functionArgs: Parser<AST list> = (fun tokens ->
            expression tokens >>= (fun (arg, functionArgsTail) ->
                match functionArgsTail with
                 // <functionArgs> ::= <expression> "," <functionArgs>
                 | {id = Lexer.Comma} :: functionArgsTail ->
                     functionArgs functionArgsTail >>= (fun (args, remaining) ->
                         Ok (arg :: args, remaining)
                     )
                 // <functionArgs> ::= <expression>
                 | remaining -> Ok ([arg], remaining)
            )
        )
        // <value> ::= "undefined"
        //          |  "infinity" | "inf"
        //          |  "pi"
        //          |  "tau"
        //          |  "euler"
        //          |  <number>
        and value: Parser<AST> = (fun tokens ->
            match tokens with
             | {id = Lexer.Undefined} :: remaining -> Ok (ValueType.Undefined  |> AST.Value, remaining)
             | {id = Lexer.Infinity } :: remaining -> Ok (ValueType.Infinity   |> AST.Value, remaining)
             | {id = Lexer.Pi       } :: remaining -> Ok ((ValueType.Number 3) |> AST.Value, remaining)
             | {id = Lexer.Tau      } :: remaining -> Ok ((ValueType.Number 6) |> AST.Value, remaining)
             | {id = Lexer.Euler    } :: remaining -> Ok ((ValueType.Number 2) |> AST.Value, remaining)
             | {id = Lexer.Number n } :: remaining -> Ok ((ValueType.Number n) |> AST.Value, remaining)
             | head                   :: _         -> SyntaxError $"Expected value - got \"{head.lexeme}\" instead"
             | []                                  -> SyntaxError  "Expected value"
        )
        
        match source tokens with
         | Error err             -> Error err
         | Ok (nodes, []       ) -> (AST.Begin >> Ok) nodes
         | Ok (_,     head :: _) -> SyntaxError $"Unexpected trailing token {head.column} ({head.id})"
