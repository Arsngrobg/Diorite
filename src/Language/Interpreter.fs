// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Interpreter.fs
// Summary: The interpreter of for the Diorite language, which also includes a REPL
// Author:  Arsngrobg
// Version: v1.7
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
///     The interpreter module
/// </summary>
module Interpreter =
    /// <summary>
    ///     The <c>Memory</c> module contains functionality for variable storage, retrieval, and modification.
    /// </summary>
    module Memory =
        // 11 rows (subscripts), and 52 columns (characters)
        let letters: char list = ['a'..'z'] @ ['A'..'Z']
        let table: Parser.AST[,] = Array2D.create 11 letters.Length Parser.AST.Undefined

        /// Simple helper function to find the column index where a character is
        let private findColIndex (character: char): int =
            letters |> List.findIndex ((=) character)

        /// <summary>
        ///     Gets the value of a given variable in the table.
        /// </summary>
        /// <param name='character'> the alphabetical character of the variable </param>
        /// <param name='rowIndex'> the row in the table where the character is, indicating the subscript </param>
        /// <returns> the value stored in the table at the location </returns>
        let get (character: char) (rowIndex: int): Parser.AST =
            let colIndex = findColIndex character
            table[rowIndex, colIndex]

        /// <summary>
        ///     Sets the value of a given variable in the table.
        /// </summary>
        /// <param name='character'> the alphabetical character of the variable </param>
        /// <param name='rowIndex'> the row in the table where the character is, indicating the subscript </param>
        /// <param name='value'> the value being assigned </param>
        let set (character: char) (rowIndex: int) (value: Parser.AST): unit =
            let colIndex = findColIndex character
            table[rowIndex, colIndex] <- value

    // TODO: implement evaluations
    // this works fine when running from the REPL - since the syntax checks occur before this function is called
    // that is why it can return a SystemError and not SyntaxError but might change this
    let rec evalTree (root: Parser.AST): Parser.AST =
        let (|+|) (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity -> Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left + right)

        let (|-|) (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left - right)

        let (|*|) (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity -> Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left * right)

        let (|/|) (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left / right)

        let (|//|) (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (Library.floor(left / right))

        let (|%|) (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left % right)

        let (|^|) (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left ** right)

        let (~+) (operand: Parser.AST): Parser.AST =
            match operand with
             | Parser.Infinity -> Parser.Infinity
             | Parser.Undefined -> Parser.Undefined
             | Parser.Number value -> Parser.Number value

        let (~-) (operand: Parser.AST): Parser.AST =
            match operand with
             | Parser.Infinity -> Parser.Infinity
             | Parser.Undefined -> Parser.Undefined
             | Parser.Number value -> Parser.Number -value

        let rec factorial (operand: Parser.AST): Parser.AST =
            match operand with
             | Parser.Infinity -> Parser.Infinity
             | Parser.Undefined -> Parser.Undefined
             | Parser.Number value -> Parser.Undefined // for now

        let rec applyArgs (parameters: Parser.FunctionParameter list) (args: Parser.AST list): unit =
            for arg, param in List.zip args parameters do
                printf $"{arg}\n"
                match param with
                 | (ch, Some subscript), _ -> Memory.set ch (subscript + 1) (evalTree arg)
                 | (ch, None), _ -> Memory.set ch 0 (evalTree arg)

        // for AST.Begin
        let rec evalNodes (nodes: Parser.AST list): Parser.AST list =
            match nodes with
             | [] -> []
             | head :: tail -> evalTree head :: evalNodes tail

        match root with
         | Parser.Begin nodes -> (evalNodes >> Parser.Begin) nodes
         // values
         | Parser.Number value -> Parser.Number value
         | Parser.Variable (character, maybeSubscript) ->
             match maybeSubscript with
              | Some subscriptNumber -> Memory.get character (subscriptNumber + 1)
              | None                 -> Memory.get character 0
         // reserved words
         | Parser.Infinity  -> Parser.Infinity
         | Parser.Undefined -> Parser.Undefined
         // binary operations
         | Parser.BinaryOperation (left, operator, right) ->
             let right: Parser.AST = evalTree right
             match operator with
              | Parser.Equals ->
                  match left with
                   | Parser.Variable (ch, maybeSubscript) ->
                       match maybeSubscript with
                        | Some subscriptNumber ->
                            Memory.set ch (subscriptNumber + 1) right
                            Parser.BinaryOperation (left, operator, right)
                        | None ->
                            Memory.set ch 0 right
                            Parser.BinaryOperation (left, operator, right)
                    //| node -> SystemError $"Unexpected type for set operation - got {node}"
              | Parser.Addition       -> (evalTree left) |+|  right
              | Parser.Subtraction    -> (evalTree left) |-|  right
              | Parser.Multiplication -> (evalTree left) |*|  right
              | Parser.Division       -> (evalTree left) |/|  right
              | Parser.FloorDivision  -> (evalTree left) |//| right
              | Parser.Modulo         -> (evalTree left) |%|  right
              | Parser.Exponentiation -> (evalTree left) |^|  right
              //| node                  -> SystemError $"Unexpected binary operator - got {node} instead"
         | Parser.UnaryOperation(operand, operator) ->
             let operand: Parser.AST = evalTree operand
             match operator with
              | Parser.Positive        -> +operand
              | Parser.Negative        -> -operand
              | Parser.Factorial       -> factorial operand
              | Parser.Integration     -> Parser.Undefined //integrate operand
              | Parser.Differentiation -> Parser.Undefined //differentiate operand
              //| node                  -> SystemError $"Unexpected unary operator - got {node} instead"
         | Parser.FunctionDef (data, body) ->
             match data.identifier with
              | character, Some subscriptNumber -> Memory.set character (subscriptNumber + 1) (Parser.FunctionDef (data, body))
              | character, None                 -> Memory.set character 0 (Parser.FunctionDef (data, body))
             Parser.FunctionDef (data, body)
         | Parser.FunctionCall (name, args) ->
             match name with
              | Parser.Symbolic symbolicName -> Parser.Undefined
              | Parser.Identifiable (ch, sb) ->
                  let fn = match sb with Some snum -> Memory.get ch (snum + 1) | None -> Memory.get ch 0
                  match fn with
                   | Parser.FunctionDef (attributes, body) ->
                       applyArgs attributes.parameters args
                       evalTree body
         | Parser.Conditions (cases, defaultCase) -> Parser.Undefined // evalConditions
         | Parser.Comparison(ifTrue, left, operator, right) ->
             let left:  Parser.AST = evalTree left
             let right: Parser.AST = evalTree right
             match operator with
              | Parser.Equals             -> if left =  right then ifTrue else Parser.Undefined
              | Parser.NotEqual           -> if left <> right then ifTrue else Parser.Undefined
              | Parser.GreaterThan        -> if left >  right then ifTrue else Parser.Undefined
              | Parser.LessThan           -> if left <  right then ifTrue else Parser.Undefined
              | Parser.GreaterThanOrEqual -> if left >= right then ifTrue else Parser.Undefined
              | Parser.LessThanOrEqual    -> if left <= right then ifTrue else Parser.Undefined
              //| node             -> SystemError $"Unexpected comparison operator - got {node} instead"
         //| node -> SystemError $"Unexpected AST node - got {node} instead"

    let rec eval (src: string): Parser.AST Result =
        let tokens: Lexer.TokenStream = Lexer.tokenize src
        let error: DioriteError option = Lexer.getError tokens
        match error with
         | Some err -> Error err
         | None ->
               let tokens = match Lexer.streamContainsToken tokens Lexer.SemiColon with
                             | false -> tokens @ [Lexer.SemiColon]
                             | true  -> tokens
               match Parser.parse tokens with
                | Error err -> Error err
                | Ok root ->
                    (evalTree >> Ok) root
