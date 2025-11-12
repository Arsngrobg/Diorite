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
    // shorthand typedef for Parser.AST
    type private AST = Parser.AST

    /// <summary>
    ///     The <c>Memory</c> module contains functionality for variable storage, retrieval, and modification.
    /// </summary>
    module Memory =
        // 11 rows (subscripts), and 52 columns (characters)
        let letters: char list = ['a'..'z'] @ ['A'..'Z']
        let table: AST[,] = Array2D.create 11 letters.Length AST.Undefined

        /// Simple helper function to find the column index where a character is
        let private findColIndex (character: char): int =
            letters |> List.findIndex ((=) character)

        /// <summary>
        ///     Gets the value of a given variable in the table.
        /// </summary>
        /// <param name='character'> the alphabetical character of the variable </param>
        /// <param name='rowIndex'> the row in the table where the character is, indicating the subscript </param>
        /// <returns> the value stored in the table at the location </returns>
        let get (character: char) (rowIndex: int): AST =
            let colIndex = findColIndex character
            table[rowIndex, colIndex]

        /// <summary>
        ///     Sets the value of a given variable in the table.
        /// </summary>
        /// <param name='character'> the alphabetical character of the variable </param>
        /// <param name='rowIndex'> the row in the table where the character is, indicating the subscript </param>
        /// <param name='value'> the value being assigned </param>
        let set (character: char) (rowIndex: int) (value: AST): unit =
            let colIndex = findColIndex character
            table[rowIndex, colIndex] <- value

    // TODO: implement evaluations
    // this works fine when running from the REPL - since the syntax checks occur before this function is called
    // that is why it can return a SystemError and not SyntaxError but might change this
    let rec evalTree (root: AST): AST Result =
        // bind operator for a pair of operands and 
        let (>>=) (operands: AST Result * AST Result) (fn: AST -> AST -> AST Result): AST Result =
            match operands with
             | Error err, _ | _, Error err -> Error err
             | Ok left, Ok right -> fn left right

        // addition rules
        let (|+|) (left: AST) (right: AST): AST Result =
            match left, right with
             | AST.Infinity,  _ | _, AST.Infinity  -> Ok AST.Infinity
             | AST.Undefined, _ | _, AST.Undefined -> Ok AST.Undefined
             | AST.Number left, AST.Number right   -> (AST.Number >> Ok) (left + right)
             | left, right -> MathError $"Unsupported operation for {left} + {right}"

        // subtraction rules
        let (|-|) (left: AST) (right: AST): AST Result =
            match left, right with
             | AST.Infinity,  _ | _, AST.Infinity
             | AST.Undefined, _ | _, AST.Undefined -> Ok AST.Undefined
             | AST.Number left, AST.Number right   -> (AST.Number >> Ok) (left - right)
             | left, right -> MathError $"Unsupported operation for {left} - {right}"

        // multiplication rules
        let (|*|) (left: AST) (right: AST): AST Result =
            match left, right with
             | AST.Infinity,  _ | _, AST.Infinity  -> Ok AST.Infinity
             | AST.Undefined, _ | _, AST.Undefined -> Ok AST.Undefined
             | AST.Number left, AST.Number right   -> (AST.Number >> Ok) (left * right)
             | left, right -> MathError $"Unsupported operation for {left} * {right}"

        // division rules
        let (|/|) (left: AST) (right: AST): AST Result =
            match left, right with
             | AST.Infinity,  _ | _, AST.Infinity
             | AST.Undefined, _ | _, AST.Undefined -> Ok AST.Undefined
             | AST.Number left, AST.Number right   ->
                 if right = 0 then MathError "division by zero"
                 else              (AST.Number >> Ok) (left / right)
             | left, right -> MathError $"Unsupported operation for {left} / {right}"

        // floor division rules
        let (|//|) (left: AST) (right: AST): AST Result =
            match left, right with
             | AST.Infinity,  _ | _, AST.Infinity
             | AST.Undefined, _ | _, AST.Undefined -> Ok AST.Undefined
             | AST.Number left, AST.Number right   ->
                 if right = 0 then MathError "division by zero"
                 else              (AST.Number >> Ok) (Library.floor(left / right))
             | left, right -> MathError $"Unsupported operation for {left} / {right}"

        // modulo rules
        let (|%|) (left: AST) (right: AST): AST Result =
            match left, right with
             | AST.Infinity,  _ | _, AST.Infinity
             | AST.Undefined, _ | _, AST.Undefined -> Ok AST.Undefined
             | AST.Number left, AST.Number right   -> (AST.Number >> Ok) (left % right)
             | left, right -> MathError $"Unsupported operation for {left} %% {right}"

        // exponentiation rules
        let (|^|) (left: AST) (right: AST): AST Result =
            match left, right with
             | AST.Infinity,  _ | _, AST.Infinity  -> Ok AST.Infinity
             | AST.Undefined, _ | _, AST.Undefined -> Ok AST.Undefined
             | AST.Number left, AST.Number right   -> (AST.Number >> Ok) (left ** right)
             | left, right -> MathError $"Unsupported operation for {left} ^ {right}"

        match root with
         // begin node
         | AST.Begin []             -> (AST.Begin >> Ok) []
         | AST.Begin (head :: tail) ->
             match evalTree head with
              | Error err  -> Error err
              | Ok    node ->
                  match (AST.Begin >> evalTree) tail with
                   | Error err            -> Error err
                   | Ok (AST.Begin nodes) -> (AST.Begin >> Ok) (node :: nodes)
                   | Ok node              -> MathError $"Unexpected node type {node}"

         // values
         | AST.Undefined         -> Ok AST.Undefined
         | AST.Infinity          -> Ok AST.Infinity
         | AST.Variable (ch, sb) ->
             let subscript: int = match sb with Some subscript -> (subscript + 1) | None -> 0
             let value: AST = Memory.get ch subscript
             Ok value
         | AST.Number value      -> (AST.Number >> Ok) value

         // binary operations
         | AST.BinaryOperation (left, operation, right) ->
             match operation with
              | AST.Equals ->
                  match left with
                   | AST.Variable (ch, sb) ->
                       let subscript: int = match sb with Some subscript -> (subscript + 1) | None -> 0
                       match evalTree right with
                        | Error err -> Error err
                        | Ok node   ->
                            Memory.set ch subscript node
                            (AST.BinaryOperation >> Ok) (left, operation, node)
                   | node -> MathError $"Unsupported assignment operation on lhs {node}"
              | AST.Addition       -> (evalTree left, evalTree right) >>= (|+|)
              | AST.Subtraction    -> (evalTree left, evalTree right) >>= (|-|)
              | AST.Multiplication -> (evalTree left, evalTree right) >>= (|*|)
              | AST.Division       -> (evalTree left, evalTree right) >>= (|/|)
              | AST.FloorDivision  -> (evalTree left, evalTree right) >>= (|//|)
              | AST.Modulo         -> (evalTree left, evalTree right) >>= (|%|)
              | AST.Exponentiation -> (evalTree left, evalTree right) >>= (|^|)
              | node               -> MathError $"Unsupported binary operation {node}"

         // unary operations
         | AST.UnaryOperation (operand, operator) ->
             match operator with
              | AST.Positive       -> (evalTree operand, (AST.Number >> Ok) 0) >>= (|+|)
              | AST.Negative       -> ((AST.Number >> Ok) 0, evalTree operand) >>= (|-|)
              | AST.Factorial      ->
                  match evalTree operand with
                   | Error err -> Error err
                   | Ok (AST.Number value) ->
                       if   value < 0 then Ok AST.Undefined
                       elif value < 2 then (AST.Number >> Ok) 1.0
                       else ((AST.Number >> Ok) value,
                             (AST.UnaryOperation >> evalTree) (AST.Number (value - 1.0), AST.Factorial)) >>= (|*|)
                   | Ok node       -> MathError $"Unsupported operand {node}"
              | AST.Absolution     ->
                  match evalTree operand with
                   | Error err -> Error err
                   | Ok (AST.Number value) ->
                       if value < 0 then (AST.Number >> Ok) -value
                       else              (AST.Number >> Ok)  value
                   | Ok node       -> MathError $"Unsupported operand {node}"
              | node -> MathError $"Unsupported unary operation {node}"

         // function definition
         | AST.FunctionDef (data, body) ->
             let mem: char * int =
                 match data.identifier with
                  | ch, None    -> ch, 0
                  | ch, Some sb -> ch, (sb + 1)
             let (ch: char), (offset: int) = mem
             Memory.set ch offset root
             (AST.FunctionDef >> Ok) (data, body)

         // function call
         | AST.FunctionCall (name, args) ->
             let rec applyArgs (parameters: Parser.FunctionParameter list) (args: AST list): unit Result =
                 if parameters.Length <> args.Length then
                    MathError $"Expected {parameters.Length} arguments - got {args.Length} instead"
                 else
                     match parameters with
                      | head :: tail ->
                         let mem: char * int =
                             match head with // TODO: check arguments for set alignment
                              | (ch, None),    numberSet -> ch, 0
                              | (ch, Some sb), numberSet -> ch, (sb + 1)
                         let (ch: char), (offset: int) = mem
                         Memory.set ch offset args.Head
                         applyArgs tail args.Tail
                      | [] -> Ok ()

             match name with
              | Parser.Symbolic name -> Ok AST.Undefined // TODO: use symbol table
              | Parser.Identifiable (ch, sb) ->
                  let fn = match sb with Some sb -> Memory.get ch (sb + 1) | None -> Memory.get ch 0
                  match fn with
                   | Parser.FunctionDef (attributes, body) ->
                       match applyArgs attributes.parameters args with
                        | Error err -> Error err
                        | Ok () ->
                            match evalTree body with
                             | Error err -> Error err
                             | Ok result -> Ok result
                   | node -> MathError $"Expected function - got {node} instead"

         // TODO: comparisons
         | AST.Conditions (cases, defaultCase) ->
             MathError "Unsupported operation - as of now"
         | AST.Comparison(ifTrue, left, operator, right) ->
             MathError "Unsupported operation - as of now"

         // if otherwise
         | node -> MathError $"Expected value {node}"

    let rec eval (src: string): AST Result =
        let tokens: Lexer.TokenStream = Lexer.tokenize src
        let error: DioriteError option = Lexer.getError tokens
        match error with
         | Some err -> Error err
         | None ->
               let tokens = if (Lexer.streamContainsToken tokens) Lexer.SemiColon then tokens
                            else tokens @ [Lexer.SemiColon]
               match Parser.parse tokens with
                | Error err  -> Error err
                | Ok    root -> evalTree root
