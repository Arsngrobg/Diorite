// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Interpreter.fs
// Summary: The interpreter of for the Diorite language, which also includes a REPL
// Author:  Arsngrobg, Borngle
// Version: v1.9
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

open Diorite.Lang

/// <summary>
///     The interpreter module is the
/// </summary>
module Interpreter =
    // shorthand typedefs
    type private AST    = Parser.AST

    type BinaryOperationRule = AST -> AST -> AST Result
    type UnaryOperationRule  = AST -> AST Result

    // bind functionality for an evaluation result
    let private (>>=) (evalResult: AST Result) (bindFunction: AST -> AST Result): AST Result =
        match evalResult with
         | Error err  -> Error err
         | Ok    node -> bindFunction node

    // binary addition rules
    let private (|+|): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | AST.PositiveInfinity, AST.NegativeInfinity
         | AST.NegativeInfinity, AST.PositiveInfinity -> Ok AST.Undefined

         | AST.PositiveInfinity, _
         | _,                    AST.PositiveInfinity -> Ok AST.PositiveInfinity

         | AST.NegativeInfinity, _
         | _,                    AST.NegativeInfinity -> Ok AST.NegativeInfinity

         | AST.Number left,      AST.Number right     -> (AST.Number >> Ok) (left + right)

         | left,                 right                -> MathError $"Unsupported addition between {left} & {right}"
    )

    // binary subtraction rules
    let private (|-|): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | AST.PositiveInfinity, AST.PositiveInfinity
         | AST.NegativeInfinity, AST.NegativeInfinity -> Ok AST.Undefined

         | _,                    AST.PositiveInfinity
         | AST.PositiveInfinity, _                    -> Ok AST.PositiveInfinity

         | AST.NegativeInfinity, _
         | _,                    AST.NegativeInfinity -> Ok AST.NegativeInfinity

         | AST.Number left,      AST.Number right     -> (AST.Number >> Ok) (left - right)

         | left,                 right                -> MathError $"Unsupported subtraction between {left} & {right}"
    )

    // binary multiplication rules
    let private (|*|): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | AST.NegativeInfinity, AST.NegativeInfinity -> Ok AST.PositiveInfinity

         | AST.NegativeInfinity, _
         | _,                    AST.NegativeInfinity -> Ok AST.NegativeInfinity

         | AST.PositiveInfinity, _
         | _,                    AST.PositiveInfinity -> Ok AST.PositiveInfinity

         | AST.Number left,      AST.Number right     -> (AST.Number >> Ok) (left * right)

         | left,                 right                -> MathError $"Unsupported multiplication between {left} & {right}"
    )

    // binary division rules
    let private (|/|): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | (AST.PositiveInfinity | AST.NegativeInfinity), (AST.PositiveInfinity | AST.NegativeInfinity)
                                                      -> Ok AST.Undefined

         | _,                    AST.PositiveInfinity
         | _,                    AST.NegativeInfinity -> (AST.Number >> Ok) 0
         | AST.PositiveInfinity, _                    -> Ok AST.PositiveInfinity
         | AST.NegativeInfinity, _                    -> Ok AST.NegativeInfinity

         | AST.Number left,      AST.Number right     ->
                if right = 0 then MathError "Division by zero"
                else              (AST.Number >> Ok) (left / right)

         | left,                 right                -> MathError $"Unsupported division between {left} & {right}"
    )

    // binary modulo rules
    let private (|%|): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | AST.PositiveInfinity, _
         | AST.NegativeInfinity, _                    -> Ok AST.Undefined

         | AST.Number left,      AST.PositiveInfinity
         | AST.Number left,      AST.NegativeInfinity -> (AST.Number >> Ok) left
         | AST.Number left,      AST.Number right     -> (AST.Number >> Ok) (left % right)

         | left,                 right                -> MathError $"Unsupported modulo between {left} & {right}"
    )

    // binary floor division rules
    let private (|//|): BinaryOperationRule = (fun left -> fun right ->
        (left |/| right) >>= (fun node ->
            match node with
             | AST.Number result -> (AST.Number >> Ok) <| Library.floor(result)
             | other             -> SystemError $"Binary division rule should have returned a number - not {other}"
        )
    )

    // binary exponent rules
    let rec private (|^|): BinaryOperationRule = (fun left -> fun right ->
        // there may be a way to simplify this, but it works so fuck you
        match left, right with
         | AST.PositiveInfinity, AST.Number right    -> Ok (
                if   right > 0 then AST.PositiveInfinity
                elif right = 0 then AST.Number 1
                else                AST.Number 0
             )
         | AST.Number left,      AST.PositiveInfinity -> Ok (
                if   left > 1             then AST.PositiveInfinity
                elif left > 0 && left < 1 then AST.Number 0
                else                           AST.Number 1
             )
         | AST.NegativeInfinity, AST.Number right     -> Ok (
                if right > 0 then
                    if   right % 2.0 = 0 then AST.PositiveInfinity
                    elif right % 2.0 = 1 then AST.NegativeInfinity
                    else                      AST.Undefined // complex
                else
                    if   right % 1.0 = 0 then AST.Number 0
                    else                      AST.Undefined // complex
             )
         | AST.PositiveInfinity, AST.NegativeInfinity ->
             (AST.PositiveInfinity |^| AST.PositiveInfinity) >>= (fun result -> (AST.Number 1) |/| result)
         | AST.PositiveInfinity, AST.PositiveInfinity -> Ok AST.PositiveInfinity
         | AST.NegativeInfinity, _                    -> Ok AST.Undefined // complex

         | AST.Number left,      AST.NegativeInfinity ->
             (AST.Number left |^| AST.PositiveInfinity) >>= (fun result -> (AST.Number 1) |/| result)

         | AST.Number left,      AST.Number right     -> (AST.Number >> Ok) (left ** right)
         | left,                 right                -> MathError $"Unsupported exponent between {left} & {right}"
    )

    // unary positive rules
    let private (|~+|): UnaryOperationRule = (fun operand ->
        match operand with
         | AST.PositiveInfinity
         | AST.NegativeInfinity
         | AST.Number _         -> Ok operand
         | other                -> MathError $"Unsupported negation for {other}"
    )

    // unary negative rules
    let private (|~-|): UnaryOperationRule = (fun operand ->
        match operand with
         | AST.PositiveInfinity -> Ok AST.NegativeInfinity
         | AST.NegativeInfinity -> Ok AST.PositiveInfinity
         | AST.Number operand   -> (AST.Number >> Ok) -operand
         | other                -> MathError $"Unsupported negation for {other}"
    )

    // unary absolution rules
    let private (|+/-|): UnaryOperationRule = (fun operand ->
        match operand with
         | AST.PositiveInfinity -> Ok operand
         | AST.NegativeInfinity -> Ok AST.PositiveInfinity
         | AST.Number operand   -> (AST.Number >> Ok) (if operand < 0 then -operand else operand)
         | other                -> MathError $"Unsupported absolution for {other}"
    )

    // unary factorial rules
    let rec private (|~!|): UnaryOperationRule = (fun operand ->
        match operand with
         | AST.PositiveInfinity -> Ok AST.PositiveInfinity
         | AST.NegativeInfinity -> Ok AST.Undefined
         | AST.Number operand   ->
             if   operand < 0 then Ok AST.Undefined
             elif operand < 2 then (AST.Number >> Ok) 1
             else
                 // expand the factorial into a sequence of multiplications that can then be applied
                 // which means that this function produces a deterministic form of it which the multiplication
                 // rules can then be applied to the expanded subtree
                 ((AST.Number (operand - 1.0)) |> (|~!|)) >>= (fun expanded ->
                    (AST.BinaryOperation >> Ok) (AST.Number operand, AST.Multiplication, expanded)
                 )
         | other                -> MathError $"Unsupported factorial for {other}"
    )

    // binary equality rules
    let (|==|): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | AST.Undefined,        AST.Undefined
         | AST.PositiveInfinity, AST.PositiveInfinity
         | AST.NegativeInfinity, AST.NegativeInfinity -> (AST.Number >> Ok) 1
         | AST.Number left,      AST.Number right     -> (AST.Number >> Ok) (if left = right then 1 else 0)
         | left,                 right                -> MathError $"Unsupported equality for {left} & {right}"
    )

    // binary inequality rules
    let (|!=|): BinaryOperationRule = (fun left -> fun right ->
        (left |==| right) >>= (fun result ->
            match result with
             | AST.Number boolean -> (AST.Number >> Ok) (if boolean = 1 then 0 else 1)
             | other              -> SystemError $"Condition evaluation should have produced a number - got {other}"
        )
    )

    // binary greater-than rules
    let (|>|): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | AST.PositiveInfinity, AST.PositiveInfinity -> (AST.Number >> Ok) 0
         | AST.PositiveInfinity, AST.NegativeInfinity -> (AST.Number >> Ok) 1
         | AST.NegativeInfinity, AST.NegativeInfinity -> (AST.Number >> Ok) 0
         | AST.NegativeInfinity, AST.PositiveInfinity -> (AST.Number >> Ok) 0
         | AST.Number left,      AST.Number right     ->
             if left > right then (AST.Number >> Ok) 1 else (AST.Number >> Ok) 0
         | left,                 right                -> MathError $"Unsupported greater-than for {left} & {right}"
    )

    // binary greater-than-or-equal rules
    let (|>=|): BinaryOperationRule = (fun left -> fun right ->
        (left |>| right) >>= (fun result ->
            match result with
             | AST.Number boolean -> if boolean = 0 then (left |==| right) else (AST.Number >> Ok) 1
             | other              -> SystemError $"Condition evaluation should have produced a number - got {other}"
        )
    )

    // binary less-than rules
    let (|<|): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | AST.PositiveInfinity, AST.PositiveInfinity -> (AST.Number >> Ok) 0
         | AST.PositiveInfinity, AST.NegativeInfinity -> (AST.Number >> Ok) 0
         | AST.NegativeInfinity, AST.NegativeInfinity -> (AST.Number >> Ok) 0
         | AST.NegativeInfinity, AST.PositiveInfinity -> (AST.Number >> Ok) 1
         | AST.Number left,      AST.Number right     ->
             if left < right then (AST.Number >> Ok) 1 else (AST.Number >> Ok) 0
         | left,                 right                -> MathError $"Unsupported less-than for {left} & {right}"
    )

    // binary less-than-or-equal rules
    let (|<=|): BinaryOperationRule = (fun left -> fun right ->
        (left |<| right) >>= (fun result ->
            match result with
             | AST.Number boolean -> if boolean = 0 then (left |==| right) else (AST.Number >> Ok) 1
             | other              -> SystemError $"Condition evaluation should have produced a number - got {other}"
        )
    )

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

    let rec evalTree (root: AST): AST Result =
        match root with
         // begin nodes
         | AST.Begin [] -> (AST.Begin >> Ok) []
         | AST.Begin (head :: tail) ->
             evalTree head >>= (fun node ->
                 (AST.Begin >> evalTree) tail >>= (fun otherNode ->
                    match otherNode with
                     | AST.Begin nodes -> (AST.Begin >> Ok) (node :: nodes)
                     | node            -> SystemError $"Expected Begin - got {node}"
                 )
             )

         // values
         | AST.Undefined        -> Ok AST.Undefined
         | AST.PositiveInfinity -> Ok AST.PositiveInfinity
         | AST.Number value -> (AST.Number >> Ok) value
         | AST.Variable (character, subscript) ->
             let offset: int = match subscript with Some sb -> sb + 1 | None -> 0
             Ok (Memory.get character offset)

         // binary operations
         | AST.BinaryOperation (left, operator, right) ->
             match operator with
              | AST.Equals         ->
                  match left with
                   | AST.Variable (character, subscript) ->
                       let offset: int = match subscript with Some sb -> sb + 1 | None -> 0
                       evalTree right >>= (fun r ->
                           Memory.set character offset r
                           (AST.BinaryOperation >> Ok) (left, AST.Equals, r)
                       )
                   | _ -> MathError "Diorite does not support series of linear equations"
              | AST.Addition       -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |+|  r))
              | AST.Subtraction    -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |-|  r))
              | AST.Multiplication -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |*|  r))
              | AST.Division       -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |/|  r))
              | AST.Modulo         -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |%|  r))
              | AST.FloorDivision  -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |//| r))
              | AST.Exponentiation -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |^|  r))
              | other              -> MathError $"Unsupported binary operation {other}"

         // unary operations (TODO: integration & differentiation)
         | AST.UnaryOperation (operand, operator) ->
             match operator with
              | AST.Positive   -> evalTree operand >>= (|~+|)
              | AST.Negative   -> evalTree operand >>= (|~-|)
              | AST.Absolution -> evalTree operand >>= (|+/-|)
              | AST.Factorial  -> evalTree operand >>= (fun o -> (o |> (|~!|) >>= evalTree))
              | other          -> MathError $"Unsupported unary operation {other}"

         // function definitions
         | AST.FunctionDef (attributes, body) ->
             match attributes.identifier with
              | character, Some subscript -> Memory.set character (subscript + 1)
              | character, None           -> Memory.set character 0
             <| root
             (AST.FunctionDef >> Ok) (attributes, body)

         // function calls (TODO: symbolic function applications)
         | AST.FunctionCall (name, args) ->
             let rec loadArgs (parameters: Parser.FunctionParameter list) (args: AST list): unit Result =
                 match parameters, args with
                  | [], [] -> Ok ()
                  // TODO: check if value in set
                  | ((character, subscript), _) :: paramsTail, argsHead :: argsTail ->
                      match evalTree argsHead with
                       | Error err  -> Error err
                       | Ok    node ->
                           Memory.set character (match subscript with Some sb -> sb + 1 | None -> 0) node
                           loadArgs paramsTail argsTail
                  | parameters, _ -> MathError $"Missing {parameters.Length} arguments"

             let fn: AST = match name with
                           | Parser.SymbolicName _        -> AST.Undefined
                           | Parser.VariableName (ch, sb) -> Memory.get ch (match sb with Some sb -> sb + 1 | None -> 0)
             match fn with
              | AST.FunctionDef (attributes, body) ->
                  let prevState: AST list = attributes.parameters |> List.map (fun ((ch, sb), _) ->
                      Memory.get ch (match sb with Some sb -> sb + 1 | None -> 0)
                  )
                  match loadArgs attributes.parameters args with
                   | Error err -> Error err
                   | Ok    ()  ->
                        let result: AST Result = evalTree body
                        (loadArgs attributes.parameters prevState) |> ignore
                        result
              | _ -> MathError "Variable is an expression and not a function definition"

         // conditions
         | AST.Conditions (cases, defaultCase) ->
             match cases with
              | []           -> evalTree defaultCase
              | head :: tail ->
                  evalTree head >>= (fun boolean ->
                      match boolean with
                       | AST.Number 1.0 ->
                           match head with
                            | AST.Comparison (ifTrue, _, _, _) -> evalTree ifTrue
                            | other -> SystemError $"Current case should be Conditions - got {other}"
                       | AST.Number 0.0 -> (AST.Conditions >> evalTree) (tail, defaultCase)
                       | other -> SystemError $"Evaluated condition did not return number - got {other}"
                  )

         // comparisons
         | AST.Comparison(_, left, operator, right) ->
             match operator with
              | AST.Equals             -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |==| r))
              | AST.NotEqual           -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |!=| r))
              | AST.GreaterThan        -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |>|  r))
              | AST.GreaterThanOrEqual -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |>=| r))
              | AST.LessThan           -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |<|  r))
              | AST.LessThanOrEqual    -> evalTree left >>= (fun l -> evalTree right >>= (fun r -> l |<=| r))
              | other                  -> MathError $"Unsupported comparison operator {other}"

         | other -> MathError $"Cannot evaluate lone {other} node"


    let rec eval (src: string): AST Result =
        let tokens: Lexer.TokenStream = Lexer.tokenize src
        let error: DioriteError option = Lexer.getError tokens
        match error with
         | Some err -> Error err
         | None ->
               let tokens = if (tokens |> List.rev).Head <> Lexer.SemiColon then tokens @ [Lexer.SemiColon] else tokens
               match Parser.parse tokens with
                | Error err  -> Error err
                | Ok    root -> evalTree root
