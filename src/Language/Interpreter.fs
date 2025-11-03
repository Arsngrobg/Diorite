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
// Version: v1.6
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang


/// <summary>
/// The <c>Memory</c> module contains functionality for variable storage, retrieval, and
/// modification.
/// </summary>
module Memory =
    let letters = ['a'..'z'] @ ['A'..'Z']
    // Using option as all variables initially empty
    let table : Parser.AST[,] = Array2D.create 11 letters.Length Parser.AST.Undefined; // 11 rows (subscripts), and 52 columns (characters)
    
    /// <summary>
    /// Simple helper function to find the column index where a character is.
    /// </summary>
    /// <param name="character"> an alphabetical character </param>
    let findColIndex (character : char) =
        letters |> List.findIndex ((=) character)
    
    /// <summary>
    /// Gets the value of a given variable in the table.
    /// </summary>
    /// <param name="character"> the alphabetical character of the variable </param>
    /// <param name="rowIndex"> the row in the table where the character is, indicating the subscript </param>
    let get (character : char) (rowIndex : int) =
        let colIndex = findColIndex character
        table[rowIndex, colIndex]
        
    /// <summary>
    /// Sets the value of a given variable in the table.
    /// </summary>
    /// <param name="character"> the alphabetical character of the variable </param>
    /// <param name="rowIndex"> the row in the table where the character is, indicating the subscript </param>
    /// <param name="value"> the value being assigned </param>
    let set (character : char) (rowIndex : int) (value : Parser.AST) =
        let colIndex = findColIndex character
        table[rowIndex, colIndex] <- value

/// <summary>
///     The <c>Evaluator</c> module includes bindings related to evaluating a <b>Diorite</b> Abstract Syntax Tree.
/// </summary>
module Evaluator =
    // TODO: implement evaluations
    // this works fine when running from the REPL - since the syntax checks occur before this function is called
    // that is why it can return a SystemError and not SyntaxError but might change this
    let rec evalTree (root: Parser.AST): Parser.AST =
        let add (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity -> Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left + right)

        let subtract (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left - right)

        let multiply (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity -> Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left * right)

        let divide (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left / right)

        let modulo (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left % right)

        let pow (left: Parser.AST) (right: Parser.AST): Parser.AST =
            match left, right with
             | Parser.Infinity, _ | _, Parser.Infinity
             | _, Parser.Undefined | Parser.Undefined, _ -> Parser.Undefined
             | Parser.Number left, Parser.Number right -> Parser.Number (left ** right)

        let positive (operand: Parser.AST): Parser.AST =
            match operand with
             | Parser.Infinity -> Parser.Infinity
             | Parser.Undefined -> Parser.Undefined
             | Parser.Number value -> Parser.Number value

        let negative (operand: Parser.AST): Parser.AST =
            match operand with
             | Parser.Infinity -> Parser.Infinity
             | Parser.Undefined -> Parser.Undefined
             | Parser.Number value -> Parser.Number -value

        let rec factorial (operand: Parser.AST): Parser.AST =
            match operand with
             | Parser.Infinity -> Parser.Infinity
             | Parser.Undefined -> Parser.Undefined
             | Parser.Number value -> Parser.Undefined // for now

        // for AST.Begin
        let rec evalNodes (nodes: Parser.AST list): Parser.AST list =
            match nodes with
             | [] -> []
             | head :: tail -> evalTree head :: evalNodes tail

        match root with
         | Parser.Begin nodes -> (evalNodes >> Parser.Begin) nodes
         // values
         | Parser.Number value -> Parser.Number value
         | Parser.Identifier (character, maybeSubscript) ->
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
                   | Parser.Identifier (ch, maybeSubscript) ->
                       match maybeSubscript with
                        | Some subscriptNumber ->
                            Memory.set ch (subscriptNumber + 1) right
                            Parser.BinaryOperation (left, operator, right)
                        | None ->
                            Memory.set ch 0 right
                            Parser.BinaryOperation (left, operator, right)
                    //| node -> SystemError $"Unexpected type for set operation - got {node}"
              | Parser.Addition       -> (evalTree left) |> add      <| right
              | Parser.Subtraction    -> (evalTree left) |> subtract <| right
              | Parser.Multiplication -> (evalTree left) |> multiply <| right
              | Parser.Division       -> (evalTree left) |> divide   <| right
              | Parser.Modulo         -> (evalTree left) |> modulo   <| right
              | Parser.Exponentiation -> (evalTree left) |> pow      <| right
              //| node                  -> SystemError $"Unexpected binary operator - got {node} instead"
         | Parser.UnaryOperation(operand, operator) ->
             let operand: Parser.AST = evalTree operand
             match operator with
              | Parser.Positive        -> positive operand
              | Parser.Negative        -> negative operand
              | Parser.Factorial       -> Parser.Undefined //factorial operand
              | Parser.Integration     -> Parser.Undefined //integrate operand
              | Parser.Differentiation -> Parser.Undefined //differentiate operand
              //| node                  -> SystemError $"Unexpected unary operator - got {node} instead"
         | Parser.FunctionDef (data, body) ->
             match data.identifier with
              | character, Some subscriptNumber -> Memory.set character (subscriptNumber + 1) (Parser.FunctionDef (data, body))
              | character, None                 -> Memory.set character 0 (Parser.FunctionDef (data, body))
             Parser.FunctionDef (data, body)
         | Parser.FunctionCall (name, args) -> Parser.Undefined //evalTree - maybe being able to pass in memory map?
         | Parser.Conditions (cases, defaultCase) -> Parser.Undefined //evalConditions
         | Parser.Comparison(ifTrue, left, operator, right) ->
             let left:  Parser.AST = evalTree left
             let right: Parser.AST = evalTree right
             match operator with
              | Parser.Equals             -> Parser.Undefined //left = right
              | Parser.NotEqual           -> Parser.Undefined //left <> right
              | Parser.GreaterThan        -> Parser.Undefined //left > right
              | Parser.LessThan           -> Parser.Undefined //left < right
              | Parser.GreaterThanOrEqual -> Parser.Undefined //left >= right
              | Parser.LessThanOrEqual    -> Parser.Undefined //left <= right
              //| node             -> SystemError $"Unexpected comparison operator - got {node} instead"
         //| node -> SystemError $"Unexpected AST node - got {node} instead"

/// <summary>
///     The <c>REPL</c> module is the functionality related to the live interpreter environment in the terminal.
///     It provides a neat and simple environment for writing <b>Diorite</b> mathematics code.
///     It exposes a singular function for initialisation.
///     <code>
///         let stable: bool = REPL.launch()
///         match stable with
///          | true  -> IO.output "REPL executed with no errors :)"    |> ignore
///          | false -> IO.output "REPL had an error during execution" |> ignore
///     </code>
/// </summary>
[<RequireQualifiedAccess>]
module REPL =
    /// <summary>
    ///     A binding that defines the title of the REPL when in use.
    /// </summary>
    /// <returns> the title of the REPL </returns>
    let title: string = $"{Identity.name} REPL (v{Version.languageVersion.ToString()})"

    // helper function to test a string to see if it is a blank line
    let private isBlankLine (line: string): bool =
        System.String.IsNullOrWhiteSpace line

    // initializes the console environment and hence the REPL environment.
    let private initialiseConsole (): bool =
        // execute batch operation
        let result: unit Result = IO.compose [
            title                        |> IO.setConsoleTitle           |> generalized
            None                         |> IO.clearConsole              |> generalized
            System.ConsoleColor.DarkGray |> IO.setConsoleBackgroundColor |> generalized
            System.ConsoleColor.Black    |> IO.setConsoleForegroundColor |> generalized
            $"    {title} \n"            |> IO.output
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
            System.ConsoleColor.Black    |> IO.setConsoleBackgroundColor |> generalized
        ]
        resultAsBool result

    // processes the provided input from the user
    let private processInput (input: string): bool =
        // tokenize the input
        let tokens: Lexer.TokenStream = Lexer.tokenize input

        // defines what is output depending on the lexer result
        let noOutputIfNoTokens (): unit Result =
            let error: DioriteError option = Lexer.getError tokens
            match error with
             | Some err -> IO.compose [
                 System.ConsoleColor.Red   |> IO.setConsoleForegroundColor |> generalized;
                 IO.output $" X  {err}\n"
                 System.ConsoleColor.White |> IO.setConsoleForegroundColor |> generalized;
               ]
             | None ->
                  match tokens with
                   | [] -> Ok () // do nothing
                   | _  ->
                       let tokens = match Lexer.streamContainsToken tokens Lexer.SemiColon with
                                     | false -> tokens @ [Lexer.SemiColon]
                                     | true  -> tokens
                       match Parser.parse tokens with
                        | Error err -> IO.compose [
                            System.ConsoleColor.Red   |> IO.setConsoleForegroundColor |> generalized;
                            IO.output $" X  {err}\n"
                            System.ConsoleColor.White |> IO.setConsoleForegroundColor |> generalized;
                         ]
                        | Ok root -> IO.compose [
                            System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> generalized
                            //IO.output $" ¦  {Lexer.tokens2str tokens}\n"
                            //IO.output $" ¦  {root}\n"
                            IO.output $" ¦  {Evaluator.evalTree root}\n"
                         ]

        // partial for moving the cursor up or down by n units
        let moveCursorY: int -> (int * int) Result = IO.moveCursorRelative 0

        // execute batch operation
        let result: unit Result = IO.compose [
            -1                           |> moveCursorY                  |> generalized
            System.ConsoleColor.DarkGray |> IO.setConsoleForegroundColor |> generalized
            " |"                         |> IO.output
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
            $"  {input}\n"               |> IO.output;
            ()                           |> noOutputIfNoTokens
            System.ConsoleColor.White    |> IO.setConsoleForegroundColor |> generalized
        ]
        resultAsBool result

    /// <summary>
    ///     Launches the REPL environment in the user's terminal.
    /// </summary>
    /// <returns> <c>true</c> if the REPL exited without error; <c>false</c> if a fatal error occurred </returns>
    let rec launch (): bool =
        let rec env (): bool =
            match IO.input(Some ">>> ") with
             | Error _     -> false
             | Ok input ->
                 match input with
                  | "@quit" -> true
                  | _       ->
                      match processInput input with
                       | true  -> env()
                       | false -> false

        // exit if initialisation failed
        match initialiseConsole() with
         | true  -> env()
         | false -> false
