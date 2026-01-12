// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Evaluation.fs
// Summary: Evaluation functions for the structured syntax types
// Author:  Arsngrobg, Borngle
// Version: v1.7
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Errors
open Diorite.Lang.Core.Runtime
open Diorite.Lang.Core.Runtime.Optimizer
open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Runtime.RuleMappings

/// <summary>
///     <p>The <c>Evaluation</c> module contains functions for evaluating structured syntax such as <c>Expression</c> or
///        <c>PiecewiseCondition list</c> for functions.
///     </p>
/// </summary>
module Evaluation =
    /// <summary>
    ///     <p>Gets the equivalent error message from the supplied <c>NumberSet</c> and whether the erroneous
    ///        <c>ValueType</c> was an input or output value.
    ///     </p>
    /// </summary>
    /// <param name="isInput"> whether the erroneous <c>ValueType</c> is an input or output </param>
    /// <param name="set"> the <c>NumberSet</c> the supplied <c>ValueType</c> doesn't match with </param>
    /// <param name="value"> the erroneous <c>ValueType</c> </param>
    /// <returns> a <c>MathError</c> that contains that message and erroneous <c>ValueType</c> </returns>
    let GetMembershipError (isInput: bool) (set: NumberSet) (value: ValueType): ValueType Result =
        let setStr: string =
            match set with
             | NumberSet.Natural    -> "N"
             | NumberSet.Integer    -> "Z"
             | NumberSet.Real       -> "R"
             | NumberSet.Rational   -> "Q"
             | NumberSet.Irrational -> "I"
             | NumberSet.Complex    -> "C"
        let msg: string =
            if isInput then $"Expected Real ({setStr}) number set membership for input argument {value}"
            else            $"Expected Real ({setStr}) number set membership for output argument {value}"
        (Some msg, [value]) ||> MathError

    /// <summary>
    ///     <p>The <c>Evaluator</c> type is a lambda that evaluates a specific structure defined by the <c>'a</c> type.
    ///        It references the supplied <c>Memory</c> struct, and produces a value, bound by the generic type
    ///        <c>'b</c>, alongside the updated <c>Memory</c>.
    ///     </p>
    /// </summary>
    type Evaluator<'a, 'b> = 'a * Memory -> ('b * Memory) Result

    /// <summary>
    ///     <p>Produces an <c>Evaluator</c> that evaluates whether a given <c>ValueType</c> has membership in the
    ///        <c>Natural</c> number set.
    ///     </p>
    /// </summary>
    /// <param name="isInput"> for error messages, if the <c>ValueType</c> is an input or output value </param>
    /// <returns> a new <c>Evaluator</c>, that tests the membership against the <c>Natural</c> set </returns>
    let NaturalSetEvaluator (isInput: bool): Evaluator<ValueType, ValueType> =
        (fun (value, memory) ->
            match value with
             | ValueType.Undefined                                                   ->
                 (ValueType.Undefined, memory) |> Ok
             | ValueType.Number    x when (x = (System.Math.Truncate x)) && (x >= 0) ->
                 (ValueType.Number x,  memory) |> Ok
             | value                                                                 ->
                 ((isInput, NumberSet.Natural, value) |||> GetMembershipError)
                 |> Result.map (fun result -> (result, memory))
        )

    /// <summary>
    ///     <p>Produces an <c>Evaluator</c> that evaluates whether a given <c>ValueType</c> has membership in the
    ///        <c>Integer</c> number set.
    ///     </p>
    /// </summary>
    /// <param name="isInput"> for error messages, if the <c>ValueType</c> is an input or output value </param>
    /// <returns> a new <c>Evaluator</c>, that tests the membership against the <c>Integer</c> set </returns>
    let IntegerSetEvaluator (isInput: bool): Evaluator<ValueType, ValueType> =
        (fun (value, memory) ->
            match value with
             | ValueType.Undefined                                    -> (ValueType.Undefined, memory) |> Ok
             | ValueType.Number x when (x = (System.Math.Truncate x)) -> (ValueType.Number x,  memory) |> Ok
             | value                                                  ->
                 ((isInput, NumberSet.Integer, value) |||> GetMembershipError)
                 |> Result.map (fun result -> (result, memory))
        )

    /// <summary>
    ///     <p>Produces an <c>Evaluator</c> that evaluates whether a given <c>ValueType</c> has membership in the
    ///        <c>Real</c> number set.
    ///     </p>
    /// </summary>
    /// <param name="isInput"> for error messages, if the <c>ValueType</c> is an input or output value </param>
    /// <returns> a new <c>Evaluator</c>, that tests the membership against the <c>Real</c> set </returns>
    let RealSetEvaluator (isInput: bool): Evaluator<ValueType, ValueType> =
        (fun (value, memory) ->
            match value with
             | ValueType.Undefined -> (ValueType.Undefined, memory) |> Ok
             | ValueType.Number x  -> (ValueType.Number x,  memory) |> Ok
             | value               ->
                 ((isInput, NumberSet.Real, value) |||> GetMembershipError)
                 |> Result.map (fun result -> (result, memory))
        )

    /// <summary>
    ///     <p>The maximum denominator to use for irrationality heuristic.</p>
    /// </summary>
    let MaxDenominator: int = 10000000
    /// <summary>
    ///     <p>The tolerance value before a number is considered to be likely irrational.</p>
    /// </summary>
    let IrrationalTolerance: float = 0.70

    /// <summary>
    ///     <p>Produces an <c>Evaluator</c> that evaluates whether a given <c>ValueType</c> has membership in the
    ///        <c>Irrational</c> number set.
    ///     </p>
    /// </summary>
    /// <param name="isInput"> for error messages, if the <c>ValueType</c> is an input or output value </param>
    /// <returns> a new <c>Evaluator</c>, that tests the membership against the <c>Irrational</c> set </returns>
    let IrrationalSetEvaluator (isInput: bool): Evaluator<ValueType, ValueType> =
        (fun (value, memory) ->
            match value with
             | ValueType.Undefined -> (ValueType.Undefined, memory) |> Ok
             | ValueType.Number x  ->
                 let denominator: int = MaxDenominator
                 let numerator:   int = int (System.Math.Round(x * (float denominator)))
            
                 let error = abs(x - (float numerator) / (float denominator))
                 let percentage: float = if error > 0 then min 1.0 (-System.Math.Log10(error) / 10.0) else 0.0
                 if percentage < IrrationalTolerance then
                     (isInput, NumberSet.Natural, value) |||> GetMembershipError
                     |> Result.map (fun result -> (result, memory))
                 else
                     (value, memory) |> Ok
             | value               ->
                 ((isInput, NumberSet.Irrational, value) |||> GetMembershipError)
                 |> Result.map (fun result -> (result, memory))
        )

    /// <summary>
    ///     <p>Produces an <c>Evaluator</c> that evaluates whether a given <c>ValueType</c> has membership in the
    ///        <c>Rational</c> number set.
    ///     </p>
    /// </summary>
    /// <param name="isInput"> for error messages, if the <c>ValueType</c> is an input or output value </param>
    /// <returns> a new <c>Evaluator</c>, that tests the membership against the <c>Rational</c> set </returns>
    let RationalSetEvaluator (isInput: bool): Evaluator<ValueType, ValueType> =
        (fun (value, memory) ->
            match value with
             | ValueType.Undefined -> (ValueType.Undefined, memory) |> Ok
             | ValueType.Number x  ->
                 match ((ValueType.Number x, memory) |> (IrrationalSetEvaluator isInput)) with
                  | Error _ -> (ValueType.Number x, memory) |> Ok
                  | Ok    _ ->
                     ((isInput, NumberSet.Rational, value) |||> GetMembershipError)
                     |> Result.map (fun result -> (result, memory))
             | value               ->
                 ((isInput, NumberSet.Rational, value) |||> GetMembershipError)
                 |> Result.map (fun result -> (result, memory))
        )

    /// <summary>
    ///     <p>Produces an <c>Evaluator</c> that evaluates whether a given <c>ValueType</c> has membership in the
    ///        <c>Complex</c> number set.
    ///     </p>
    ///     <p><i>All values are inherintly complex.</i></p>
    /// </summary>
    let ComplexSetEvaluator: Evaluator<ValueType, ValueType> = Ok

    /// <summary>
    ///     <p>The evaluator for a <c>VariableType</c>.</p>
    ///     <p>It will try to unwrap the <c>VariableType</c> into its <c>ValueType</c> in <c>Memory</c>.</p>
    /// </summary>
    let VariableEvaluator: Evaluator<VariableType, ValueType> = (fun (variable, memory) ->
        match (variable |> (GetVariable memory)) with
         | CellData.OfValue    value        -> (value, memory) |> Ok
         | CellData.OfFunction (fnAttrs, _) ->
             let functionStr: string =
                 fnAttrs.parameters
                 |> List.map  fst
                 |> List.map  (fun (c, s) -> if s = 0uy then $"{c}" else $"{c}{s-1uy}")
                 |> String.concat ", "
                 |> (fun paramStr -> $"{fnAttrs.parameters}({paramStr})")
             (Some $"Expected ValueType got FunctionType instead ({functionStr})", []) ||> MathError
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for an <c>Expression</c>.</p>
    /// </summary>
    let rec ExpressionEvaluator: Evaluator<Expression, ValueType> = (fun (expression, memory) ->
        match expression with
         | Expression.Value           value               -> (value,                memory) |> Ok
         | Expression.Variable        variable            -> (variable,             memory) |> VariableEvaluator
         | Expression.BinaryOperation (l, o, r)           -> (((l, o, r),           memory) |> BinaryOperationEvaluator)
         | Expression.UnaryOperation  (operand, operator) -> (((operand, operator), memory) |> UnaryOperationEvaluator)
         | Expression.FunctionCall    (fnRef,   fnArgs)   -> (((fnRef, fnArgs),     memory) |> FunctionCallEvaluator)
    )
    
    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <i>structured binary operation</i>.</p>
    /// </summary>
    and BinaryOperationEvaluator: Evaluator<Expression * BinaryOperator * Expression, ValueType> =
        // optimised function call to reuse the call stack
        (fun ((l, o, r), memory) ->
            // optimised function call to reuse the call stack
            let optimisedCall: Bounce = Call (fun () ->
                // left
                match ((l, memory) |> ExpressionEvaluator) with
                 | Error err         -> Done (Error err)
                 | Ok    (l, memory) ->
                     // right
                     Call (fun () ->
                        match ((r, memory) |> ExpressionEvaluator) with
                         | Error err         -> Done (Error err)
                         | Ok    (r, memory) ->
                             // application
                             Done (
                                 ((l, r) |> (GetBinaryRule o))
                                 |> Result.map (fun value -> (value, memory))
                             )
                     )
            )
            
            optimisedCall |> EvaluateBounce
        )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <i>structured unary operation</i>.</p>
    /// </summary>
    and UnaryOperationEvaluator: Evaluator<Expression * UnaryOperator, ValueType> =
        (fun ((operand, operator), memory) ->
            // optimised function call to reuse the call stack
            let optimisedCall: Bounce = Call (fun () ->
                // operand
                match ((operand, memory) |> ExpressionEvaluator) with
                 | Error err               -> Done (Error err)
                 | Ok    (operand, memory) ->
                     // application
                     Done (
                         (operand |> (GetUnaryRule operator))
                         |> Result.map (fun value -> (value, memory))
                     )
            )

            optimisedCall |> EvaluateBounce
        )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a sequence of function arguments.</p>
    /// </summary>
    and FunctionArgumentEvaluator: Evaluator<Expression list, ValueType list> = (fun (args, memory) ->
        match args with
         | []           -> ([], memory) |> Ok
         | head :: tail ->
             match ((head, memory) |> ExpressionEvaluator) with
              | Error err           -> Error err
              | Ok    (arg, memory) ->
                  match ((tail, memory) |> FunctionArgumentEvaluator) with
                   | Error err            -> Error err
                   | Ok    (args, memory) -> Ok (arg :: args, memory)
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <i>structured function call</i>.</p>
    /// </summary>
    and FunctionCallEvaluator: Evaluator<FunctionReferenceType * Expression list, ValueType> =
        (fun ((fnRef, fnArgs), memory) ->
            // error factory
            let err (msg: string): Bounce =
                 Done ((Some msg, []) ||> MathError)

            // optimised function call to reuse the call stack
            let optimisedCall: Bounce = Call (fun () ->
                // validate function reference
                match (fnRef |> (GetFunctionFromRef memory)) with
                 | None                   ->
                     err $"Function reference {fnRef} does not point to a real function in memory"
                 | Some (fnAttrs, fnBody) ->
                     // validate function arguments
                     let argDiff: int = fnAttrs.parameters.Length - fnArgs.Length
                     if argDiff < 0 then
                        err $"Missing {-argDiff} position arguments for function: {fnRef}"
                     elif argDiff > 0 then
                         err $"Too many arguments supplied to function: {fnRef}"
                     else
                         Call (fun () ->
                             // some memoization can happen
                             match ((fnArgs, memory) |> FunctionArgumentEvaluator) with
                              | Error err              -> Done (Error err)
                              | Ok    (fnArgs, memory) ->
                                  let pairs: (VariableType * CellData) list =
                                      fnArgs
                                      |> List.map CellData.OfValue
                                      |> List.zip (List.map fst fnAttrs.parameters)
                                  
                                  let scopedMemory: Memory = pairs |> (SetVariables memory)
                                  Call (fun () -> Done ())
                         )
            )

            optimisedCall |> EvaluateBounce
        )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <c>FunctionBody</c>.</p>
    /// </summary>
    and FunctionBodyEvaluator: Evaluator<FunctionBody, ValueType> = (fun (fnBody, memory) ->
        // optimised function call to reuse the call stack
        let optimisedCall: Bounce = Call (fun () ->
            match fnBody with
             | FunctionBody.Expression          expression -> Done ((expression, memory) |> ExpressionEvaluator)
             | FunctionBody.PiecewiseConditions conditions -> Done ((conditions, memory) |> PiecewiseConditionsEvaluator)
        )

        optimisedCall |> EvaluateBounce
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a sequence of <c>PiecewiseCondition</c>s.</p>
    /// </summary>
    and PiecewiseConditionsEvaluator: Evaluator<PiecewiseCondition list, ValueType> = (fun (cs, memory) ->
        let optimisedCall: Bounce = Call (fun () ->
            match cs with
             | []                          -> failwith "no PiecewiseConditions provided"
             | [(baseCase, _)]             -> Done ((baseCase, memory) |> FunctionResultEvaluator)
             | head :: tail ->
                 match ((head, memory) |> PiecewiseConditionEvaluator) with
                  | Error err             -> Done (Error err)
                  | Ok    (value, memory) ->
                      match value with
                       | ValueType.Number x when x |> System.Double.IsNaN ->
                           Done ((tail, memory) |> PiecewiseConditionsEvaluator)
                       | value                                            ->
                           Done ((value, memory) |> Ok)
        )

        optimisedCall |> EvaluateBounce
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <c>PiecewiseCondition</c>.</p>
    /// </summary>
    and PiecewiseConditionEvaluator: Evaluator<PiecewiseCondition, ValueType> = (fun ((ifTrue, (l, o, r)), memory) ->
        // optimised function call to reuse the call stack
        let optimisedCall: Bounce = Call (fun () ->
            // left
            match ((l, memory) |> ExpressionEvaluator) with
             | Error err         -> Done (Error err)
             | Ok    (l, memory) ->
                 // right
                 Call (fun () ->
                    match ((r, memory) |> ExpressionEvaluator) with
                     | Error err         -> Done (Error err)
                     | Ok    (r, memory) ->
                         // application
                         Call (fun () ->
                             match ((l, r) |> (GetComparisonRule o)) with
                             | Error err -> Done (Error err)
                             | Ok    b   ->
                                 if not b then
                                     Done ((ConstantNaN, memory) |> Ok)
                                 else
                                     Done ((ifTrue, memory) |> FunctionResultEvaluator)
                         )
                 )
        )
            
        optimisedCall |> EvaluateBounce
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <c>FunctionResult</c>.</p>
    /// </summary>
    and FunctionResultEvaluator: Evaluator<FunctionResult, ValueType> = (fun (fnResult, memory) ->
        // optimised function call to reuse the call stack
        let optimisedCall: Bounce = Call (fun () ->
            match fnResult with
             | FunctionResult.Error      error      -> Done ((error, []) ||> MathError)
             | FunctionResult.Expression expression -> Done ((expression, memory) |> ExpressionEvaluator)
        )

        optimisedCall |> EvaluateBounce
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <i>structured assignment operation</i>.</p>
    /// </summary>
    let AssignmentEvaluator: Evaluator<VariableType * Expression, ValueType> = (fun ((variable, expression), memory) ->
        ((expression, memory) |> ExpressionEvaluator)
        |> Result.map (fun (value, memory) ->
               let updatedMemory: Memory = (variable, CellData.OfValue value) ||> (SetVariable memory)
               (value, updatedMemory)
           )
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for an <c>AnonymousFunction</c>.</p>
    /// </summary>
    let PlotEvaluator: Evaluator<AnonymousFunction, unit> = (fun (anonymousFunction, memory) ->
        let bakedEval: ValueType -> ValueType Result = (fun input ->
            let scopedMemory: Memory = SetVariable memory anonymousFunction.parameter (CellData.OfValue input)
            ((anonymousFunction.expression, scopedMemory) |> ExpressionEvaluator)
            |> Result.map fst
        )

        memory.plotCallback bakedEval
        ((), memory) |> Ok
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <i>structured function definition</i>.</p>
    /// </summary>
    let FunctionDefinitionEvaluator: Evaluator<FunctionType, unit> = (fun ((fnAttrs, fnBody), memory) ->
        let fn: FunctionType = (fnAttrs, fnBody)
        let flattenedFn: FunctionType Result =
            if fnAttrs.metadata.inlined then
                ((fn, memory) ||> FlattenFunction)
            else
                fn |> Ok

        flattenedFn
        |> Result.map (fun fn ->
               let newState: Memory = (memory, fnAttrs.identifier, (CellData.OfFunction fn)) |||> SetVariable
               match fnAttrs.metadata.symbol with
                | None     -> ((), newState)
                | Some sym ->
                    let newState: Memory = (sym, fn) |> (UpdateSymbol newState)
                    ((), newState)
           )
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for an <c>ASTNode</c>.</p>
    /// </summary>
    let ASTNodeEvaluator: Evaluator<ASTNode, ValueType option> = (fun (node, memory) ->
        match node with
         | ASTNode.Expression         expression             ->
             ((expression, memory) |> ExpressionEvaluator)
             |> Result.map (fun (value, memory) -> (Some value, memory))
         | ASTNode.PlotFunction       anonymousFunction      ->
             ((anonymousFunction, memory) |> PlotEvaluator)
             |> Result.map (fun (_, memory) -> (None, memory))
         | ASTNode.Assignment         (variable, expression) ->
             (((variable, expression), memory) |> AssignmentEvaluator)
             |> Result.map (fun (value, memory) -> (Some value, memory))
         | ASTNode.FunctionDefinition functionDefinition     ->
             ((functionDefinition, memory) |> FunctionDefinitionEvaluator)
             |> Result.map (fun (_, memory) -> (None, memory))
    )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for an <c>AST</c>.</p>
    /// </summary>
    let rec ASTEvaluator: Evaluator<AST, ValueType list> = (fun (tree, memory) ->
        match tree with
         | []           -> ([], memory) |> Ok
         | head :: tail ->
            match ((head, memory) |> ASTNodeEvaluator) with
            | Ok (Some value, newMemory) -> // add on the value to the list
                ((tail, newMemory) |> ASTEvaluator)
                |> Result.map (fun (values, memory) -> (value :: values, memory))
            | Ok (None, newMemory)       -> // move onto the next subtree
                ((tail, newMemory) |> ASTEvaluator)
            | Error err                  -> // error
                Error err
    )
