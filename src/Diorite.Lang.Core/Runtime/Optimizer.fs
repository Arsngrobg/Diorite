// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Optimizer.fs
// Summary: The optimizer for Diorite code
// Author:  Arsngrobg, Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Errors
open Diorite.Lang.Core.Runtime.RuleMappings

/// <summary>
///     <p>The <c>Optimizer</c> module contains bindings related to optimising certain aspects of <b>Diorite</b>.</p>
/// </summary>
module Optimizer =
    /// <summary>
    ///     <p>The return result of the <c>Bounce</c> type.</p>
    /// </summary>
    type BounceResult = (ValueType * Memory) Result

    /// <summary>
    ///     <p>The <c>Bounce</c> type is an optimiser type for <b>Diorite</b> evaluations calls. Specifically, for
    ///        recursive function calls.
    ///     </p>
    ///     <p>It ensures that it reuses the same call stack upon every recursive evaluation in <b>Diorite</b>.</p>
    /// </summary>
    type Bounce =
        /// <summary>
        ///     <p>The case where the optimised recursive function has finished executing.</p>
        /// </summary>
        | Done of BounceResult
        /// <summary>
        ///     <p>The case where the optimised recursive function has produced a deferred function call.</p>
        /// </summary>
        | Call of (unit -> Bounce)

    /// <summary>
    ///     <p>Recursively invokes the <c>Bounce</c>.</p>
    ///     <p>It repeatedly invokes the <c>Bounce</c> until a <c>Done</c> case is reached.</p>
    /// </summary>
    /// <param name="bounce"> the <c>Bounce</c> to evaluate </param>
    /// <returns> the return result of the optimised evaluator function </returns>
    let rec EvaluateBounce (bounce: Bounce): BounceResult =
        match bounce with
         | Bounce.Done result -> result
         | Bounce.Call thunk  -> (thunk ()) |> EvaluateBounce

    /// <summary>
    ///     <p>Flattens the <c>Expression</c> to its smallest structure possible.</p>
    ///     <p>Requires the current <c>Memory</c> context in order to make the correct decisions when flattening the
    ///        tree structure.
    ///     </p>
    ///     <p>Variables that are <i>not</i> parameters are inlined into this <c>Expression</c>.</p>
    /// </summary>
    /// <param name="expression"> the <c>Expression</c> to flatten </param>
    /// <param name="memory"> the current <c>Memory</c> context to reference from </param>
    /// <param name="fnParams"> the function's parameter sequence </param>
    /// <returns> the most optimal representation of this <c>Expression</c>, given the <c>Memory</c> state </returns>
    let rec FlattenExpression (expression: Expression) (memory: Memory) (fnParams: FunctionParameter list): Expression Result =
        let reducedParams: VariableType list = fnParams |> (List.map fst)
        match expression with
         | Expression.Variable variable ->
              let isParameter: bool = reducedParams |> (List.contains variable)
              if isParameter then
                  variable |> (Expression.Variable >> Ok)
              else
                  let cell: CellData = variable |> (GetVariable memory)
                  match cell with
                   | CellData.OfFunction _ -> variable |> (Expression.Variable >> Ok)
                   | CellData.OfValue    v -> v        |> (Expression.Value    >> Ok)
         | Expression.BinaryOperation (l, o, r) ->
             ((l, memory, fnParams) |||> FlattenExpression) |> Result.bind (fun l' ->
             ((r, memory, fnParams) |||> FlattenExpression) |> Result.bind (fun r' ->
                 match (l', r') with
                  | Expression.Value l', Expression.Value r' ->
                      ((l', r') |> (GetBinaryRule o)) |> Result.map Expression.Value
                  | l,                  r                    -> (l, o, r) |> (Expression.BinaryOperation >> Ok)
             ))
         | Expression.UnaryOperation (operand, operator) ->
             ((operand, memory, fnParams) |||> FlattenExpression) |> Result.bind (fun operand' ->
                 match operand' with
                  | Expression.Value operand' ->
                      (operand' |> (GetUnaryRule operator)) |> Result.map Expression.Value
                  | operand'                  -> (operand', operator) |> (Expression.UnaryOperation >> Ok)
             )
         | expression -> expression |> Ok

    /// <summary>
    ///     <p>Flattens the <c>FunctionResult</c> to its smallest structure possible.</p>
    ///     <p>Requires the current <c>Memory</c> context in order to make the correct decisions when flattening the
    ///        tree structure.
    ///     </p>
    ///     <p>Variables that are <i>not</i> parameters are inlined into this <c>FunctionResult</c>.</p>
    /// </summary>
    /// <param name="fnResult"> the <c>FunctionResult</c> to flatten </param>
    /// <param name="memory"> the current <c>Memory</c> context to reference from </param>
    /// <param name="fnParams"> the function's parameter sequence </param>
    /// <returns> the most optimal representation of this <c>FunctionResult</c>, given the <c>Memory</c> state </returns>
    let FlattenFnResult (fnResult: FunctionResult) (memory: Memory) (fnParams: FunctionParameter list): FunctionResult Result =
        match fnResult with
         | FunctionResult.Expression exp -> ((exp, memory, fnParams) |||> FlattenExpression)
                                            |> Result.map FunctionResult.Expression
         | fnResult                      -> fnResult |> Ok

    /// <summary>
    ///     <p>Flattens the <c>ComparisonOperation</c> to its smallest structure possible.</p>
    ///     <p>Requires the current <c>Memory</c> context in order to make the correct decisions when flattening the
    ///        tree structure.
    ///     </p>
    ///     <p>Variables that are <i>not</i> parameters are inlined into this <c>ComparisonOperation</c>.</p>
    /// </summary>
    /// <param name="cmpOp"> the <c>ComparisonOperation</c> to flatten </param>
    /// <param name="memory"> the current <c>Memory</c> context to reference from </param>
    /// <param name="fnParams"> the function's parameter sequence </param>
    /// <returns> the most optimal representation of this <c>ComparisonOperation</c>, given the <c>Memory</c> state </returns>
    let FlattenComparisonOperation (cmpOp: ComparisonOperation) (memory: Memory) (fnParams: FunctionParameter list): ComparisonOperation Result =
        let (l: Expression), (o: ComparisonOperator), (r: Expression) = cmpOp
        ((l, memory, fnParams) |||> FlattenExpression) |> Result.bind (fun l ->
        ((r, memory, fnParams) |||> FlattenExpression) |> Result.map  (fun r ->
            (l, o, r)
        ))

    /// <summary>
    ///     <p>Flattens the <c>PiecewiseCondition</c> to its smallest structure possible.</p>
    ///     <p>Requires the current <c>Memory</c> context in order to make the correct decisions when flattening the
    ///        tree structure.
    ///     </p>
    ///     <p>Variables that are <i>not</i> parameters are inlined into this <c>PiecewiseCondition</c>.</p>
    /// </summary>
    /// <param name="pwc"> the <c>PiecewiseCondition</c> to flatten </param>
    /// <param name="memory"> the current <c>Memory</c> context to reference from </param>
    /// <param name="fnParams"> the function's parameter sequence </param>
    /// <returns> the most optimal representation of this <c>PiecewiseCondition</c>, given the <c>Memory</c> state </returns>
    let FlattenPWCondition (pwc: PiecewiseCondition) (memory: Memory) (fnParams: FunctionParameter list): PiecewiseCondition Result =
        let (fnResult: FunctionResult), (cmpOp: ComparisonOperation) = pwc
        ((fnResult, memory, fnParams) |||> FlattenFnResult)            |> Result.bind (fun fnResult ->
        ((cmpOp,    memory, fnParams) |||> FlattenComparisonOperation) |> Result.map  (fun cmpOp    ->
            (fnResult, cmpOp)
        ))

    /// <summary>
    ///     <p>Flattens the <c>PiecewiseCondition</c>s to its smallest structure possible.</p>
    ///     <p>Requires the current <c>Memory</c> context in order to make the correct decisions when flattening the
    ///        tree structure.
    ///     </p>
    ///     <p>Variables that are <i>not</i> parameters are inlined into these <c>PiecewiseCondition</c>s.</p>
    /// </summary>
    /// <param name="pwcs"> the <c>PiecewiseCondition</c>s to flatten </param>
    /// <param name="memory"> the current <c>Memory</c> context to reference from </param>
    /// <param name="fnParams"> the function's parameter sequence </param>
    /// <returns> the most optimal representation of the <c>PiecewiseCondition</c>s, given the <c>Memory</c> state </returns>
    let rec FlattenPWConditions (pwcs: PiecewiseCondition list) (memory: Memory) (fnParams: FunctionParameter list): PiecewiseCondition list Result =
        match pwcs with
         | []           -> failwith "No PiecewiseConditions supplied to EvalPWConditions"
         | [pwc]        -> (pwc, memory, fnParams) |||> FlattenPWCondition |> Result.map (fun pwc -> [pwc])
         | head :: tail ->
             ((head, memory, fnParams) |||> FlattenPWCondition)
             |> Result.bind (fun pwc ->
                    (tail, memory, fnParams) |||> FlattenPWConditions
                    |> Result.map (fun pwcsTail -> pwc :: pwcsTail)
                )

    /// <summary>
    ///     <p>Flattens the <c>FunctionType</c> to its smallest structure possible.</p>
    ///     <p>Requires the current <c>Memory</c> context in order to make the correct decisions when flattening the
    ///        tree structure.
    ///     </p>
    ///     <p>Variables that are <i>not</i> parameters are inlined into this <c>FunctionType</c>.</p>
    /// </summary>
    /// <param name="fn"> the <c>FunctionType</c> to flatten </param>
    /// <param name="memory"> the current <c>Memory</c> context to reference from </param>
    /// <returns> the most optimal representation of the <c>FunctionType</c>, given the <c>Memory</c> state </returns>
    let FlattenFunction (fn: FunctionType) (memory: Memory): FunctionType Result =
        let (fnAttrs: FunctionAttributes), (fnBody: FunctionBody) = fn
        match fnBody with
         | FunctionBody.Expression          exp -> ((exp, memory, fnAttrs.parameters) |||> FlattenExpression)
                                                   |> Result.map (fun e -> (fnAttrs, FunctionBody.Expression e))
         | FunctionBody.PiecewiseConditions pwc -> ((pwc, memory, fnAttrs.parameters) |||> FlattenPWConditions)
                                                   |> Result.map (fun pwc -> (fnAttrs, FunctionBody.PiecewiseConditions pwc))
