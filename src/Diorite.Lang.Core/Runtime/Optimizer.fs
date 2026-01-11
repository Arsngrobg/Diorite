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
    let rec FlattenExpression (expression: Expression) (memory: Memory) (fnAttrs: FunctionAttributes): Expression Result =
        let reducedParams: VariableType list = fnAttrs.parameters |> (List.map fst)
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
             ((l, memory, fnAttrs) |||> FlattenExpression) |> Result.bind (fun l' ->
             ((r, memory, fnAttrs) |||> FlattenExpression) |> Result.bind (fun r' ->
                 match (l', r') with
                  | Expression.Value l', Expression.Value r' ->
                      ((l', r') |> (GetBinaryRule o)) |> Result.map Expression.Value
                  | l,                  r                    -> (l, o, r) |> (Expression.BinaryOperation >> Ok)
             ))
         | Expression.UnaryOperation (operand, operator) ->
             ((operand, memory, fnAttrs) |||> FlattenExpression) |> Result.bind (fun operand' ->
                 match operand' with
                  | Expression.Value operand' ->
                      (operand' |> (GetUnaryRule operator)) |> Result.map Expression.Value
                  | operand'                  -> (operand', operator) |> (Expression.UnaryOperation >> Ok)
             )
         | expression -> expression |> Ok

    let FlattenFnResult (fnResult: FunctionResult) (memory: Memory) (fnAttrs: FunctionAttributes): FunctionResult Result =
        match fnResult with
         | FunctionResult.Expression exp -> ((exp, memory, fnAttrs) |||> FlattenExpression)
                                            |> Result.map FunctionResult.Expression
         | fnResult                      -> fnResult |> Ok

    let FlattenComparisonOperation (cmpOp: ComparisonOperation) (memory: Memory) (fnAttrs: FunctionAttributes): ComparisonOperation Result =
        let (l: Expression), (o: ComparisonOperator), (r: Expression) = cmpOp
        ((l, memory, fnAttrs) |||> FlattenExpression) |> Result.bind (fun l ->
        ((r, memory, fnAttrs) |||> FlattenExpression) |> Result.map  (fun r ->
            (l, o, r)
        ))

    let FlattenPWCondition (pwc: PiecewiseCondition) (memory: Memory) (fnAttrs: FunctionAttributes): PiecewiseCondition Result =
        let (fnResult: FunctionResult), (cmpOp: ComparisonOperation) = pwc
        ((fnResult, memory, fnAttrs) |||> FlattenFnResult)            |> Result.bind (fun fnResult ->
        ((cmpOp,    memory, fnAttrs) |||> FlattenComparisonOperation) |> Result.map  (fun cmpOp    ->
            (fnResult, cmpOp)
        ))

    let rec FlattenPWConditions (pwcs: PiecewiseCondition list) (memory: Memory) (fnAttrs: FunctionAttributes): PiecewiseCondition list Result =
        match pwcs with
         | []           -> failwith "No PiecewiseConditions supplied to EvalPWConditions"
         | [pwc]        -> (pwc, memory, fnAttrs) |||> FlattenPWCondition |> Result.map (fun pwc -> [pwc])
         | head :: tail ->
             ((head, memory, fnAttrs) |||> FlattenPWCondition)
             |> Result.bind (fun pwc ->
                    (tail, memory, fnAttrs) |||> FlattenPWConditions
                    |> Result.map (fun pwcsTail -> pwc :: pwcsTail)
                )

    let FlattenFunction (fn: FunctionType) (memory: Memory): FunctionType Result =
        let (fnAttrs: FunctionAttributes), (fnBody: FunctionBody) = fn
        match fnBody with
         | FunctionBody.Expression          exp -> ((exp, memory, fnAttrs) |||> FlattenExpression)
                                                   |> Result.map (fun e -> (fnAttrs, FunctionBody.Expression e))
         | FunctionBody.PiecewiseConditions pwc -> ((pwc, memory, fnAttrs) |||> FlattenPWConditions)
                                                   |> Result.map (fun pwc -> (fnAttrs, FunctionBody.PiecewiseConditions pwc))
