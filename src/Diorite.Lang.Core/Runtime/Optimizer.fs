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

    // let FlattenFunction (fn: FunctionType) (memory: Memory): FunctionType Result =
    //     let (fnAttrs: FunctionAttributes), (fnBody: FunctionBody) = fn
    //     match fnBody with
    //      | FunctionBody.Expression          exp -> ((exp, memory, fnAttrs) |||> FlattenExpression)
    //                                                |> Result.map (fun e -> (fnAttrs, FunctionBody.Expression e))
    //      | FunctionBody.PiecewiseConditions pwc -> ((pwc, memory, fnAttrs) |||> FlattenPiecewiseConditions)
    //                                                |> Result.map (fun pwc -> (fnAttrs, FunctionBody.PiecewiseConditions pwc))
