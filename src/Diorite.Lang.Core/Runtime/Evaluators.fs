// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Evaluators.fs
// Summary: Implementations of the Evaluator type
// Author:  Arsngrobg, Borngle
// Version: v1.6
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Errors
open Diorite.Lang.Core.Runtime
open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Runtime.RuleMappings

/// <summary>
///     <p>The implementations of the <c>Evaluator</c> type.</p>
/// </summary>
module Evaluators =
    let rec ExpressionEvaluator: Evaluator<Expression> = fun expression memory ->
        match expression with
         | Expression.Value           value               -> (value,               memory) ||> ValueEvaluator
         | Expression.Variable        var                 -> (var,                 memory) ||> VariableEvaluator
         | Expression.BinaryOperation (l, o, r)           -> ((l, o, r),           memory) ||> BinaryOperationEvaluator
         | Expression.UnaryOperation  (operand, operator) -> ((operand, operator), memory) ||> UnaryOperationEvaluator
         | Expression.FunctionCall    (fnRef, fnArgs)     -> ((fnRef, fnArgs),     memory) ||> FunctionCallEvaluator

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <c>ValueType</c>.</p>
    ///     <p>It is an identity function.</p>
    /// </summary>
    and ValueEvaluator: Evaluator<ValueType> = fun value memory ->
        (value, memory) |> Ok

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <c>VariableType</c>.</p>
    ///     <p>If the <c>VariableType</c> references a <c>FunctionType</c>, this <c>Evaluator</c> returns a
    ///        <c>MathError</c>.
    ///     </p>
    /// </summary>
    and VariableEvaluator: Evaluator<VariableType> = fun var memory ->
        match (GetVariable memory var) with
         | CellData.OfValue    value -> (value, memory) ||> ValueEvaluator
         | CellData.OfFunction _     -> (Some "Expected value - got function instead", []) ||> MathError

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a structured binary operation.</p>
    /// </summary>
    and BinaryOperationEvaluator: Evaluator<Expression * BinaryOperator * Expression> = fun (l, o, r) memory ->
        ((l, memory) ||> ExpressionEvaluator) |> Result.bind (fun (l, memory) ->
        ((r, memory) ||> ExpressionEvaluator) |> Result.bind (fun (r, memory) ->
            (l, r) |> (GetBinaryRule o) |> Result.map (fun value -> (value, memory))
        ))

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a structured unary operation.</p>
    /// </summary>
    and UnaryOperationEvaluator: Evaluator<Expression * UnaryOperator> = fun (operand, operator) memory ->
        ((operand, memory) ||> ExpressionEvaluator) |> Result.bind (fun (operand, memory) ->
            operand |> (GetUnaryRule operator) |> Result.map (fun value -> (value, memory))
        )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a structured function call.</p>
    /// </summary>
    and FunctionCallEvaluator: Evaluator<FunctionReferenceType * Expression list> = fun (fnRef, fnArgs) memory ->
        match (GetFunctionFromRef memory fnRef) with
         | None                   -> (Some "Function reference does not point to a function", []) ||> MathError
         | Some (fnAttrs, fnBody) ->
             let CollectArgs (rawArgs: Expression list): (ValueType list * Memory) Result =
                 rawArgs
                 |> List.fold (fun acc exp ->  // evaluate expressions from left to right and thread the memory through
                        acc |> Result.bind (fun (_args, mem) ->
                                   ((exp, mem) ||> ExpressionEvaluator) |> Result.map (fun (_arg, mem) ->
                                       (_arg :: _args, mem)
                                   )
                               )
                    )
                    (Ok ([], memory))
                 |> Result.map (fun (args, mem) -> // reverse the argument list to get correct sequence of values
                        (List.rev args, mem)
                    )

             (CollectArgs fnArgs) |> Result.bind (fun (args, memory) ->
                 let pairs: (VariableType * CellData) list =
                     args
                     |> List.map CellData.OfValue
                     |> List.zip (List.map fst fnAttrs.parameters)
                 let state: Memory = SetVariables memory pairs
                 // memory state ignored as scoped to function
                 ((fnBody, state) ||> FunctionBodyEvaluator) |> Result.map (fun (result, _) ->
                     (result, memory)
                 )
             )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <c>FunctionBody</c>.</p>
    /// </summary>
    and FunctionBodyEvaluator: Evaluator<FunctionBody> = fun fnBody memory ->
        match fnBody with
         | FunctionBody.Expression e           -> (e,  memory) ||> ExpressionEvaluator
         | FunctionBody.PiecewiseConditions cs -> (cs, memory) ||> PiecewiseConditionsEvaluator

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a sequence of <c>PiecewiseCondition</c>s.</p>
    /// </summary>
    and PiecewiseConditionsEvaluator: Evaluator<PiecewiseCondition list> = fun conditions memory ->
        match conditions with
         | []           -> failwith "No PiecewiseConditions given"
         | [pwc]        -> (pwc, memory) ||> PiecewiseConditionEvaluator
         | head :: tail ->
             ((head, memory) ||> PiecewiseConditionEvaluator)
             |> Result.bind (fun (value, memory) ->
                 match value with
                 | ValueType.Number x -> if x |> System.Double.IsNaN then
                                             (tail, memory) ||> PiecewiseConditionsEvaluator
                                         else (ValueType.Number x, memory) |> Ok
                 | value              -> (value, memory) ||> ValueEvaluator
             )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <c>PiecewiseCondition</c>.</p>
    ///     <p>It returns <c>NaN</c> if comparison is <c>false</c>.</p>
    /// </summary>
    and PiecewiseConditionEvaluator: Evaluator<PiecewiseCondition> = fun (ifTrue, (l, o, r)) memory ->
        ((l, memory) ||> ExpressionEvaluator) |> Result.bind (fun (l, memory) ->
        ((r, memory) ||> ExpressionEvaluator) |> Result.bind (fun (r, memory) ->
            (l, r) |> (GetComparisonRule o) |> Result.bind (fun b ->
                if not b then (ConstantNaN, memory) |> Ok
                else (ifTrue, memory) ||> FunctionResultEvaluator
            )
        ))

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <c>FunctionResult</c>.</p>
    /// </summary>
    and FunctionResultEvaluator: Evaluator<FunctionResult> = fun fnResult memory ->
        match fnResult with
         | FunctionResult.Error      err -> (err, []    ) ||> MathError
         | FunctionResult.Expression exp -> (exp, memory) ||> ExpressionEvaluator

    /// <summary>
    ///     <p>The <c>Evaluator</c> for an <c>AnonymousFunction</c>.</p>
    /// </summary>
    let AnonymousFunctionEvaluator: Evaluator<AnonymousFunction> = fun anon memory ->
        let bakedEval: ValueType -> ValueType = fun value ->
            let state: Memory = SetVariable memory (anon.parameter, CellData.OfValue value)
            match ((anon.expression, state) ||> ExpressionEvaluator) with
             | Error _           -> ValueType.Undefined // safe since cannot plot undefined
             | Ok    (result, _) -> result

        memory.plotCallback bakedEval
        (ValueType.Undefined, memory) |> Ok

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a structured assignment.</p>
    /// </summary>
    let AssignmentEvaluator: Evaluator<VariableType * Expression> = fun (v, e) memory ->
        ((e, memory) ||> ExpressionEvaluator)
        |> Result.map (fun (result, memory) ->
               let newState: Memory = SetVariable memory (v, CellData.OfValue result)
               (result, newState)
           )

    /// <summary>
    ///     <p>The <c>Evaluator</c> for a <c>FunctionType</c>.</p>
    /// </summary>
    let FunctionDefinitionEvaluator: Evaluator<FunctionType> = fun (fnAttrs, fnBody) memory ->
        let newState: Memory = SetVariable memory (fnAttrs.identifier, CellData.OfFunction (fnAttrs, fnBody))
        match fnAttrs.metadata.symbol with
         | Some sym ->
             let newState: Memory = UpdateSymbol newState (sym, (fnAttrs, fnBody))
             (ValueType.Undefined, newState) |> Ok
         | None -> (ValueType.Undefined, newState) |> Ok

    /// <summary>
    ///     <p>The <c>Evaluator</c> for an <c>ASTNode</c>.</p>
    /// </summary>
    let ASTNodeEvaluator: Evaluator<ASTNode> = fun node memory ->
        match node with
         | ASTNode.Expression   e                       -> (e,                 memory) ||> ExpressionEvaluator
         | ASTNode.PlotFunction e                       -> (e,                 memory) ||> AnonymousFunctionEvaluator
         | ASTNode.Assignment   (v, e)                  -> ((v, e),            memory) ||> AssignmentEvaluator
         | ASTNode.FunctionDefinition (fnAttrs, fnBody) -> ((fnAttrs, fnBody), memory) ||> FunctionDefinitionEvaluator

    /// <summary>
    ///     <p>The top-level function for evaluating an Abstract Syntax Tree.</p>
    /// </summary>
    /// <param name="tree"> the AST to evaluate </param>
    /// <param name="memory"> the <c>Memory</c> state to use </param>
    /// <returns> a sequence of <c>Result</c>s from each node evaluated </returns>
    let rec EvaluateTree (tree: AST) (memory: Memory): (ValueType * Memory) Result list =
        match tree with
         | []           -> []
         | head :: tail ->
             let result = ((head, memory) ||> ASTNodeEvaluator)
             match result with
              | Ok (result, memory) ->
                  if head.IsPlotFunction || head.IsFunctionDefinition then
                                                                        (EvaluateTree tail memory)
                  else                        Ok    (result, memory) :: (EvaluateTree tail memory)
              | Error err ->                  Error err              :: (EvaluateTree tail memory)
