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
open Diorite.Lang.Core.Lexer
open Diorite.Lang.Core.Parser
open Diorite.Lang.Core.Runtime
open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Runtime.RuleMappings

/// <summary>
///     <p>The <c>Evaluation</c> module contains functions for evaluating structured syntax such as <c>Expression</c> or
///        <c>PiecewiseCondition list</c> for functions.
///     </p>
/// </summary>
module Evaluation =
    /// <summary>
    ///     <p>A type to visually mark that this function only returns a <c>ValueType</c>, hence memory is not mutated
    ///        between calls, hence only a <c>ValueType</c> is returned.
    ///     </p>
    /// </summary>
    type ReadOnly       = ValueType Result
    /// <summary>
    ///     <p>A type to visually mark that this function only returns a <c>Memory</c> context, hence memory is written
    ///        to <b>only</b>.
    ///     </p>
    /// </summary>
    type WriteOnly      = Memory
    /// <summary>
    ///     <p>A type to visually mark that this function may return a value, and potentially modified <c>Memory</c>
    ///        record.
    ///     </p>
    /// </summary>
    type ReadAndWrite   = (ValueType option * Memory) Result
    /// <summary>
    ///     <p>A type to visually mark that this function returns a definite value &amp; mutated state.</p>
    /// </summary>
    type DefiniteResult = (ValueType * Memory) Result

    /// <summary>
    ///     <p>Evaluates the incoming <c>ValueType</c>.</p>
    ///     <p>This is the identity function.</p>
    /// </summary>
    /// <param name="value"> the <c>ValueType</c> to evaluate </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    let EvalValue (value: ValueType): ReadOnly =
        value |> Ok

    /// <summary>
    ///     <p>Evaluates the incoming <c>VariableType</c>.</p>
    ///     <p>This function assumes the intended use is unwrapping a value from a variable, hence will return a
    ///        <c>MathError</c> if the data stored in the location defined by the <c>VariableType</c> is a
    ///        <c>FunctionType</c> and not a <c>ValueType</c>.
    ///     </p>
    /// </summary>
    /// <param name="variable"> the <c>VariableType</c> to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    let EvalVariable (variable: VariableType) (memory: Memory): ReadOnly =
        match (GetVariable memory variable) with
         | CellData.OfValue    value        -> value |> EvalValue
         | CellData.OfFunction (fnAttrs, _) ->
             let functionStr: string =
                 fnAttrs.parameters
                 |> List.map  fst
                 |> List.map  (fun (c, s) -> if s = 0uy then $"{c}" else $"{c}{s-1uy}")
                 |> String.concat ", "
                 |> (fun paramStr -> $"{fnAttrs.parameters}({paramStr})")
             (Some $"Expected ValueType got FunctionType instead ({functionStr})", []) ||> MathError

    /// <summary>
    ///     <p>Evaluates the <c>ValueType</c> as if it has membership in the <c>Natural</c> number set.</p>
    /// </summary>
    /// <param name="value"> the <c>ValueType</c> to check for set membership </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    let EvalSetNatural (value: ValueType): ReadOnly =
        match value with
         | ValueType.Number x when (x = (System.Math.Truncate x)) && (x >= 0) ->
             EvalValue (ValueType.Number x)
         | _                                                                  ->
             (Some "Expected Natural (N) number set membership", [value]) ||> MathError

    /// <summary>
    ///     <p>Evaluates the <c>ValueType</c> as if it has membership in the <c>Integer</c> number set.</p>
    /// </summary>
    /// <param name="value"> the <c>ValueType</c> to check for set membership </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    let EvalSetInteger (value: ValueType): ReadOnly =
        match value with
         | ValueType.Number x when (x = (System.Math.Truncate x)) ->
             (ValueType.Number x) |> EvalValue
         | _                                                      ->
             (Some "Expected Integer (I) number set membership", [value]) ||> MathError

    /// <summary>
    ///     <p>Evaluates the <c>ValueType</c> as if it has membership in the <c>Real</c> number set.</p>
    /// </summary>
    /// <param name="value"> the <c>ValueType</c> to check for set membership </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    let EvalSetReal (value: ValueType): ReadOnly =
        match value with
         | ValueType.Number x ->
             (ValueType.Number x) |> EvalValue
         | _                  ->
             (Some "Expected Real (R) number set membership", [value]) ||> MathError

    /// <summary>
    ///     <p>Evaluates the <c>ValueType</c> as if it has membership in the <c>Rational</c> number set.</p>
    /// </summary>
    /// <param name="value"> the <c>ValueType</c> to check for set membership </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    let EvalSetRational (value: ValueType): ReadOnly =
        match value with
         | ValueType.Number x ->
             (ValueType.Number x) |> EvalValue
         | _                  ->
             (Some "Expected Rational (Q) number set membership", [value]) ||> MathError

    /// <summary>
    ///     <p>Evaluates the <c>ValueType</c> as if it has membership in the <c>Irrational</c> number set.</p>
    /// </summary>
    /// <param name="value"> the <c>ValueType</c> to check for set membership </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    let EvalSetIrrational (value: ValueType): ReadOnly =
        // TODO: heuristic
        (Some "Expected Irrational (I) number set membership", [value]) ||> MathError

    /// <summary>
    ///     <p>Evaluates the <c>ValueType</c> as if it has membership in the <c>Complex</c> number set.</p>
    /// </summary>
    /// <param name="value"> the <c>ValueType</c> to check for set membership </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    let EvalSetComplex (value: ValueType): ReadOnly =
        value |> EvalValue // every value is complex

    /// <summary>
    ///     <p>Evaluates the structured pairing of a <c>VariableType</c> &amp; <c>NumberSet</c>.</p>
    /// </summary>
    /// <param name="value"> the <c>ValueType</c> to check for set membership </param>
    /// <param name="set"> the <c>NumberSet</c> to chack <c>ValueType</c> against </param>
    /// <returns> the <c>ValueType</c> upon successful validation of the set membership </returns>
    let EvalSetMembership (value: ValueType, set: NumberSet): ReadOnly =
        match set with
         | NumberSet.Natural    -> value |> EvalSetNatural
         | NumberSet.Integer    -> value |> EvalSetInteger
         | NumberSet.Real       -> value |> EvalSetReal
         | NumberSet.Rational   -> value |> EvalSetRational
         | NumberSet.Irrational -> value |> EvalSetIrrational
         | NumberSet.Complex    -> value |> EvalSetComplex

    /// <summary>
    ///     <p>Evaluates the incoming a structured binary operation.</p>
    /// </summary>
    /// <param name="binOp"> the structured binary operation to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    let rec EvalBinOp (binOp: Expression * BinaryOperator * Expression) (memory: Memory): ReadOnly =
        let (l: Expression), (o: BinaryOperator), (r: Expression) = binOp
        ((l, memory) ||> EvalExpression) |> Result.bind (fun l ->
        ((r, memory) ||> EvalExpression) |> Result.bind (fun r ->
            ((l, r) |> (GetBinaryRule o))
        ))

    /// <summary>
    ///     <p>Evaluates the incoming structured unary operation.</p>
    /// </summary>
    /// <param name="unOp"> the structured unary operation to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    and EvalUnOp (unOp: Expression * UnaryOperator) (memory: Memory): ReadOnly =
        let (operand: Expression), (operator: UnaryOperator) = unOp
        ((operand, memory) ||> EvalExpression) |> Result.bind (fun operand ->
            (operand |> (GetUnaryRule operator))
        )

    /// <summary>
    ///     <p>Evaluates the incoming structured function call.</p>
    /// </summary>
    /// <param name="fnCall"> the structured function call to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    and EvalFnCall (fnCall: FunctionReferenceType * Expression list) (memory: Memory): ReadOnly =
        let (fnRef: FunctionReferenceType), (fnArgs: Expression list) = fnCall
        match (GetFunctionFromRef memory fnRef) with
         | None                   -> (
                                      Some $"Function reference {fnRef} does not point to a real function in memory",
                                      []
                                     ) ||> MathError
         | Some (fnAttrs, fnBody) ->
             let argDiff: int = fnAttrs.parameters.Length - fnArgs.Length
             if argDiff < 0 then
                 (Some $"Missing {-argDiff} position arguments for function: {fnRef}", []) ||> MathError
             elif argDiff > 0 then
                 (Some $"Too many arguments supplied to function: {fnRef}", []) ||> MathError
             else
             (fnArgs, List.map snd fnAttrs.parameters)
             ||> List.map2  (fun exp set ->     // evaluate all expressions
                     ((exp, memory) ||> EvalExpression)
                     |> Result.bind (fun value -> (value, set) |> EvalSetMembership)
                 )
             |> List.fold   (fun acc result ->                            // first occurrence of error - fail
                    match (acc, result) with
                     | Ok    vs, Ok    v -> Ok    (v::vs)
                     | Error e,  _
                     | _,        Error e -> Error e
                )
                (Ok [])
             |> Result.map  List.rev                                      // reverse as fold produces reversed list
             |> Result.bind (fun fnArgs ->
                    fnArgs 
                    |> List.map CellData.OfValue
                    |> List.zip (List.map fst fnAttrs.parameters)         // pair up args to their variables
                    |> (fun pairs ->                                      // map args to each variable in memory
                           let scopedMemory: Memory = SetVariables memory pairs
                           ((fnBody, scopedMemory) ||> EvalFnBody)
                           |> Result.mapError (fun err ->
                                  match err with
                                   | MathError (msg, _) -> (msg, fnArgs) |> DioriteError.MathError
                                   | err                -> err
                              )
                           |> Result.bind (fun value -> EvalSetMembership (value, fnAttrs.range))
                        )
                )

    /// <summary>
    ///     <p>Evaluates the incoming <c>FunctionBody</c>.</p>
    /// </summary>
    /// <param name="fnBody"> the <c>FunctionBody</c> to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    and EvalFnBody (fnBody: FunctionBody) (memory: Memory): ReadOnly =
        match fnBody with
         | FunctionBody.Expression          exp        -> (exp,        memory) ||> EvalExpression
         | FunctionBody.PiecewiseConditions conditions -> (conditions, memory) ||> EvalPWConditions

    /// <summary>
    ///     <p>Evaluates the incoming structured piecewise condition sequence.</p>
    ///     <p>If no conditions are supplied, then this function fails with a fatal error.</p>
    /// </summary>
    /// <param name="conditions"> the structured piecewise condition sequence to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    and EvalPWConditions (conditions: PiecewiseCondition list) (memory: Memory): ReadOnly =
        match conditions with
         | []              -> failwith "No PiecewiseConditions supplied to EvalPWConditions"
         | [(baseCase, _)] -> (baseCase, memory) ||> EvalFnResult
         | head :: tail    ->
             ((head, memory) ||> EvalPWCondition)
             |> Result.bind (fun result ->
                    match result with
                     | ValueType.Number x when x |> System.Double.IsNaN -> EvalPWConditions tail memory
                     | result                                           -> result |> EvalValue
                )

    /// <summary>
    ///     <p>Evaluates the incoming <c>PiecewiseCondition</c>.</p>
    /// </summary>
    /// <param name="condition"> the <c>PiecewiseCondition</c> to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    and EvalPWCondition (condition: PiecewiseCondition) (memory: Memory): ReadOnly =
        let (fnResult: FunctionResult), (cmpOp: ComparisonOperation) = condition
        let (l: Expression), (o: ComparisonOperator), (r: Expression) = cmpOp
        ((l, memory) ||> EvalExpression) |> Result.bind (fun l ->
        ((r, memory) ||> EvalExpression) |> Result.bind (fun r ->
            ((l, r) |> (GetComparisonRule o))
            |> Result.bind (fun b ->
                   if not b then ConstantNaN |> Ok
                   else          (fnResult, memory) ||> EvalFnResult
               )
        ))

    /// <summary>
    ///     <p>Evaluates the incoming <c>FunctionResult</c>.</p>
    /// </summary>
    /// <param name="fnResult"> the <c>FunctionResult</c> to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    and EvalFnResult (fnResult: FunctionResult) (memory: Memory): ReadOnly =
        match fnResult with
         | FunctionResult.Error      err -> (err, [])     ||> MathError
         | FunctionResult.Expression exp -> (exp, memory) ||> EvalExpression

    /// <summary>
    ///     <p>Evaluates the incoming <c>Expression</c>.</p>
    /// </summary>
    /// <param name="expression"> the <c>Expression</c> to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation </returns>
    and EvalExpression (expression: Expression) (memory: Memory): ReadOnly =
        match expression with
         | Expression.Value           value           -> value                      |> EvalValue
         | Expression.Variable        variable        -> (variable,        memory) ||> EvalVariable
         | Expression.BinaryOperation (l, o, r)       -> ((l, o, r),       memory) ||> EvalBinOp
         | Expression.UnaryOperation  (op, o)         -> ((op, o),         memory) ||> EvalUnOp
         | Expression.FunctionCall    (fnRef, fnArgs) -> ((fnRef, fnArgs), memory) ||> EvalFnCall

    /// <summary>
    ///     <p>Evaluates the incoming structured variable assignment.</p>
    /// </summary>
    /// <param name="assignment"> the structured variable assignment to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation &amp; the mutated memory state </returns>
    let EvalAssign (assignment: VariableType * Expression) (memory: Memory): ReadAndWrite =
        let (variable: VariableType), (expression: Expression) = assignment
        ((expression, memory) ||> EvalExpression)
        |> Result.map (fun result ->
               let newState: Memory = SetVariable memory (variable, CellData.OfValue result)
               (Some result, newState)
           )

    /// <summary>
    ///     <p>Evaluates the incoming <c>AnonymousFunction</c>.</p>
    ///     <p>This invokes the <c>plotCallback</c> function registered in <b>Diorite</b>'s <c>VirtualMemory</c>.</p>
    ///     <p><i>This function evaluates but returns nothing as state is local to the evaluation, and values are
    ///           explicitly handled by the user's callback function.
    ///     </i></p>
    /// </summary>
    /// <param name="anonymousFunction"> the <c>AnonymousFunction</c> to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    let EvalFnPlot (anonymousFunction: AnonymousFunction) (memory: Memory): unit =
        let bakedEval: ValueType -> ReadOnly = (fun input ->
            let scopedMemory: Memory = (anonymousFunction.parameter, CellData.OfValue input) |> (SetVariable memory)
            (anonymousFunction.expression, scopedMemory) ||> EvalExpression
        )

        memory.plotCallback bakedEval

    /// <summary>
    ///     <p>Evaluates the incoming <c>FunctionType</c>.</p>
    /// </summary>
    /// <param name="fn"> the <c>FunctionType</c> to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType</c> as a result of this evaluation &amp; the mutated memory state </returns>
    let EvalFnDef (fn: FunctionType) (memory: Memory): WriteOnly =
        let (fnAttrs: FunctionAttributes), _ = fn
        let newState: Memory = (fnAttrs.identifier,      CellData.OfFunction fn) |> (SetVariable  memory)
        match fnAttrs.metadata.symbol with
         | None     -> newState
         | Some sym ->
             let newState: Memory = (sym, fn) |> (UpdateSymbol newState)
             newState

    /// <summary>
    ///     <p>Evaluates the incoming <c>ASTNode</c>.</p>
    /// </summary>
    /// <param name="node"> the <c>ASTNode</c> to evaluate </param>
    /// <param name="memory"> the stateful context to reference from </param>
    /// <returns> a <c>ValueType option</c> as a result of this evaluation &amp; the mutated <c>Memory</c></returns>
    let EvalNode (node: ASTNode) (memory: Memory): ReadAndWrite =
        // helper functions to map to ReadAndWrite type
        let MapReadOnly:  ReadOnly  -> ReadAndWrite = Result.map (fun v -> (Some v, memory))
        let MapWriteOnly: WriteOnly -> ReadAndWrite = fun m -> (None, m)      |> Ok
        let MapUnit:      unit      -> ReadAndWrite = fun _ -> (None, memory) |> Ok

        match node with
         | ASTNode.Expression         exp        -> (exp,        memory) ||> EvalExpression |> MapReadOnly
         | ASTNode.PlotFunction       afn        -> (afn,        memory) ||> EvalFnPlot     |> MapUnit
         | ASTNode.Assignment         (var, exp) -> ((var, exp), memory) ||> EvalAssign
         | ASTNode.FunctionDefinition fn         -> (fn,         memory) ||> EvalFnDef      |> MapWriteOnly

    /// <summary>
    ///     <p>Evaluates the incoming <c>AST</c>.</p>
    /// </summary>
    /// <param name="root"> the <c>AST</c> to evaluate </param>
    /// <param name="memory"> the stateful context to initially reference from (propagated through chain) </param>
    /// <returns> a sequence of <c>(ValueType * Memory) Result</c>s - <c>None</c> results are ignored</returns>
    let rec EvalTree (root: AST) (memory: Memory): DefiniteResult list =
        match root with
         | []           -> []
         | head :: tail ->
             match ((head, memory) ||> EvalNode) with
              | Ok    (None,   memory) ->                        ((tail, memory) ||> EvalTree)
              | Ok    (Some v, memory) -> (Ok    (v, memory)) :: ((tail, memory) ||> EvalTree)
              | Error err              -> (Error err)         :: ((tail, memory) ||> EvalTree)

    /// <summary>
    ///     <p>Evaluates the incoming <c>string</c>.</p>
    ///     <p>Applies the tokenisation and parsing pipelines to the input <c>string</c>.</p>
    /// </summary>
    /// <param name="source"> the <c>string</c> to evaluate. </param>
    /// <param name="memory"> the stateful context to initially reference from (propagated through chain) </param>
    /// <returns> a sequence of <c>(ValueType * Memory) Result</c>s - <c>None</c> results are ignored</returns>
    let EvalString (source: string) (memory: Memory): DefiniteResult list =
        let tokens: TokenStream = source |> Tokenise
        match (GetTokenizerError tokens) with
         | Some err -> [Error err]
         | None     ->
             match (ParseTokens tokens) with
              | Error err  -> [Error err]
              | Ok    tree -> (EvalTree tree memory)
