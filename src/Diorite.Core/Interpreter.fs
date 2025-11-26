// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Interpreter.fs
// Summary: The interpreter for the Diorite language
// Author:  Arsngrobg, Borngle
// Version: v1.11
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

// typedefs
type private AST                = Parser.AST
type private BinaryOperator     = Parser.BinaryOperator
type private UnaryOperator      = Parser.UnaryOperator
type private ComparisonOperator = Parser.ComparisonOperator

[<RequireQualifiedAccess>]
module Interpreter =        
    [<RequireQualifiedAccess>]
    module Memory =
        type FunctionData = FunctionAttributes * AST

        type StorageType =
            | OfValue    of ValueType
            | OfFunction of FunctionData

        type VariableTable = StorageType array
        type SymbolTable   = Map<string, FunctionData>

        type Storage = {
            variables: VariableTable
            symbols:   SymbolTable
        }
        
        let initStorage (): Storage = {
            variables = Array.create (11 * supportedVariableChars) (StorageType.OfValue Undefined)
            symbols   = Map.empty<string, FunctionData>
        }

        let addressOf (variable: VariableType): int =
            let (character: char), (subscript: uint8) = variable
            let page: int = if character |> System.Char.IsLower then (int character - int 'a')
                            else                                     (int character - int 'A')
            let address: int = page * 11 + (int subscript)
            address

        let getVariable (table: VariableTable) (variable: VariableType): StorageType =
            let address: int = addressOf variable
            table[address]

        let setVariable (table: VariableTable) (variable: VariableType) (value: StorageType): VariableTable =
            let address: int = addressOf variable
            let newTable = Array.updateAt address value table
            newTable

        let getSymbol (table: SymbolTable) (symbol: string): FunctionData option =
            table.TryFind symbol

        let setSymbol (table: SymbolTable) (symbol: string) (fn: FunctionData): SymbolTable =
            table.Add (symbol, fn)

    type BinaryOperationRule     = ValueType -> ValueType -> ValueType Result
    type UnaryOperationRule      = ValueType              -> ValueType Result
    type ComparisonOperationRule = ValueType -> ValueType -> bool      Result

    let unsupportedBinaryOperation (op: BinaryOperator) (left: ValueType) (right: ValueType): 'a Result =
        MathError $"Unsupported binary {op} between {strValue left} & {strValue right}"

    let unsupportedUnaryOperation (op: UnaryOperator) (operand: ValueType): 'a Result =
        MathError $"Unsupported unary {op} for operand {strValue operand}"

    let unsupportedComparisonOperation (op: ComparisonOperator) (left: ValueType) (right: ValueType): 'a Result =
        MathError $"Unsupported {op} comparison between {strValue left} & {strValue right}"

    let num:   float -> ValueType Result = Number    >> Ok
    let pInf:           ValueType Result = PInfinity |> Ok
    let nInf:           ValueType Result = NInfinity |> Ok
    let undef:          ValueType Result = Undefined |> Ok

    let cmp:      bool -> bool  Result = Ok
    let cmpTrue:          bool  Result = true  |> Ok
    let cmpFalse:         bool  Result = false |> Ok
    let cmpNext:          float Result = nan   |> Ok // using NaN as a way to signal that the cmp succeeded, but cmpFalse

    let rec (<+>): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | PInfinity, NInfinity
         | NInfinity, PInfinity  -> PInfinity <-> PInfinity
         | PInfinity, _
         | _,         PInfinity  -> pInf
         | Number l,  Number r   -> num (l + r)
         | left,      right      -> (unsupportedBinaryOperation BinaryOperator.Addition) left right
    )
    and (<->): BinaryOperationRule = (fun left -> fun right ->
        ((<~->) right) ?=> (fun nRight ->
            match (left <+> nRight) with
             | Ok    v -> Ok v
             | Error _ -> (unsupportedBinaryOperation BinaryOperator.Subtraction) left right
        )
    )
    and (<*>): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | PInfinity, PInfinity
         | NInfinity, NInfinity -> pInf
         | NInfinity, _
         | _,         NInfinity -> nInf
         | Number l,  Number r  -> num (l * r)
         | left,      right     -> (unsupportedBinaryOperation BinaryOperator.Multiplication) left right
    )
    and (</>): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | _,         PInfinity
         | _,         NInfinity -> undef
         | PInfinity, _         -> pInf
         | NInfinity, _         -> nInf
         | Number l,  Number r  -> if r <> 0 then num (l / r) else MathError "Divison by zero"
         | left,      right     -> unsupportedBinaryOperation BinaryOperator.Division left right
    )
    and (<%>): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         // use native modulo operation if possible
         | Number l, Number r -> if r <> 0 then num (l % r) else MathError "Implicit division by zero"
         // a % b = a - b(a//b)
         | left,     right    ->
             (left  <//> right)      ?=> (fun floorDivAB -> //      (a//b)
             (right <*>  floorDivAB) ?=> (fun mulByB     -> //     b(a//b)
                 match (left <-> mulByB) with               // a - b(a//b)
                  | Ok    v -> Ok v
                  | Error _ -> (unsupportedBinaryOperation BinaryOperator.Modulo) left right
             ))
    )
    and (<//>): BinaryOperationRule = (fun left -> fun right ->
        (left </> right) ?=> (fun value ->
            match value with
             | Number n  -> num (System.Math.Floor n)
             | PInfinity -> pInf
             | NInfinity -> nInf
             | Undefined -> (unsupportedBinaryOperation BinaryOperator.Modulo) left right
        )
    )
    and (<^>): BinaryOperationRule = (fun left -> fun right ->
        match left, right with
         | Number l, PInfinity ->
             if   l < 0 then undef
             elif l < 1 then num 0
             elif l = 1 then num 1
             else            pInf
         | PInfinity, Number r  ->
             if   r < 0 then num 0
             elif r = 0 then undef // law of exponents - inf^x / inf^x = inf / inf = undefined
             else            pInf
         | Number l, NInfinity  -> (Number l <^> PInfinity) ?=> (fun result -> (Number 1 </> result))
         | NInfinity, Number r  ->
             if   r < 0 then (PInfinity <^> Number r) ?=> (fun result -> (Number 1 </> result))
             elif r <> System.Double.Floor r      then undef
             elif (System.Double.Abs r) % 2.0 = 0 then pInf
             else                                      nInf
         | Number l,  Number r  -> num (l ** r)
         | left,      right     -> (unsupportedBinaryOperation BinaryOperator.Exponent) left right
    )
    and (<~+>): UnaryOperationRule = (fun operand ->
        match operand with
         | PInfinity -> pInf
         | NInfinity -> nInf
         | Number n  -> num n
         | operand   -> (unsupportedUnaryOperation UnaryOperator.Positive) operand
    )
    and (<~->): UnaryOperationRule = (fun operand ->
        match operand with
         | PInfinity -> nInf
         | NInfinity -> pInf
         | Number n  -> num -n
         | operand   -> (unsupportedUnaryOperation UnaryOperator.Negative) operand
    )
    and (<~!>): UnaryOperationRule = (fun operand ->
        let rec computeNumerical (n: int): int =
            if n < 1 then 1
            else n * computeNumerical (n - 1)

        match operand with
         | PInfinity -> pInf
         | NInfinity -> undef
         | Number n  ->
             if (n < 0.0) || (n % 1.0 <> 0.0) then undef
             else n |> (int >> computeNumerical >> float >> num)
         | operand -> unsupportedUnaryOperation UnaryOperator.Factorial operand
    )
    and (<+->): UnaryOperationRule = (fun operand ->
        match operand with
         | PInfinity
         | NInfinity -> pInf
         | Number n  -> num (if n < 0 then -n else n)
         | operand   -> (unsupportedUnaryOperation UnaryOperator.Absolute) operand
    )
    and (?=?): ComparisonOperationRule = (fun left -> fun right ->
        match left, right with
         | PInfinity, PInfinity
         | NInfinity, NInfinity
         | Undefined, Undefined -> cmpTrue
         | Number l,  Number r  -> cmp (l = r)
         | _,         _         -> cmpFalse
    )
    and (?!=?): ComparisonOperationRule = (fun left -> fun right ->
        match (left ?=? right) with
         | Error _ -> (unsupportedComparisonOperation ComparisonOperator.NotEqualTo) left right
         | Ok    b -> if b then cmpFalse else cmpTrue
    )
    and (?>?): ComparisonOperationRule = (fun left -> fun right ->
        match left, right with
         | _,         PInfinity
         | NInfinity, _         -> cmpFalse
         | PInfinity, _
         | _,         NInfinity -> cmpFalse
         | Number l,  Number r  -> cmp (l > r)
         | left,      right     -> (unsupportedComparisonOperation ComparisonOperator.LessThan) left right
    )
    and (?>=?): ComparisonOperationRule = (fun left -> fun right ->
        match (left ?>? right) with
         | Error _ -> (unsupportedComparisonOperation ComparisonOperator.GreaterThanOrEqualTo) left right
         | Ok    b -> if b then cmpTrue else (left ?=? right)
    )
    and (?<?): ComparisonOperationRule = (fun left -> fun right ->
        match (left ?>=? right) with
         | Error _ -> (unsupportedComparisonOperation ComparisonOperator.LessThan) left right
         | Ok    b -> if b then cmpFalse else cmpTrue
    )
    and (?<=?): ComparisonOperationRule = (fun left -> fun right ->
        match (left ?>? right) with
         | Error _ -> (unsupportedComparisonOperation ComparisonOperator.LessThanOrEqualTo) left right
         | Ok    b -> if b then cmpTrue else (left ?=? right)
    )

    let rec evalTree (mem: Memory.Storage) (root: AST): (ValueType list * Memory.Storage) Result =
        Ok ([], mem)
