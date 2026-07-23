// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Evaluators.fs
// Summary: The mappings for the operation rules in Diorite, as it coincides with the syntax definitions
// Author:  Arsngrobg, Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax
open Diorite.Lang.Core.Runtime.BinaryOperationRules
open Diorite.Lang.Core.Runtime.UnaryOperationRules
open Diorite.Lang.Core.Runtime.ComparisonOperationRules

/// <summary>
///     <p>Contains the mappings for each operation rule as per the operation types defined in the <c>Syntax</c>
///        namespace.
///     </p>
/// </summary>
module RuleMappings =
    /// <summary>
    ///     <p>Gets the respective <c>BinaryOperationRule</c> for the supplied <c>BinaryOperator</c>.</p>
    /// </summary>
    let GetBinaryRule: BinaryOperator -> BinaryOperationRule = function
     | BinaryOperator.Addition       -> BinaryAdditionRule
     | BinaryOperator.Subtraction    -> BinarySubtractionRule
     | BinaryOperator.Multiplication -> BinaryMultiplicationRule
     | BinaryOperator.Division       -> BinaryDivisionRule
     | BinaryOperator.Modulo         -> BinaryModuloRule
     | BinaryOperator.FloorDivision  -> BinaryFloorDivisionRule
     | BinaryOperator.Exponent       -> BinaryExponentRule
     | BinaryOperator.OfComplex      -> BinaryOfComplexRule

    /// <summary>
    ///     <p>Gets the respective <c>UnaryOperationRule</c> for the supplied <c>UnaryOperator</c>.</p>
    /// </summary>
    let GetUnaryRule: UnaryOperator -> UnaryOperationRule = function
        | UnaryOperator.Positive     -> UnaryPositiveRule
        | UnaryOperator.Negative     -> UnaryNegativeRule
        | UnaryOperator.Factorial    -> UnaryFactorialRule
        | UnaryOperator.Absolute     -> UnaryAbsoluteRule
        | UnaryOperator.GetImaginary -> UnaryGetImaginaryRule
        | UnaryOperator.GetReal      -> UnaryGetRealRule

    /// <summary>
    ///     <p>Gets the respective <c>ComparisonOperationRule</c> for the supplied <c>ComparisonOperator</c>.</p>
    /// </summary>
    let GetComparisonRule: ComparisonOperator -> ComparisonOperationRule = function
        | ComparisonOperator.Equality             -> EqualityComparisonRule
        | ComparisonOperator.Inequality           -> InequalityComparisonRule
        | ComparisonOperator.StrictLessThan       -> StrictLessThanComparisonRule
        | ComparisonOperator.StrictGreaterThan    -> StrictGreaterThanComparisonRule
        | ComparisonOperator.NonStrictLessThan    -> NonStrictLessThanComparisonRule
        | ComparisonOperator.NonStrictGreaterThan -> NonStrictGreaterThanComparisonRule
