// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Function.cs
// Summary: The type definition for the Function, the structured representation of a function
// Author:  Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Diorite.Lang.API.Syntax.Structure;
using Diorite.Lang.API.Traits;
using Diorite.Lang.API.Syntax.Data;

namespace Diorite.Lang.API.Syntax.Views;

/// <summary>
///     <p>The <c>Function</c> type describes a <b>Diorite</b> function.</p>
///     <p>A function is composed of two parts:
///        <p><b>1.</b> its <see cref="FunctionAttributes"/>, which contain relevant data such as: the identifier that
///           the function was initially declared with, its parameters, return set type, etc...
///        </p>
///        <p><b>2.</b> its <see cref="Ast"/>, which represents the body of the function.</p>
///     </p>
///     <p><i>This is a <b>Read-Only</b> type, meaning they cannot and should not be instantiated outside of API
///           functions.
///     </i></p>
/// </summary>
public sealed class Function : ICoreView<Tuple<Core.Syntax.FunctionAttributes, Core.Syntax.FunctionBody>>,
                               ICoreConverter<Tuple<Core.Syntax.FunctionAttributes, Core.Syntax.FunctionBody>, Function>
{
    /// <summary>
    ///     <p>Creates a new <c>Function</c> from its equivalent core type.</p>
    /// </summary>
    /// <param name="coreFunction"> the equivalent core type </param>
    /// <returns> a new <c>Function</c>, derived from its equivalent core type </returns>
    public static Function OfCoreType(Tuple<Core.Syntax.FunctionAttributes, Core.Syntax.FunctionBody> coreFunction) =>
        new (
            FunctionAttributes.OfCoreType(coreFunction.Item1),
            Ast.OfCoreType(coreFunction.Item2),
            coreFunction.Item2
        );

    public FunctionAttributes FunctionAttributes { get; }
    public Ast                FunctionBody       { get; }
    
    // executable-only
    internal readonly Core.Syntax.FunctionBody Executable;
    
    private Function(FunctionAttributes functionAttributes, Ast functionBody, Core.Syntax.FunctionBody executable) =>
        (FunctionAttributes, FunctionBody, Executable) = (functionAttributes, functionBody, executable);

    public Tuple<Core.Syntax.FunctionAttributes, Core.Syntax.FunctionBody> AsCoreType() =>
        new (FunctionAttributes.AsCoreType(), Executable);

    public override int GetHashCode() =>
        HashCode.Combine(FunctionAttributes, FunctionBody);

    public override bool Equals(object? obj) =>
        obj is Function other &&
        FunctionAttributes.Equals(other.FunctionAttributes) && FunctionBody.Equals(other.FunctionBody);

    public override string ToString() =>
        $"Function[Attributes: {FunctionAttributes}, Body: {FunctionBody}]";
}