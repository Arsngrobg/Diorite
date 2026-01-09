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

using Diorite.Lang.API.Traits;
using Diorite.Lang.Core.Syntax;

namespace Diorite.Lang.API.Syntax.Views;

public class Function : ICoreView<Tuple<FunctionAttributes, FunctionBody>> {
    private FunctionAttributes FunctionAttributes {get;}
    private FunctionBody FunctionBody {get;}
    
    public Function(FunctionAttributes functionAttributes, FunctionBody functionBody) {
        FunctionAttributes = functionAttributes;
        FunctionBody = functionBody;
    }
    
    public Tuple<FunctionAttributes, FunctionBody> AsCoreType() {
        return new (FunctionAttributes, FunctionBody);
    }
    
    /// <summary>
    ///     <p>Creates a new <c>Function</c> from its equivalent core type.</p>
    /// </summary>
    /// <param name="functionCore"> the equivalent core type </param>
    /// <returns> a new <c>Function</c>, derived from its equivalent core type </returns>
    internal static Function OfCoreType(Tuple<FunctionAttributes, FunctionBody> functionCore) =>
        new (functionCore.Item1, functionCore.Item2);
}