// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    FunctionReferenceType.cs
// Summary: The type definition of the FunctionReferenceType type, which describes the reference types for a Diorite
//          function  
// Author:  Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Diorite.Lang.API.Traits;

namespace Diorite.Lang.API.Syntax.Views;

public class FunctionReferenceType : ICoreView<Core.Syntax.FunctionReferenceType>
{
    private readonly Core.Syntax.FunctionReferenceType _functionReferenceType;
    
    private FunctionReferenceType(Core.Syntax.FunctionReferenceType functionReferenceType)
    {
        _functionReferenceType = functionReferenceType;
    }
    
    public Core.Syntax.FunctionReferenceType AsCoreType()
    {
        return _functionReferenceType;
    }
}