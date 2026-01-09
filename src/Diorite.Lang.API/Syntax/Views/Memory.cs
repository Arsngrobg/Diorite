// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Memory.cs
// Summary: The type definition for the Memory, the primary storage type for <b>Diorite</b>
// Author:  Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Diorite.Lang.API.Traits;
using Diorite.Lang.Core.Runtime;

namespace Diorite.Lang.API.Syntax.Views;

public class Memory : ICoreView<Core.Runtime.VirtualMemory.Memory> {
    private readonly  Core.Runtime.VirtualMemory.Memory _coreMemory;
    
    public Memory(Core.Runtime.VirtualMemory.Memory coreMemory) {
        _coreMemory = coreMemory;
    }
    
    public Core.Runtime.VirtualMemory.Memory AsCoreType() {
        return _coreMemory;
    }
    
    public VirtualMemory.CellData GetVariable(Variable variable) {
        var coreVariable = variable.AsCoreType();
        return VirtualMemory.GetVariable(_coreMemory, coreVariable.Item1, coreVariable.Item2);
    }

    public Memory SetVariable(Variable variable, VirtualMemory.CellData cellData) {
        var coreVariable = variable.AsCoreType();
        var newCoreMemory = VirtualMemory.SetVariable(
            _coreMemory, coreVariable.Item1, coreVariable.Item2, cellData
        );
        return new Memory(newCoreMemory);
    }

    public Memory SetVariables(IEnumerable<(Variable variable, VirtualMemory.CellData cellData)> pairs) {
        var newCoreMemory = _coreMemory;
        foreach (var (variable, cellData) in pairs) {
            var coreVariable = variable.AsCoreType();
            newCoreMemory = VirtualMemory.SetVariable(newCoreMemory, coreVariable.Item1, coreVariable.Item2, cellData);
        }
        return new Memory(newCoreMemory);
    }

    public Memory UpdateSymbol(string alias, Function function) {
        var functionCore = function.AsCoreType();
        var newSymbols = _coreMemory.symbols.Add(alias, functionCore);
        var newCoreMemory = new Core.Runtime.VirtualMemory.Memory(
            _coreMemory.variables,
            newSymbols,
            _coreMemory.plotCallback
        );
        return new Memory(newCoreMemory);
    }

    public Function GetFunctionFromSymbol(string alias) {
        return new Function(_coreMemory.symbols.TryFind(alias).Value.Item1, 
            _coreMemory.symbols.TryFind(alias).Value.Item2);
    }
}