// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Memory.cs
// Summary: The type definition for the Memory, the primary storage type for Diorite
// Author:  Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using System.Collections;
using Diorite.Lang.API.Callbacks;
using Diorite.Lang.API.Traits;
using Microsoft.FSharp.Collections;

namespace Diorite.Lang.API.Syntax.Views;

/// <summary>
///     <p>The <c>Memory</c> type is the primary storage type for <b>Diorite</b>.</p>
///     <p>It holds <b>two</b> sections of data:
///        <p><b>1.</b> <c>VariableTable</c> - where in each cell, they are either storing a <c>ValueType</c> or
///           a <c>FunctionType</c>. Initially, all cells are declared as <c>Undefined</c>.
///        </p>
///        <p><b>2.</b> <c>SymbolRegister</c> - which maintains copies of function declarations with symbolic
///           names using the <c>[symbol:[SYMBOL]]</c> syntax. This means a redefined function can still be
///           called, provided they have been declared to have a symbolic alias.
///        </p>
///        <p><b>3.</b> <c>PlotCallback</c> - the callback function to call when the <c>plot</c> syntax is used.</p>
///     </p>
/// </summary>
public class Memory : ICoreView<Core.Runtime.VirtualMemory.Memory>,
                      ICoreConverter<Core.Runtime.VirtualMemory.Memory, Memory>
{
    /// <summary>
    ///     <p>Returns the default state of <c>Memory</c>.</p>
    ///     <p>All variables in the <c>VariableTable</c> are <c>Undefined</c>.</p>
    ///     <p>The <c>SymbolRegister</c> is empty.</p>
    /// </summary>
    /// <param name="callback"> the callback function for the <c>plot</c> syntax </param>
    /// <returns> a default <c>Memory</c> state </returns>
    public static Memory FromDefaults(PlotCallback callback) =>
        new (
            Enumerable.Repeat(MemoryCell.AllocateValueSlot(Value.Undefined), Variable.MaxVariables).ToArray(),
            new Dictionary<string, Function>(),
            callback
        );

    public static Memory OfCoreType(Core.Runtime.VirtualMemory.Memory coreMemory)
    {
        // idk if this even works
        // no errors so hopefully
        PlotCallback derivedCallback = csFn =>
        {
            var fsharpFn = Microsoft.FSharp.Core.FSharpFunc<Core.Syntax.ValueType, 
                    Microsoft.FSharp.Core.FSharpResult<Core.Syntax.ValueType, Core.Errors.DioriteError>>
                .FromConverter(x =>
                {
                    var val = csFn(Value.OfCoreType(x));
                    return Microsoft.FSharp.Core.FSharpResult<
                        Core.Syntax.ValueType,
                        Core.Errors.DioriteError
                    >.NewOk(val.AsCoreType());
                });

            // Call the original F# callback, ignore Unit
            coreMemory.plotCallback.Invoke(fsharpFn);
        };
        return new Memory(
            coreMemory.variables.Select(MemoryCell.OfCoreType).ToArray(),
            coreMemory.symbols.Select(kv => new { kv.Key, Value = Function.OfCoreType(kv.Value) })
                              .ToDictionary(x => x.Key, x => x.Value),
            derivedCallback
        );
    }

    private static int IndexOf(Variable variable)
    {
        var coreVariable = variable.AsCoreType(); 
        return Core.Runtime.VirtualMemory.IndexOf(coreVariable.Item1, coreVariable.Item2);
    }

    private readonly MemoryCell[]                 _variableTable;
    private readonly Dictionary<string, Function> _symbolRegister;
    private readonly PlotCallback                 _plotCallback;

    private Memory(MemoryCell[] variableTable, Dictionary<string, Function> symbolRegister, PlotCallback plotCallback) =>
        (_variableTable, _symbolRegister, _plotCallback) = (variableTable, symbolRegister, plotCallback);

    public Core.Runtime.VirtualMemory.Memory AsCoreType()
    {
        // Func<Value, Value> -> FSharpFunc<ValueType, ValueType Result>
        var asFsharpFunc = Microsoft.FSharp.Core.FSharpFunc<
            Microsoft.FSharp.Core.FSharpFunc<
                Core.Syntax.ValueType, Microsoft.FSharp.Core.FSharpResult<
                    Core.Syntax.ValueType, Core.Errors.DioriteError
                >
            >,
            Microsoft.FSharp.Core.Unit
        >.FromConverter(anonFn =>
        {
            _plotCallback(x =>
            {
                var fsResult = anonFn.Invoke(x.AsCoreType());
                return fsResult.IsOk ? Value.OfCoreType(fsResult.ResultValue) : throw new Exception("yeah nah");
            });

            return null!;
        });
        
        return new Core.Runtime.VirtualMemory.Memory(
            _variableTable.Select(c => c.AsCoreType()).ToArray(),
            MapModule.OfSeq(_symbolRegister.Select(
                kv => new Tuple<string, Tuple<Core.Syntax.FunctionAttributes, Core.Syntax.FunctionBody>>(
                    kv.Key, kv.Value.AsCoreType()
                )
            )),
            asFsharpFunc
        );
    }
    
    /// <summary>
    ///     <p>Gets the variable in the <c>VariableTable</c> of this <c>Memory</c> object.</p>
    /// </summary>
    /// <param name="variable"> the <c>Variable</c> that points to an index in the <c>VariableTable</c> </param>
    /// <returns> the <c>CellData</c> for that <c>Variable</c> </returns>
    public MemoryCell GetVariable(Variable variable) =>
        _variableTable[IndexOf(variable)];

    /// <summary>
    ///     <p>Sets the variable in the <c>VariableTable</c> of this <c>Memory</c> object.</p>
    /// </summary>
    /// <param name="variable"> the <c>Variable</c> that points to an index in the <c>VariableTable</c> </param>
    /// <param name="cellData"> the <c>CellData</c> that either contains a function or value </param>
    public void SetVariable(Variable variable, MemoryCell cellData) =>
        _variableTable[IndexOf(variable)] = cellData;

    /// <summary>
    ///     <p>Applies the sequence of variable-value pairs to this <c>Memory</c> object.</p>
    /// </summary>
    /// <param name="pairs"> a variadic sequence of <c>Variable</c>-<c>CellData</c> pairs </param>
    public void SetVariables(params (Variable, MemoryCell)[] pairs)
    {
        foreach (var (variable, cellData) in pairs)
            _variableTable[IndexOf(variable)] = cellData;
    }

    /// <summary>
    ///     <p>Updates the <c>SymbolRegister</c> for this <c>Memory</c> object.</p>
    ///     <p>This copies the <c>FunctionType</c> into this virtual <c>Memory</c> object so even after the variables
    ///        is redefined, it can still be referenced.
    ///     </p>
    /// </summary>
    /// <param name="alias"> the <c>string</c> alias for the function </param>
    /// <param name="function"> the <c>Function</c> to copy into the register </param>
    public void UpdateSymbol(string alias, Function function) =>
        _symbolRegister.Add(alias, function);

    /// <summary>
    ///     <p>Tries to obtain the <c>Function</c> from the supplied <c>string</c> alias for the <b>Diorite</b>
    ///        function. It returns a <see cref="Nullable"/>&lt;<see cref="Function"/>&gt; type as the <c>alias</c> may
    ///        not point to a valid <b>Diorite</b> function at the time of this function call.
    ///     </p>
    /// </summary>
    /// <param name="alias"> the <c>string</c> alias for the <b>Diorite</b> function that may exist </param>
    public Function? GetFunctionFromSymbol(string alias) =>
        _symbolRegister.GetValueOrDefault(alias);

    /// <summary>
    ///     <p>Tries to obtain the function from the supplied <see cref="FunctionReference"/> value.</p>
    ///     <p>If the <see cref="FunctionReference"/> does not point to a <see cref="Function"/> in virtual memory, then
    ///        it will return <c>null</c>.
    ///     </p>
    /// </summary>
    /// <param name="fnRef"> the <c>FunctionReferenceType</c> that may point to a <c>FunctionType</c> </param>
    /// <returns> the <see cref="Nullable"/> <b>Diorite</b> <see cref="Function"/> type </returns>
    public Function? GetFunctionFromRef(FunctionReference fnRef)
    {
        var fn = fnRef.Match(
            onSymbol:          GetFunctionFromSymbol,
            onVariable: var =>
                _variableTable[IndexOf(var)].Match(
                    onValueSlot:    _   => null!,
                    onFunctionSlot: fun => fun
                )
        );
        return fn;
    }
    
    public override int GetHashCode() =>
        HashCode.Combine(_variableTable, _symbolRegister);

    public override bool Equals(object? obj) =>
        obj is Memory other &&
        _variableTable.SequenceEqual(other._variableTable) &&
        _symbolRegister.Count == other._symbolRegister.Count &&
        StructuralComparisons.StructuralEqualityComparer.Equals(
            _symbolRegister.ToArray(),
            other._symbolRegister.ToArray()
        );

    public override string ToString() {
        // TODO: maybe print out a table or a list of defined variables/symbols
        string memory = "";
        return memory;
    }
}