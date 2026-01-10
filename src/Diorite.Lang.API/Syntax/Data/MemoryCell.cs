// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    MemoryCellData.cs
// Summary: The type definition for the MemoryCell type, which is the storage type for each cell in the virtual
//          memory for Diorite
// Author:  Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using Diorite.Lang.API.Traits;

namespace Diorite.Lang.API.Syntax.Views;

/// <summary>
///     <p>The <c>CellData</c> type defines the types of data that can be stored in a variable table cell in
///        <b>Diorite</b>. Either, it can be of a value (<c>Undefined</c>, <c>Number</c>, <c>Complex</c>). Or
///        it can be of a function declaration (e.g. <c>f(x) = 2*x</c>)
///     </p>
/// </summary>
public abstract class MemoryCell : ICoreView<Core.Runtime.VirtualMemory.CellData>,
                                   ICoreConverter<Core.Runtime.VirtualMemory.CellData, MemoryCell>
{
    /// <summary>
    ///     <p>Creates a new <c>MemoryCell</c> object that stores a <see cref="Value"/> in the slot.</p>
    /// </summary>
    /// <param name="value"> the <see cref="Value"/> to store in this <c>MemoryCell</c> </param>
    /// <returns> a new <c>MemoryCell</c> object that stores the supplied <see cref="Value"/> </returns>
    public static MemoryCell AllocateValueSlot(Value value) =>
        new OfValue(value);

    /// <summary>
    ///     <p>Creates a new <c>MemoryCell</c> object that stores a <see cref="Function"/> in the slot.</p>
    /// </summary>
    /// <param name="function"> the <see cref="Function"/> to store in this <c>MemoryCell</c> </param>
    /// <returns> a new <c>MemoryCell</c> object that stores the supplied <see cref="Function"/> </returns>
    public static MemoryCell AllocateFunctionSlot(Function function) =>
        new OfFunction(function);

    public static MemoryCell OfCoreType(Core.Runtime.VirtualMemory.CellData cellData) => cellData switch
    {
        Core.Runtime.VirtualMemory.CellData.OfValue    value => new OfValue(Value.OfCoreType(value.Item)),
        Core.Runtime.VirtualMemory.CellData.OfFunction fn    => new OfFunction(Function.OfCoreType(fn.Item)),
        _ => throw new InvalidOperationException("Not all cases were complete for MemoryCell")
    };

    private MemoryCell() {}

    /// <summary>
    ///     <p>Performs pattern matching on this <c>FunctionReference</c>.</p>
    /// </summary>
    /// <param name="onValueSlot"> the callback to execute if this <c>MemoryCell</c> stores a value </param>
    /// <param name="onFunctionSlot"> the callback to execute if this <c>MemoryCell</c> stores a function </param>
    /// <typeparam name="T"> the result of this pattern matching operation </typeparam>
    /// <returns> the result of the pattern match, bound by the type <c>T</c> </returns>
    public T Match<T>(Func<Value, T> onValueSlot, Func<Function, T> onFunctionSlot) => this switch
    {
        OfValue    slot => onValueSlot(slot.Value),
        OfFunction slot => onFunctionSlot(slot.Function),
        _               => throw new InvalidOperationException($"Invalid case {GetType()}")
    };

    public abstract Core.Runtime.VirtualMemory.CellData AsCoreType();

    public abstract override int    GetHashCode();
    public abstract override bool   Equals(object? obj);
    public abstract override string ToString();

    /// <summary>
    ///     <p>The <c>ValueType</c> branch of a <c>CellData</c> in the variable table.</p>
    ///     <p><i>e.g. <c>x = 2</c>/<c>x = undefined</c>/<c>x = complex(0, 1)</c></i></p>
    /// </summary>
    private sealed class OfValue : MemoryCell
    {
        internal Value Value { get; }

        internal OfValue(Value value) =>
            Value = value;

        public override Core.Runtime.VirtualMemory.CellData AsCoreType() =>
            Core.Runtime.VirtualMemory.CellData.NewOfValue(Value.AsCoreType());

        public override int GetHashCode() =>
            Value.GetHashCode();

        public override bool Equals(object? obj) =>
            obj is OfValue other &&
            Value.Equals(other.Value);

        public override string ToString() =>
            Value.ToString();
    }

    /// <summary>
    ///     <p>The <c>FunctionType</c> branch of a <c>CellData</c> in the variable table.</p>
    ///     <p><i>e.g. <c>f(x) = 2*x</c></i></p>
    /// </summary>
    private sealed class OfFunction : MemoryCell
    {
        internal Function Function { get; }

        internal OfFunction(Function function) =>
            Function = function;

        public override Core.Runtime.VirtualMemory.CellData AsCoreType() =>
            Core.Runtime.VirtualMemory.CellData.NewOfFunction(Function.AsCoreType());

        public override int GetHashCode() =>
            Function.GetHashCode();

        public override bool Equals(object? obj) =>
            obj is OfFunction other &&
            Function.Equals(other.Function);

        public override string ToString() =>
            Function.ToString();
    }
}