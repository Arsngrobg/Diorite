// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    VirtualMemory.fs
// Summary: The VirtualMemory API for Diorite
// Author:  Arsngrobg, Borngle
// Version: v1.4
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------


namespace Diorite.Lang.Core.Runtime

open Diorite.Lang.Core.Syntax

/// <summary>
///     <p>The <c>VirtualMemory</c> module contains related bindings for managing the <b>Diorite</b> virtual memory.</p>
///     <p><b>Diorite</b> manages <b>two</b> forms memory:
///        <p><b>1.</b> variable table - where in each cell, they are either storing a <c>ValueType</c> or a
///           <c>FunctionType</c>. Initially, all cells are declared as <c>Undefined</c>.
///        </p>
///        <p><b>2.</b> symbol register - which maintains copies of function declarations with symbolic names using
///           the <c>[symbol:[SYMBOL]]</c> syntax. This means a redefined function can still be called, provided
///           they have been declared to have a symbolic alias.
///        </p>
///     </p>
/// </summary>
[<AutoOpen>]
module VirtualMemory =
    /// <summary>
    ///     <p>The <c>CellData</c> type defines the types of data that can be stored in a variable table cell in
    ///        <b>Diorite</b>. Either, it can be of a value (<c>Undefined</c>, <c>Number</c>, <c>Complex</c>). Or
    ///        it can be of a function declaration (e.g. <c>f(x) = 2*x</c>)
    ///     </p>
    /// </summary>
    type CellData =
        /// <summary>
        ///     <p>The <c>ValueType</c> branch of a <c>CellData</c> in the variable table.</p>
        ///     <p><i>e.g. <c>x = 2</c>/<c>x = undefined</c>/<c>x = complex(0, 1)</c></i></p>
        /// </summary>
        | OfValue    of ValueType
        /// <summary>
        ///     <p>The <c>FunctionType</c> branch of a <c>CellData</c> in the variable table.</p>
        ///     <p><i>e.g. <c>f(x) = 2*x</c></i></p>
        /// </summary>
        | OfFunction of FunctionType

    /// <summary>
    ///     <p>A <c>VariableTable</c> is a region of contiguous memory that maps to a table of data where each row
    ///        is mapped to a prefix of an alphabetical character denoting a variable, and the columns are divided
    ///        into two groups: where an internal subscript of <c>0</c> is the variable without the additional
    ///        subscript character, every subscript from <c>1</c> to <c>10</c> is the encoded subscript.
    ///     </p>
    ///     <p><i>Each character group is offset by <c>11</c> cells, as <c>11</c> different variations of the same
    ///        character.</i>
    ///     </p>
    /// </summary>
    type VariableTable = CellData array

    /// <summary>
    ///     <p>A <c>SymbolRegister</c> is a <c>Map</c> that holds the aliases for a defined <c>FunctionType</c>s in
    ///        <b>Diorite</b>. Each <c>FunctionType</c> in the register is uniquely defined, but that does not
    ///        restrict functions of equal tree signature. That means no function can have at least <b>one</b>
    ///        aliases per definition.
    ///     </p>
    /// </summary>
    type SymbolRegister = Map<string, FunctionType>
    
    /// <summary>
    ///     <p>The callback function that is called whenever the <c>plot &lt;Expression&gt;</c> syntax is evaluated.
    ///        It accepts a function that takes in a parameter, and produces a value, only 1-dimensional functions
    ///        are supported for plotting functions as the interpreter evaluates it as a <c>FunctionType</c> with a
    ///        singular argument.
    ///     </p>
    /// </summary>
    type PlotCallback = (ValueType -> ValueType) -> unit

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
    ///     </p>
    ///     <p>Each 'instance' of <c>Memory</c> should remain immutable.</p>
    /// </summary>
    type Memory = {
        /// <summary>
        ///     <p>The table that maps to each possible <c>VariableType</c> in <b>Diorite</b>.</p>
        /// </summary>
        variables: VariableTable
        /// <summary>
        ///     <p>The register (<c>Map</c>) that contains the aliases and their mapped <c>FunctionType</c>.</p>
        /// </summary>
        symbols:   SymbolRegister
        /// <summary>
        ///     <p>The callback function that is called whenever the <c>plot &lt;Expression&gt;</c> syntax is evaluated.
        ///        It accepts a function that takes in a parameter, and produces a value, only 1-dimensional functions
        ///        are supported for plotting functions as the interpreter evaluates it as a <c>FunctionType</c> with a
        ///        singular argument.
        ///     </p>
        /// </summary>
        plotCallback: PlotCallback
    }

    /// <summary>
    ///     <p>The default state of <c>Memory</c>.</p>
    ///     <p>All variables in the <c>VariableTable</c> are <c>Undefined</c>.</p>
    ///     <p>The <c>SymbolRegister</c> is empty.</p>
    /// </summary>
    /// <param name="plotCallback"> the callback function for the <c>plot</c> syntax </param>
    /// <returns> a default <c>Memory</c> state </returns>
    let Defaults (plotCallback: PlotCallback): Memory = {
        variables    = (ValueType.Undefined |> CellData.OfValue) |> (Array.create MaxVariables)
        symbols      = Map.empty<string, FunctionType>
        plotCallback = plotCallback
    }

    /// <summary>
    ///     <p>Calculates the index of the supplied <c>VariableType</c> for its respective position in the
    ///        <c>VariableTable</c>.
    ///     </p>
    /// </summary>
    /// <param name="var"> the <c>VariableType</c> to derive the position from </param>
    /// <returns> the position of the <c>VariableType</c> in the <c>VariableTable</c> </returns>
    let IndexOf (var: VariableType): int =
        let (c: char), (s: uint8) = var
        assert (c |> System.Char.IsLetter)
        let charIdx: int = if c |> System.Char.IsUpper then ((int 'Z') - (int c)) + 26 else (int 'z') - (int c)
        let regionStart: int = charIdx * 11
        regionStart + (int s)

    /// <summary>
    ///     <p>Sets the variable in the <c>VariableTable</c> of the supplied <c>Memory</c>.</p>
    ///     <p>It returns a new copy of the <c>Memory</c> that includes the updated <c>VariableTable</c>.</p>
    /// </summary>
    /// <param name="memory"> the <c>Memory</c> struct </param>
    /// <param name="slot"> the <c>VariableType</c> that points to an index in the <c>VariableTable</c> </param>
    /// <param name="value"> <c>CellData</c> that either contains a function or value </param>
    /// <returns> a new <c>Memory</c> struct that carries the new variable table </returns>
    let SetVariable (memory: Memory) (slot: VariableType, value: CellData): Memory =
        let position: int = IndexOf slot
        let tableCopy: VariableTable = memory.variables |> (Array.updateAt position value)
        {
            variables    = tableCopy
            symbols      = memory.symbols
            plotCallback = memory.plotCallback
        }

    /// <summary>
    ///     <p>Applies the sequence of variable-value pairs and returns the copy of <c>Memory</c> which reflects
    ///        this change.
    ///     </p>
    /// </summary>
    /// <param name="memory"> the <c>Memory</c> struct </param>
    /// <param name="pairs"> a list of <c>VariableType</c>-<c>CellData</c> pairs </param>
    /// <returns> a new <c>Memory</c> struct that carries the new variable table </returns>
    let SetVariables (memory: Memory) (pairs: (VariableType * CellData) list): Memory =
        let rec MutateTable (table: VariableTable) (pairs: (VariableType * CellData) list): unit =
            match pairs with
             | []              -> ()
             | (v, cd) :: tail ->
                 let position: int = IndexOf v 
                 table[position] <- cd
                 MutateTable table tail

        let tableCopy: VariableTable = Array.copy memory.variables
        MutateTable tableCopy pairs
        {
            variables    = tableCopy
            symbols      = memory.symbols
            plotCallback = memory.plotCallback
        }

    /// <summary>
    ///     <p>Gets the variable in the <c>VariableTable</c> of the supplied <c>Memory</c>.</p>
    /// </summary>
    /// <param name="memory"> the <c>Memory</c> struct </param>
    /// <param name="slot"> the <c>VariableType</c> that points to an index in the <c>VariableTable</c> </param>
    /// <returns> the <c>CellData</c> for that <c>VariableType</c> </returns>
    let GetVariable (memory: Memory) (slot: VariableType): CellData =
        let position: int = IndexOf slot
        memory.variables[position]

    /// <summary>
    ///     <p>Updates the <c>SymbolRegister</c> for the supplied <c>Memory</c>.</p>
    ///     <p>This copies the <c>FunctionType</c> into <b>Diorite</b>'s virtual memory so even after the variables
    ///        is redefined, it can still be referenced.
    ///     </p>
    /// </summary>
    /// <param name="memory"> the <c>Memory</c> struct </param>
    /// <param name="alias"> the <c>string</c> alias for the function </param>
    /// <param name="fn"> the <c>FunctionType</c> to copy into the register </param>
    /// <returns> the updated <c>Memory</c> struct wih the updated <c>SymbolRegister</c> </returns>
    let UpdateSymbol (memory: Memory) (alias: string, fn: FunctionType): Memory =
        let updatedSymbols: SymbolRegister = memory.symbols.Add (alias, fn)
        {
            variables    = memory.variables // no need to copy as any modifications will be applied to copies later on
            symbols      = updatedSymbols
            plotCallback = memory.plotCallback
        }

    /// <summary>
    ///     <p>Tries to obtain the <c>FunctionType</c> from the supplied <c>string</c> alias for the <b>Diorite</b>
    ///        function. It returns a wrapper <c>option</c> type as the <c>alias</c> may not point to a valid
    ///        <b>Diorite</b> function at the time of this function call.
    ///     </p>
    /// </summary>
    /// <param name="memory"> the <c>Memory</c> struct </param>
    /// <param name="alias"> the <c>string</c> alias for the <b>Diorite</b> function that may exist </param>
    /// <returns> the <b>Diorite</b> function wrapped in an <c>option</c> type </returns>
    let GetFunctionFromSymbol (memory: Memory) (alias: string): FunctionType option =
        memory.symbols.TryFind alias

    /// <summary>
    ///     <p>Tries to obtain the function from the supplied <c>FunctionReferenceType</c> value.</p>
    ///     <p>If the <c>FunctionReferenceType</c> does not point to a <c>FunctionType</c> in virtual memory, then it
    ///        will return <c>None</c>.
    ///     </p>
    /// </summary>
    /// <param name="ref"> the <c>FunctionReferenceType</c> that may point to a <c>FunctionType</c> </param>
    /// <param name="memory"> the <c>Memory</c> to check for membership </param>
    /// <returns> the <b>Diorite</b> function wrapped in an <c>option</c> type </returns>
    let GetFunctionFromRef (memory: Memory) (ref: FunctionReferenceType): FunctionType option =
        match ref with
         | FunctionReferenceType.OfSymbol   sym -> GetFunctionFromSymbol memory sym
         | FunctionReferenceType.OfVariable var ->
             match (GetVariable memory var) with
              | CellData.OfFunction fn -> Some fn
              | CellData.OfValue    _  -> None
