using System.Runtime.CompilerServices;
using System.Text;
using Diorite.Lang.API.Syntax.Views;

namespace Diorite.Lang.API.Library;

/// <summary>
///     <p>The <c>Library</c> class is a builder for a snapshot of <see cref="Memory"/> before the execution of
///        user-defined <b>Diorite</b> source code.
///     </p>
///     <p>It allows for the chaining of operations through the <see cref="AddFile"/> and <see cref="AddSource"/>
///        methods. Typical usage is to initialise memory before executing some arbitrary <b>Diorite</b> code, as
///        building the initial memory snapshot is expensive.
///     </p>
///     <p>To get the initial <see cref="Memory"/> snapshot of the code, use the <see cref="BuildMemorySnapshot"/>
///        method to <i>build</i> the library.
///     </p>
/// </summary>
public sealed class Library
{
    /// <summary>
    ///     <p>Creates a new <c>Library</c> builder object from the supplied <b>Diorite</b> <c>source</c> string.</p>
    /// </summary>
    /// <param name="source"> the <b>Diorite</b> source string to load into the <c>Library</c> builder </param>
    /// <returns> a new <c>Library</c> builder object that has the supplied <c>source</c> string </returns>
    public static Library OfSource(string source) =>
        OfNothing().AddSource(source);

    /// <summary>
    ///     <p>Creates a new <c>Library</c> builder object with the contents of the source file, described by the
    ///        <c>filePath</c> argument.
    ///     </p>
    ///     <i>This static factory will throw an <c>ArgumentException</c> if the supplied <c>filePath</c> does not end
    ///        with <c>.diorite</c>.
    ///     </i>
    /// </summary>
    /// <param name="filePath"> the path to the <b>Diorite</b> source file </param>
    /// <returns> a new <c>Library</c> builder object with the contents of the supplied source file </returns>
    /// <exception cref="ArgumentException"> if the <c>filePath</c> points to a file not ending with <c>.diorite</c>
    /// </exception>
    public static Library OfFile(string filePath) =>
        OfNothing().AddFile(filePath);

    /// <summary>
    ///     <p>Creates a new <c>Library</c> builder object with no source in it.</p>
    /// </summary>
    /// <returns> a new <c>Library</c> builder object with no content </returns>
    public static Library OfNothing() =>
        new (string.Empty);

    private string _source;

    private Library(string source) =>
        _source = source;

    /// <summary>
    ///     <p>Constructs the <see cref="Memory"/> snapshot of this library post evaluation.</p>
    ///     <p>Raw expressions are ignored.</p>
    /// </summary>
    /// <returns> the <see cref="Memory"/> snapshot of this <c>Library</c> post evaluation </returns>
    public Memory BuildMemorySnapshot()
    {
        var noOp = Microsoft.FSharp.Core.FSharpFunc<
            Microsoft.FSharp.Core.FSharpFunc<
                Core.Syntax.ValueType,
                Microsoft.FSharp.Core.FSharpResult<
                    Core.Syntax.ValueType,
                    Core.Errors.DioriteError
                >
            >,
            Microsoft.FSharp.Core.Unit
        >.FromConverter(_ => null!);

        var blankMemory  = Core.Runtime.VirtualMemory.Defaults(noOp);
        var coreSnapshot = Core.Runtime.Evaluation.EvalString(_source, blankMemory).Item2;
        return Memory.OfCoreType(coreSnapshot);
    }

    /// <summary>
    ///     <p>Adds the supplied <b>Diorite</b> <c>source</c> code to this <c>Library</c> builder object.</p>
    ///     <p><i>Returns the same instance of this <c>Library</c> object to chain operations.</i></p>
    /// </summary>
    /// <param name="source"> the <b>Diorite</b> source code to include in this <c>Library</c> </param>
    /// <returns> the same <c>Library</c> object, for chaining operations </returns>
    public Library AddSource(string source)
    {
        _source = $"{_source}\n{source}";
        return this;
    }

    /// <summary>
    ///     <p>Adds the supplied <b>Diorite</b> <c>file</c> source file to this <c>Library</c> builder object.</p>
    ///     <p><i>Returns the same instance of this <c>Library</c> object to chain operations.</i></p>
    ///     <i>This static factory will throw an <c>ArgumentException</c> if the supplied <c>filePath</c> does not end
    ///        with <c>.diorite</c>.
    ///     </i>
    /// </summary>
    /// <param name="filePath"> the <b>Diorite</b> source file to include in this <c>Library</c> builder </param>
    /// <returns> the same <c>Library</c> object, for chaining operations </returns>
    /// <exception cref="ArgumentException"> if the <c>filePath</c> points to a file not ending with <c>.diorite</c>
    /// </exception>
    public Library AddFile(string filePath)
    {
        if (!filePath.EndsWith(".diorite"))
            throw new ArgumentException($"File must have the .diorite file extension: {filePath}");

        var sourceContents = File.ReadAllText(filePath);
        return AddSource(sourceContents);
    }

    public override int GetHashCode() =>
        RuntimeHelpers.GetHashCode(this);

    public override bool Equals(object? obj) =>
        ReferenceEquals(this, obj);

    public override string ToString() {
        var stringBuilder = new StringBuilder("Library[\n");
        foreach (var line in _source.Split('\n'))
            stringBuilder.Append(". . . . ").Append(line).Append('\n');
        return stringBuilder.Append(']').ToString();
    }
}