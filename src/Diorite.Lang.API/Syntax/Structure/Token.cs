// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Token.cs
// Summary: The type definition for the Token type, which is an atomic lexical unit in Diorite 
// Author:  Arsngrobg, Borngle
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API.Syntax.Structure;

/// <summary>
///     <p>A <c>Token</c> is an atomic lexical unit in the <b>Diorite</b> language.</p>
///     <p><i>This is a <b>Read-Only</b> type, meaning they cannot and should not be instantiated outside of API
///           functions.
///     </i></p>
/// </summary>
public class Token
{
    /// <summary>
    ///     <p><i>For internal API usage only</i></p>
    ///     <p>Creates a new <c>Token</c> from the supplied core <c>token</c> value, by mapping its data to its
    ///        API type. The API <c>Token</c> type is lossy, as it does not contain the actual payload data the token
    ///        may have. It is a purely structured representation of the <c>Token</c>.
    ///     </p>
    /// </summary>
    /// <param name="token"> the core <c>Token</c> to map to its API type </param>
    /// <returns> the core <c>Token</c> type mapped to its API <c>Token</c> type </returns>
    internal static Token FromCoreType(Core.Syntax.Token token)
    {
        var type = (Kind) token.id.Tag; // STABLE AS LONG AS THE ABI IS ALSO STABLE
        return new Token(token.lexeme, type, token.line, token.column);
    }

    /// <summary>
    ///     <p>The <c>Kind</c> enum is an identifier type for a lexical <c>Token</c> in the <b>Diorite</b>
    ///        language.
    ///     </p>
    /// </summary>
    public enum Kind
    {
        /// <summary>
        ///     <p>Any string literal that is not recognised by the <b>Diorite</b> language.</p>
        ///     <p>This indicates the lexer failed to recognise the lexeme, hence an error has occurred.</p>
        /// </summary>
        IllegalToken,
        /// <summary>
        ///     <p>A floating-point decimal number.</p>
        ///     <p>Any integer or decimal representation.</p>
        /// </summary>
        Number,
        /// <summary>
        ///     <p>A character followed by an optional encoded subscript.</p>
        ///     <p>A variable is a tuple consisting of the character that represents it, and the encoded subscript.
        ///        The encoded subscript is an unsigned, 8-bit integer that should be within the range of <c>0</c> to
        ///        <c>11</c>. Where an internal subscript of <c>0</c> is just the plain-text character, and a subscript
        ///        of <c>1</c> through <c>10</c> are the characters with the number minus one.
        ///     </p>
        ///     <p>A variable represents <i>some</i> value (<c>ValueType</c>) in memory, whether that be the
        ///        interpreter's virtual memory, or the device's hardware memory when compiled.
        ///     </p>
        /// </summary>
        Variable,
        /// <summary>
        ///     <p>Any string literal that is not already reserved by the <b>Diorite</b> language.</p>
        ///     <p>It is an alias for a <b>Diorite</b> function that has been declared to have such name.</p>
        /// </summary>
        Symbol,
        /// <summary>
        ///     <p>The string literal <c>"undefined"</c></p>
        ///     <p>The equivalent representation of the absence of a value.</p>
        /// </summary>
        Undefined,
        /// <summary>
        ///     <p>The string literal <c>"infinity"</c>/<c>"inf"</c></p>
        ///     <p>The representation of an extremely large, positive-bound value.</p>
        /// </summary>
        Infinity,
        /// <summary>
        ///     <p>The string literal <c>"complex"</c>.</p>
        ///     <p>A constructor for a complex value.</p>
        /// </summary>
        Complex,
        /// <summary>
        ///     <p>The string literal <c>"im"</c>.</p>
        ///     <p>A destructor for a complex value, will extract the imaginary value from the complex value.</p>
        /// </summary>
        Im,
        /// <summary>
        ///     <p>The string literal <c>"re"</c>.</p>
        ///     <p>A destructor for a complex value, will extract the real value from the complex value.</p>
        /// </summary>
        Re,
        /// <summary>
        ///     <p>The string literal <c>"plot"</c>.</p>
        ///     <p>Plots the function that is on the right-hand side of this token.</p>
        /// </summary>
        Plot,
        /// <summary>
        ///     <p>The string literal <c>"using"</c>.</p>
        ///     <p>Defines the primary argument for the anonymous function using the <c>plot</c> syntax.</p>
        /// </summary>
        Using,
        /// <summary>
        ///     <p>the string literal <c>"error"</c>.</p>
        ///     <p>Throws a <c>MathError</c> when encountered with the optional error message given to it.</p>
        /// </summary>
        Error,
        /// <summary>
        ///     <p>The string literal <c>"pi"</c>.</p>
        ///     <p>The constant value for pi (π).</p>
        /// </summary>
        Pi,
        /// <summary>
        ///     <p>The string literal <c>"tau"</c></p>
        ///     <p>The constant value for tau (τ).</p>
        /// </summary>
        Tau,
        /// <summary>
        ///     <p>The string literal <c>"euler"</c>.</p>
        ///     <p>The constant value for Euler's constant (e).</p>
        /// </summary>
        Euler,
        /// <summary>
        ///     <p>The character literal <c>':'</c></p>
        ///     <p>The delimiter for defining the domain for a function in <b>Diorite</b>.</p>
        /// </summary>
        Colon,
        /// <summary>
        ///     <p>The string literal <c>"->"</c>.</p>
        ///     <p>The delimiter for defining the range for a function in <b>Diorite</b>.</p>
        /// </summary>
        Arrow,
        /// <summary>
        ///     <p>The character literal <c>','</c>.</p>
        ///     <p>The delimiter for sequencing parameters or arguments for a function in <b>Diorite</b>.</p>
        /// </summary>
        Comma,
        /// <summary>
        ///     <p>The character literal <c>'^'</c>.</p>
        ///     <p>The binary exponent operator.</p>
        /// </summary>
        Hat,
        /// <summary>
        ///     <p>The character literal <c>'!'</c>.</p>
        ///     <p>The unary factorial operator.</p>
        /// </summary>
        Exclamation,
        /// <summary>
        ///     <p>The character literal <c>'*'</c>.</p>
        ///     <p>The binary multiplication operator.</p>
        /// </summary>
        Asterisk,
        /// <summary>
        ///     <p>The character literal <c>'/'</c>.</p>
        ///     <p>The binary division operator.</p>
        /// </summary>
        ForwardSlash,
        /// <summary>
        ///     <p>The string literal <c>"//"</c>.</p>
        ///     <p>The binary floor division operator.</p>
        /// </summary>
        DoubleForwardSlash,
        /// <summary>
        ///     <p>The character literal <c>'%'</c>.</p>
        ///     <p>The binary modulo operator.</p>
        /// </summary>
        Percentage,
        /// <summary>
        ///     <p>The character literal <c>'+'</c>.</p>
        ///     <p>The binary addition operator.</p>
        /// </summary>
        Plus,
        /// <summary>
        ///     <p>The character literal <c>'-'</c>.</p>
        ///     <p>The binary subtraction operator.</p>
        /// </summary>
        Hyphen,
        /// <summary>
        ///     <p>The character literal <c>'='</c>.</p>
        ///     <p>The assignment/equality operator.</p>
        /// </summary>
        Equal,
        /// <summary>
        ///     <p>The string literal <c>"!="</c>.</p>
        ///     <p>The inequality operator.</p>
        /// </summary>
        NotEqual,
        /// <summary>
        ///     <p>The character literal <c>'&lt;'</c>.</p>
        ///     <p>The strict less-than inequality operator.</p>
        /// </summary>
        LessThan,
        /// <summary>
        ///     <p>The character literal <c>'&gt;'</c>.</p>
        ///     <p>The strict greater-than inequality operator.</p>
        /// </summary>
        GreaterThan,
        /// <summary>
        ///     <p>The string literal <c>'&lt;='</c>.</p>
        ///     <p>The non-strict less-than-or-equal inequality operator.</p>
        /// </summary>
        LessThanOrEqual,
        /// <summary>
        ///     <p>The string literal <c>'&gt;='</c>.</p>
        ///     <p>The non-strict greater-than-or-equal inequality operator.</p>
        /// </summary>
        GreaterThanOrEqual,
        /// <summary>
        ///     <p>The string literal <c>"if"</c>.</p>
        ///     <p>Represents a guard condition in a piecewise expression.
        ///        It selects the correct branch if the associated boolean expression evaluates to <c>true</c>.
        ///     </p>
        /// </summary>
        If,
        /// <summary>
        ///     <p>The string literal <c>"otherwise"</c>.</p>
        ///     <p>Represents the fallback branch in a piecewise operation.
        ///        This branch is selected only if all previous conditions evaluate to <c>false</c>.
        ///     </p>
        /// </summary>
        Otherwise,
        /// <summary>
        ///     <p>The character literal <c>'('</c>.</p>
        ///     <p>Denotes the beginning of a subexpression, a sequence of parameters or arguments.</p>
        /// </summary>
        LeftParenthesis,
        /// <summary>
        ///     <p>The character literal <c>')'</c>.</p>
        ///     <p>Denotes the end of a subexpression, a sequence of parameters or arguments.</p>
        /// </summary>
        RightParenthesis,
        /// <summary>
        ///     <p>The character literal <c>'['</c>.</p>
        ///     <p>Denotes the beginning of a function metadata attribute.</p>
        /// </summary>
        LeftBracket,
        /// <summary>
        ///     <p>The character literal <c>']'</c>.</p>
        ///     <p>Denotes the end of a function metadata attribute.</p>
        /// </summary>
        RightBracket,
        /// <summary>
        ///     <p>The character literal <c>'{'</c>.</p>
        ///     <p>Denotes the beginning of a function body.</p>
        /// </summary>
        LeftBrace,
        /// <summary>
        ///     <p>The character literal <c>'}'</c>.</p>
        ///     <p>Denotes the end of a function body.</p>
        /// </summary>
        RightBrace,
        /// <summary>
        ///     <p>The character literal <c>'|'</c>.</p>
        ///     <p>States that the result of the expression they wrap should have the absolute operation.</p>
        /// </summary>
        Bar,
        /// <summary>
        ///     <p>Any string literal wrapped within a pair of double quotes (<c>"</c>).</p>
        ///     <p><i>Used in error messages</i></p>
        /// </summary>
        StringLiteral,
        /// <summary>
        ///     <p>The character literal <c>';'</c>.</p>
        ///     <p>Denotes the end of a statement in <b>Diorite</b>.</p>
        /// </summary>
        SemiColon
    }
    
    /// <summary>
    ///     <p>The string literal this <c>Token</c> represents.</p>
    /// </summary>
    public string Lexeme { get; }
    /// <summary>
    ///     <p>The identifier for this <c>Token</c>.</p>
    /// </summary>
    public Kind   Type   { get; }
    /// <summary>
    ///     <p>The line this <c>Token</c> is found on.</p>
    ///     <p>This is the relative line number from the lexer context.</p>
    /// </summary>
    public uint   Line   { get; }
    /// <summary>
    ///     <p>The column this <c>Token</c> is found on.</p>
    ///     <p>This is the relative line number from the lexer context.</p>
    /// </summary>
    public uint   Column { get; }
    
    private Token(string lexeme, Kind type, uint line, uint column) =>
        (Lexeme, Type, Line, Column) = (lexeme, type, line, column);

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a literal value or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a literal value </returns>
    public bool IsIllegal() =>
        Type is Kind.IllegalToken;
    
    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a literal value or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a literal value </returns>
    public bool IsLiteral() =>
        Type is Kind.Number
             or Kind.Undefined
             or Kind.Infinity
             or Kind.Pi
             or Kind.Tau
             or Kind.Euler
             or Kind.StringLiteral;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is an identifier or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes an identifier </returns>
    public bool IsIdentifier() =>
        Type is Kind.Variable
             or Kind.Symbol;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a keyword or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a keyword </returns>
    public bool IsKeyword() =>
        Type is Kind.Plot
             or Kind.Using
             or Kind.Error
             or Kind.If
             or Kind.Otherwise;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a binary operator or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a binary operator </returns>
    public bool IsBinaryOperator() =>
        Type is Kind.Plus
             or Kind.Hyphen
             or Kind.Asterisk
             or Kind.ForwardSlash
             or Kind.Percentage
             or Kind.DoubleForwardSlash
             or Kind.Hat
             or Kind.Complex;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a unary operator or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a unary operator </returns>
    public bool IsUnaryOperator() =>
        Type is Kind.Plus
             or Kind.Hyphen
             or Kind.Exclamation
             or Kind.Bar
             or Kind.Re
             or Kind.Im;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a comparison operator or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a comparison operator </returns>
    public bool IsComparisonOperator() =>
        Type is Kind.Equal
             or Kind.NotEqual
             or Kind.LessThan
             or Kind.GreaterThan
             or Kind.LessThanOrEqual
             or Kind.GreaterThanOrEqual;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is an operator or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes an operator </returns>
    public bool IsOperator() =>
        IsBinaryOperator() || IsUnaryOperator() || IsComparisonOperator();

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is an argument delimiter.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes an argument delimiter </returns>
    public bool IsArgumentDelimiter() =>
        Type is Kind.Comma;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a domain delimiter.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a domain delimiter </returns>
    public bool IsDomainDelimiter() =>
        Type is Kind.Colon;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a range delimiter.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a range delimiter </returns>
    public bool IsRangeDelimiter() =>
        Type is Kind.Arrow;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a grouping delimiter.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a grouping delimiter </returns>
    public bool IsGroupingDelimiter() =>
        Type is Kind.LeftParenthesis
             or Kind.RightParenthesis;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a metadata delimiter or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a metadata delimiter </returns>
    public bool IsMetadataDelimiter() =>
        Type is Kind.LeftBracket
             or Kind.RightBracket;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a block delimiter or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a block delimiter </returns>
    public bool IsBlockDelimiter() =>
        Type is Kind.LeftBrace
            or Kind.RightBrace;

    /// <summary>
    ///     <p>Checks whether this <c>Token</c> is a semicolon (<c>';'</c>) or not.</p>
    /// </summary>
    /// <returns> if this <c>Token</c> describes a semicolon (<c>';'</c>) </returns>
    public bool IsEndStatement() =>
        Type is Kind.SemiColon;

    public override int GetHashCode() =>
        HashCode.Combine(Lexeme, Type, Line, Column);

    public override bool Equals(object? obj) =>
        obj is Token other &&
        Lexeme.Equals(other.Lexeme) && Type == other.Type && Line == other.Line && Column == other.Column;

    public override string ToString() =>
        $"Token[Lexeme: {Lexeme}, Type: {Type}, Position: [{Line}:{Column}]]";
}