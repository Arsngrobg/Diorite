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
///     <p><i>This is a <b>Read-Only</b> type, meaning they cannot and should not be instantiated, rather returned by
///           API functions.
///     </i></p>
/// </summary>
public class Token
{
    /// <summary>
    ///     <p>The string literal this <c>Token</c> represents.</p>
    /// </summary>
    public string    Lexeme { get; }
    /// <summary>
    ///     <p>The identifier for this <c>Token</c>.</p>
    /// </summary>
    public TokenType Type   { get; }
    /// <summary>
    ///     <p>The line this <c>Token</c> is found on.</p>
    ///     <p>This is the relative line number from the lexer context.</p>
    /// </summary>
    public uint      Line   { get; }
    /// <summary>
    ///     <p>The column this <c>Token</c> is found on.</p>
    ///     <p>This is the relative line number from the lexer context.</p>
    /// </summary>
    public uint      Column { get; }

    private Token(string lexeme, TokenType type, uint line, uint column)
    {
        Lexeme = lexeme;
        Type   = type;
        Line   = line;
        Column = column;
    }

    public override int GetHashCode() =>
        HashCode.Combine(Lexeme, Type, Line, Column);

    public override bool Equals(object? obj) =>
        (obj is Token other) &&
        (Type == other.Type) && (Lexeme == other.Lexeme) && (Line == other.Line) && (Column == other.Column);

    public override string ToString() =>
        $"Token[type={Type}, lexeme={Lexeme}, line={Line}, column={Column}]";
}