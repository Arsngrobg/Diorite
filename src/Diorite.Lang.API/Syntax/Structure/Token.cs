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

public class Token
{
    public string    Lexeme { get; }
    public TokenType Type   { get; }
    public uint      Line   { get; }
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

    public override string ToString() =>
        $"Token[type={Type}, lexeme={Lexeme}, line={Line}, column={Column}]";
}