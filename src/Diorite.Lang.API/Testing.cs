// testing file as the API is implemented

using Diorite.Lang.API.Syntax.Views;
using Diorite.Lang.API.Syntax.Structure;
using Diorite.Lang.Core.Errors;
using Diorite.Lang.Core.Syntax;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using Token = Diorite.Lang.API.Syntax.Structure.Token;

var variables = new [] {
    Variable.OfCharacter  ('x'),
    Variable.OfCharacter  ('y'),
    Variable.OfCharacter  ('z'),
    Variable.OfCharacter  ('A'),
    Variable.OfCharacter  ('B'),
    Variable.OfCharacter  ('C'),

    Variable.Subscriptable('x', 0),
    Variable.Subscriptable('x', 1),
    Variable.Subscriptable('x', 2),
    Variable.Subscriptable('x', 3),
    Variable.Subscriptable('x', 4),
    Variable.Subscriptable('x', 5),
    Variable.Subscriptable('x', 6),
    Variable.Subscriptable('x', 7),
    Variable.Subscriptable('x', 8),
    Variable.Subscriptable('x', 9)
};
Console.WriteLine("Variables:");
foreach (var variable in variables)
{
    Console.WriteLine($"\t{variable} => {variable.AsCoreType()}");
}

Value.FractionalRepresentation = true;
var values = new [] {
    Value.OfNumber (100),
    Value.OfNumber (3.245),
    Value.OfNumber (1 / 3.0),
    Value.OfNumber (1e-3),
    Value.OfNumber (double.MaxValue),
    Value.OfNumber (double.MinValue),
    Value.OfNumber (double.Epsilon),
    
    Value.OfComplex(25, 25),
    Value.OfComplex(0 , 25),
    Value.OfComplex(25, 0 ),
    Value.OfComplex(0 , 0 ),
    
    Value.ImUnit,
    Value.PInfinity,
    Value.NInfinity,
    Value.Undefined
};
Console.WriteLine("\nValues:");
foreach (var value in values)
{
    Console.WriteLine($"\t{value} => {value.AsCoreType()}");
    Console.WriteLine($"\t\tisComplex? => {Value.IsComplex(value)}");
    Console.WriteLine($"\t\tisNumber?  => {Value.IsNumber(value)}");
}

var tokens = Token.TokensOf("x = 2;@");
Console.WriteLine("\nTokens:");
foreach (var token in tokens)
{
    Console.WriteLine($"\t{token}");
}

var tree = Diorite.Lang.Core.Parser.TopLevelParser.ParseString("1 + 2 + 3;").ResultValue;
Console.WriteLine(AstNode<object?>.OfCoreType(tree.Head));
