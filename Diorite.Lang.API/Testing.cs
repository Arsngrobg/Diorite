// testing file as the API is implemented

using Diorite.Lang.API.Syntax;

var x  = VariableType.OfCharacter('x');
var x0 = VariableType.Subscriptable('y', 1);
Console.WriteLine($"{x}, {x0}");
Console.WriteLine($"{x.AsTuple()}, {x0.AsTuple()}");
