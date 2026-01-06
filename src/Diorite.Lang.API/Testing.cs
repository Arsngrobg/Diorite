// testing file as the API is implemented

using Diorite.Lang.API.Syntax;

var x  = DioriteVariable.OfCharacter('x');
var y0 = DioriteVariable.Subscriptable('y', 0);
Console.WriteLine($"Variables:\n{x} => {x.AsCoreType()}\n{y0} => {y0.AsCoreType()}\n");

var number    = DioriteValue.OfNumber(1e-2);
var complex   = DioriteValue.ImUnit;
var undefined = DioriteValue.Undefined;
Console.WriteLine($"Values:\n{number} => {number.AsCoreType()}\n{complex} => {complex.AsCoreType()}");
Console.WriteLine($"{undefined} => {undefined.AsCoreType()}");
