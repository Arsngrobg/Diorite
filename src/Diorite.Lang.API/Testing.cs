// testing file as the API is implemented

using Diorite.Lang.API.Syntax.Views;

var x  = Variable.OfCharacter('x');
var y0 = Variable.Subscriptable('y', 0);
Console.WriteLine($"Variables:\n{x} => {x.AsCoreType()}\n{y0} => {y0.AsCoreType()}\n");

var number    = Value.OfNumber(1e-2);
var complex   = Value.ImUnit;
var undefined = Value.Undefined;
Console.WriteLine($"Values:\n{number} => {number.AsCoreType()}\n{complex} => {complex.AsCoreType()}");
Console.WriteLine($"{undefined} => {undefined.AsCoreType()}");
