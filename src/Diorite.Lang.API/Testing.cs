// testing file as the API is implemented

using Diorite.Lang.API.Syntax.Views;

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
    Variable.Subscriptable('x', 9),
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
}
