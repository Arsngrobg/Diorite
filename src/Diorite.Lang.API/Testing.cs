// testing file as the API is implemented

using Diorite.Lang.API;
using Diorite.Lang.API.Library;
using Diorite.Lang.API.Syntax.Data;
using Diorite.Lang.API.Syntax.Views;
using Token = Diorite.Lang.API.Syntax.Structure.Token;

Console.WriteLine($"Running Diorite Ver{DioriteVersion.CoreVersion} using API Ver {DioriteVersion.ApiVersion}");

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
    
    Value.Pi,
    Value.Tau,
    Value.E,
    Value.PInfinity,
    Value.NInfinity,
    Value.ImUnit,
    Value.Undefined
};
Console.WriteLine("\nValues:");
foreach (var value in values)
{
    Console.WriteLine($"\t{value} => {value.AsCoreType()}");
    Console.WriteLine($"\t\tisComplex? => {Value.IsComplex(value)}");
    Console.WriteLine($"\t\tisNumber?  => {Value.IsNumber(value)}");
}

var tokens1 = Token.TokensOf("x = 2;@sakd @@ @", failIfIllegal: false);
Console.WriteLine("\nTokens:");
foreach (var token in tokens1)
{
    Console.WriteLine($"\t{token}");
}

var tokens2 = Token.TokensOf("x = 2;");
Console.WriteLine("\nTokens:");
foreach (var token in tokens2)
{
    Console.WriteLine($"\t{token}");
}

var source = """
[symbol:sum]
L(n: N) -> N = {
    0                if n = 0;
    n + sum(n - 1)   otherwise;
}

[symbol:sum_acc]
L(n, a) = {
    a                if n = 0;
    sum_acc(n-1, a+n) otherwise;
}

L = undefined;          
""";

var lib = Library.OfSource(source);
var evaluator = DioriteEvaluator.Configure(lib.BuildMemorySnapshot(), Memory.PlotDoNothing);
Console.WriteLine(evaluator.GetMemory());
Console.WriteLine(evaluator.EvaluateExpression("sum(1000)"));
