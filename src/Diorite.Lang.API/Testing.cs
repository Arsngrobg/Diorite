// testing file as the API is implemented

using Diorite.Lang.API;
using Diorite.Lang.API.Syntax.Views;
using Diorite.Lang.API.Syntax.Structure;
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

Console.WriteLine("\nTrees:");
var ast = Ast.OfSource("""
100;
3.245;
1/3;
1*10^-3;
complex(25, 25);
complex(0,  25);
complex(25, 0 );
complex(0,  0 );
pi;
tau;
euler;
+inf;
-inf;
undefined;

x = 1200*(2139+124);

[symbol:abs]
f(x : Z) -> Z = {
    -x if x < 0; x otherwise;
}

[symbol:sin_loop]
f(x : R, n : Z, y : R, z : R) -> R = {
    z if n >= 10; # Limiting to 10 terms
    f(
        x,
        n + 1,
        y * -1.0 * x * x / ((2 * n) * (2 * n + 1)),
        z + y * -1.0 * x * x / ((2 * n) * (2 * n + 1))
    ) otherwise;
}

[symbol:sin]
f(x : R) -> R = sin_loop(x, 1, x, x);

abs(-2.43);
""");
Console.WriteLine(ast.TreeStr());

var error = DioriteError.OfMathError();
Console.WriteLine(error.Message);

Console.WriteLine("\nMemory:");
