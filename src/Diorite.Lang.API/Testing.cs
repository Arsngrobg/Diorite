using Diorite.Lang.API;
using Diorite.Lang.API.Callbacks;
using Diorite.Lang.API.Library;
using Diorite.Lang.API.Syntax.Structure;
using Diorite.Lang.API.Syntax.Views;

// init
Console.BackgroundColor = ConsoleColor.Black;
Console.ForegroundColor = ConsoleColor.White;
Console.Clear();

PlotCallback callback = anonFn =>
{
    var offset = 5;
    Console.WriteLine("Hello World!");
    for (var y = 0; y < 10; y++)
    for (var x = 0; x < 40; x++)
    {
        Console.SetCursorPosition(x, y);
        Console.Write("`");
    }
    for (var x = 0; x < 40; x++)
        try
        {
            x += offset;
            Console.SetCursorPosition(x, (int)anonFn(Value.OfNumber(x)).Re());
            Console.Write('#');
        }
        catch (ArgumentOutOfRangeException _) {}
};

string basePath = Path.Combine(AppContext.BaseDirectory, "Library", "Stdlib", "base.diorite");
string trigPath = Path.Combine(AppContext.BaseDirectory, "Library", "Stdlib", "trigonometry.diorite");
var lib = Library.OfFile(basePath);
lib.AddFile(trigPath);

var evaluator = DioriteEvaluator.Configure(lib.BuildMemorySnapshot(), callback);
while (true)
{
    Console.Write(">>> ");
    var source = Console.ReadLine()!;
    if (string.IsNullOrEmpty(source))
        continue;
    Console.ForegroundColor = ConsoleColor.DarkGray;
    //Console.WriteLine(string.Join(", ", Token.TokensOf(source)));
    try
    {
        //Console.WriteLine(Ast.OfSource(source).TreeStr());
        var values = evaluator.EvaluateSource(source);
        Console.ForegroundColor = ConsoleColor.Green;
        Console.WriteLine($"{string.Join(", ", values)}");
        Console.ForegroundColor = ConsoleColor.White;}
    catch (DioriteError err)
    {
        Console.ForegroundColor = ConsoleColor.Red;
        Console.WriteLine(err.Message);
        Console.ForegroundColor = ConsoleColor.White;
    }
}
