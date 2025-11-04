# Diorite
An industry-grade desktop maths environment that runs in the CLI or a GUI. Designed for high-level mathematics, for
professional mathematicians.

## If Building From Source
You are required to install the [.NET 9.0 SDK](https://dotnet.microsoft.com/download) (or higher) in order to build this
project from source. If you are not check the [releases](https://github.com/Arsngrobg/Diorite/releases) tab for a
ready-to-use Diorite interpreter/compiler and an *optional* IME (Integrated Mathematics Environment) to quickly and
easily get into mathematical processing.

# Important
The language is designed to run primarily on the **Windows** operating system, as the bundled IME (Integrated
Mathematics Environment) executes on the WPF user interface platform - which is **not** supported on Linux. However,
you are still able to utilize the interpreter/compiler on Linux systems if you do so.

# Getting Started
Install Diorite by either compiling from source or checking out the
[releases](https://github.com/Arsngrobg/Diorite/releases) tab. Store it in a safe directory, and add it to your `PATH`
variables so you can execute this command in the command line:
```fsharp
$ diorite -i
```
without any issues.

It brings up the interactive REPL environment in the terminal, and you can begin executing mathematical expressions
and write pure mathematical functions.
```fsharp
 |
 |  f(x) = {
 |      undefined           if x < 0;
 |      x                   if x < 2;
 |      f(x - 2) + f(x - 1) otherwise;
 |  }
 >  f(20);
 =  6765
>>> ▮
```
This executes the fibonacci sequence up to the 20th position.
If you want to save your state, just type `@save <filename>` and it will save the current state of your Diorite REPL in
a `.diorite` file in the current working directory.

To run your `.diorite` file, in the terminal, execute the command:
```fsharp
$ diorite -i <name>.diorite
```
After execution, you will be presented with the REPL exactly as you left it, ready to continue where you left off. Note
that if using an IDE, the integrated terminal does not format well with the REPL environment.

# Feature List
- Value Types
  - Number:    variables storing a value are implicitly `double` under the hood - but can be bound by a `set hint`
  - Variable:  character with an optional numerical subscript (x, x0, x00 are two different variables)
  - Constants:
    - `infinity`/`inf`: any operation applied on it will just return `infinity`
    - `π`/`pi`:         `3.14159265...`
    - `τ`/`tau`:        `6.28318530...`  (`2π`)
    - `e`/`euler`:      `2.718281828...` (euler's number)
- Operations (BIDMAS)
  - Binary:   exponentiation, multiplication, division, modulo, addition, and subtraction
  - Unary:    plus, minus, factorial
  - Brackets: determines order of operations
- Variables
  - Initially: `undefined`
  - Storage:   either a value, function, or expression (auto-solve if trivial - e.g. `x = 2+2`)
  - Internals: table of size `(1 + 10) * (26 * 2)`, where:
    - 1 slot:   the default variable   (e.g. `x` )
    - 10 slots: the subscript variants (e.g. `x4` - offset `subscript + 1`)
    - 26 slots: the lowercase characters
    - 26 slots: the uppercase characters <br>
    This allows for `572` uniquely defined variables - plenty
- Functions
  - Storage:    occupies same *memory* as variables
  - Internals:  the abstract syntax tree (AST) is stored in the *memory* location to be executed
  - Domain:     definable domains using set notation
  - Attributes: can be attributed with square brackets and the attribute
    - `symbol`: allows for a symbol to be assigned to this function, the symbol persists through the programs lifetime
      - `inline`: inlines the function whenever possible
      - expression functions are inlined as is
      - conditional functions only inline the value
      - `memoized`: function return values are stored in a cache to save CPU cycles on recompute
      - 'force':  forces the compiler\interpreter to use these attributes - regardless of the CLI arguments
        - `inline`
        - `memoized`
- Errors
  - `SyntaxError`: illegal token or illegal sequence of tokens / unexpected token
  - `MathError`:   division by zero, square root of negative, infinite recursion, executing operations on `undefined`
- Standard Library
  - Bootstrapped: written in the language itself
  - Examples:
```fsharp
    abs  (x: R)      : Z   functional wrapper for the absolute operation                 (abs(-2)        = |-2|)
    sign (x: R)      : Z   returns the unit multiple of x                                (sign(7612)     = 1   )
    pow  (x: R, n: R): R   functional wrapper around the power operation                 (pow(2,4) = 2^4 = 16  )
    floor(x: R)      : Z   returns the greatest integer less-than or equal-to x          (floor(2.45)    = 2   )
    ceil (x: R)      : Z   returns the lowest integer greater-than or equal-to x         (ceil(-12.54)   = -12 )
    deg  (r: R)      : R   converts the radians r to degress                             (deg(90)        = 0.5π)
    rad  (d: R)      : R   converts the degress d to radians                             (rad(0.5π)      = 90  )
    gcd  (a: Z, b: Z): Z   returns the greatest common denominator of a and b            (gcd(8,12)      = 4   )
    lcm  (a: Z, b: Z): Z   returns the lowest common multiple of a and b                 (lcm(8,12)      = 24  )
    sin  (x: R)      : R   approximates the value of sin(x) using the Taylor Series      (sin(2π)        = 0   )
    cos  (x: R)      : R   approximates the value of cos(x) using the Taylor Series      (cos(2π)        = 1   )
    tan  (x: R)      : R   approximates the value of tan(x) using the Taylor Series      (tan(2π)        = 0   )
    asin (x: R)      : R   approximates the value of arcsin(x) using the Taylor Series   (asin(0)        = 0   )
    acos (x: R)      : R   approximates the value of arccos(x) using the Taylor Series   (acos(1)        = 0   )
    atan (x: R)      : R   approximates the value of arctan(x) using the Taylor Series   (atan(0)        = 0   )
```
  - External: external linkage to the `plot(fn)` function which is implemented in *F#*.
    ```fsharp
    plot (fn)  visualises the provided function (fn) in the respective interactive environment
    ```
- Optimizations
  - Memoization: implied if not explicitly by either a function attribute or CLI argument
  - Inlining:    implied if not explicitly by either a function attribute or CLI argument
- Compilation
  - IL:     compiles to C# IL
  - Method: using ilasm
- IME
  - GUI:          modern feel, sleek
  - Extends:      extends plotting functionality by displaying it in the graphical interface using the GPU
  - Linting:      display premature `SyntaxError` messages or `MathError` if potential errors may occur
  - Highlighting: syntax highlighting
  - Execution:    compilation or interpretation (will interpret as you type)
  - Formatting:   `pi` -> `π`, `tau` = `τ`, etc...
- Wiki
  - Tutorial:      for beginners or experienced mathematicians
  - Documentation: core library, function attributes, errors, constants

##### Copyright
*Diorite*, Developed & Created by Arsngrobg and Borngle, **2025**
