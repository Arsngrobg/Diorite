# Diorite
An industry-grade desktop maths environment that runs in the CLI or a GUI. Designed for high-level mathematics, for
professional mathematicians.

## If Building From Source
You are required to install the [.NET 9.0 SDK](https://dotnet.microsoft.com/download) (or higher) in order to build this
project from source. If you are not check the [releases](https://github.com/Arsngrobg/Diorite/releases) tab for a
ready-to-use Diorite interpreter/compiler and an *optional* IME (Integrated Mathematics Environment) to quickly and
easily get into mathematical processing.

## Important
The language is designed to run primarily on the **Windows** operating system, as the bundled IME (Integrated
Mathematics Environment) executes on the WPF user interface platform - which is **not** supported on Linux. However,
you are still able to utilize the interpreter/compiler on Linux systems if you do so.

## Roadmap
- [ ] INT1 - Values
    - [ ] Number (Mathematicians shouldn't have to worry about the underlying type, just numbers!)
    - [ ] Rational (Any rational decimal should be represented as a fraction whenever possible)
    - [ ] Complex (Implicitly defined through the pattern: `a + bi`)
    - [ ] Imaginary Number (`i` character default reserved as `sqrt(-1)`)
    - [ ] Standard Notation (1e10 or 1*10^10)
- [ ] INT2 - Operations & Expressions
    - [ ] Binary Operations (Addition, subtraction, multiplication, exponentiation, modulo)
    - [ ] Unary Operations (plus, negation, factorial)
    - [ ] Expressions (BIDMAS)
- [ ] INT3 - Assignment & Variable Storage
    - [ ] Equations (EXPRESSION = EXPRESSION -> `2x = 4`)
    - [ ] Storage (e.g. `x = 5`)
    - [ ] Implicit Solving (e.g. `2x = 4` -> `x = 2`)
- [ ] INT4 - Functions & Statements
    - [ ] Overlap (functions and variables occupy the same memory bank `x = 2` then `x(t) = ...` overrides `x = 2`)
    - [ ] Conditionals (Functions can have a 'list' of conditions that affect the result e.g. `f(x) = undefined` for 
          negative values)
    - [ ] Iteration (Recursive functions, summations)
- [ ] INT5 - Standard Library
    - [ ] Constants
      - [ ] `π` (pi)
      - [ ] `τ` (tau)
      - [ ] `e`
      - [ ] `inf`
      - [ ] `undefined`
    - [ ] Functions
      - [ ] `sum` (Summation of functions -> sum())
      - [ ] `pow` (x to the power of y)
      - [ ] `sqrt` (Square root)
      - [ ] `cbrt` (Cube root)
      - [ ] `factorial` (5!)
      - [ ] `gcd` (Greatest common denominator of a pair of numbers)
      - [ ] `lcm` (Lowest common multiple of a pair of numbers)
      - [ ] `floor` (rounds the decimal down to the nearest integer representation)
      - [ ] `ceil` (rounds the decimal up to the nearest integer representation)
      - [ ] `sign` (returns the unit multiple of the given input i.e. +ve = `+1`, -ve = `-1`)
      - [ ] `log` (Logarithm of `x` to the given base)
      - [ ] `deg` (Convert radians to degrees)
      - [ ] `rad` (Convert degrees to radians)
      - [ ] trigonometrics (`sin`, `cos`, `tan`, `asin`, `acos`, `atan`, etc...)
      - [ ] `int` (Numerical integration on the provided function)
      - [ ] `dif` (Numerical differentiation on the provided function)
- [ ] INT6 - Function Plotting
    - [ ] Terminal Renderer (For terminal-based environments)
    - [ ] Reading points from a graph (point follows mouse cursor)
- [ ] INT7 - Errors
    - [ ] `MathError` (Infinite recursion, division by zero)
    - [ ] `SyntaxError` (Mismatched parenthesis, unusual token)
- [ ] CMP1 - Compiler
    - [ ] Compile to .NET bytecode (ilasm)
    - [ ] Standard library compiled
- [ ] CMP2 - Optimizations
    - [ ] Memoization/LRU Caching
    - [ ] Shrinking value bit-widths depending on function domain
    - [ ] compile-time function inlining and squashing
- [ ] GUI1 - Simple Editing & Execution
    - [ ] Text Editing
    - [ ] Running code (interpreting)
    - [ ] Compiling code
- [ ] GUI2 - Accelerated Function Plotting
    - [ ] GPU-processing for graph plotting
- [ ] GUI3 - Extra Features
    - [ ] Hot Reloading (as you write, the scripts execute in real-time)
    - [ ] Linter (Checking for potential infinite recursion, division by zero)
    - [ ] LSP (Modular way of developing a linter)
    - [ ] Code Completion (inference)
    - [ ] Render some standard library functions as their mathematical forms (e.g. integration as ∫)
- [ ] DOC1 - Wiki
    - [ ] Documentation on GH Pages
    - [ ] Code is Documented

<hr>
<i>Diorite</i>, Developed & Created by Arsngrobg and Borngle, <b>2025</b>

