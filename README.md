# Diorite
An industry-grade desktop maths environment that runs in the CL or a GUI. Designed for high-level mathematics, for
professional mathematicians.

## IMPORTANT
This software is primarily for Windows as the bundled IDE uses WPF on the .NET platform, but you can still use the interpreter/compiler software on Linux systems perfectly fine.

# Base Requirements
- [ ] Interpreter (F#)
  - [ ] Interpreting and executing arithmetic expressions
  - [ ] Variable assignment
  - [ ] Variable usage in expressions
- [ ] IDE (C#)

# Ideas
- LSP (probably as we write the IME)
- Wiki (GH Pages, on a separate branch)
- Transpile to C (optional as the document states)

- [ ] INT1 - Expressions
    - [ ] Number
    - [ ] Rational
    - [ ] Complex
- [ ] INT2 - Assignment
    - [ ] Variables
    - [ ] Functions (f(x), g(x), etc...)
- [ ] INT3 - Statements
    - [ ] [Conditional Functions](https://tex.stackexchange.com/questions/47170/how-to-write-conditional-equations-with-one-sided-curly-brackets)
    - [ ] Iteration (recursion-ish, sum, lim)
    - [ ] Plotting (plot(function_name, min, max, scale))
- [ ] INT4 - Compiler
    - [ ] Compilation to C code
    - [ ] Optimizations (type inference, plotting)
- [ ] INT5 - IME
    - [ ] Text editing
    - [ ] Run code
    - [ ] Compile code
    - [ ] Hot-Reloading (as you program)
    - [ ] Linting (OPTIONAL)
    - [ ] Code Completion (OPTIONAL)
    - [ ] LSP (OPTIONAL)
- [ ] INT6 - Wiki/Docs
    - [ ] Website on GH pages
