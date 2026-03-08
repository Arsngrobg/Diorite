// the language is so simple that it can be represented by a sequence of instructions and not a tree :P
// we will have the same amount of variables as last time: 572
// but, we add another, called 'reg' - an internal register value for function return values
// reg will be at the end of the variable table
// we will also have a 'tmp' register which is also at the end of the memory table used for intermediate
// values
// the 'reg' & 'tmp' cannot be access through the high level programming interface

open System

[<RequireQualifiedAccess>]
type ValueType =
    | Integer   of int64
    | Float     of double
    | Complex   of a: double * b: double
    | Undefined

type Register = uint16

type OpCode =
    | OpAssign  of reg:  Register * value: ValueType
    | OpAccess  of reg:  Register
    | OpAdd     of dest: Register * op1: Choice<Register, ValueType> * op2: Choice<Register, ValueType>
    | OpSub     of dest: Register * op1: Choice<Register, ValueType> * op2: Choice<Register, ValueType>
    | OpMul     of dest: Register * op1: Choice<Register, ValueType> * op2: Choice<Register, ValueType>
    | OpDiv     of dest: Register * op1: Choice<Register, ValueType> * op2: Choice<Register, ValueType>
    | OpFDiv    of dest: Register * op1: Choice<Register, ValueType> * op2: Choice<Register, ValueType>
    | OpMod     of dest: Register * op1: Choice<Register, ValueType> * op2: Choice<Register, ValueType>
    | OpExp     of dest: Register * op1: Choice<Register, ValueType> * op2: Choice<Register, ValueType>
    | OpFuncall of reg:  Register
    | OpCmp     of reg:  Register
    | OpIfeq    of op:   Choice<Register, ValueType>
    | OpIfneq   of op:   Choice<Register, ValueType>
    | OpIflt    of op:   Choice<Register, ValueType>
    | OpIflte   of op:   Choice<Register, ValueType>
    | OpIfgt    of op:   Choice<Register, ValueType>
    | OpIfgte   of op:   Choice<Register, ValueType>
    | OpJmp     of inst: uint64
    | OpRet

[<Struct>]
type Token = {
    start:  int32
    length: int32
}

type TokenizerState =
    | EOF
    | Token of value: Token * next: unit -> TokenizerState

let tokenizer(source: string): unit -> TokenizerState

type ParserState =
    | End
    | OpCode of value: OpCode * next: unit -> ParserState

let parser(source: string): unit -> ParserState

// Code Snippet Example:
// -----------------------------------------------------------------------------
// # This is some example source code
// Z = 100
//
// line(x) = 2*x + 1
// quad(x) = x^2 + 2*x + 1
//
// # the equivalent to the sum function
// f(x) = {
//     -x if x < 0
//      x otherwise
// }
//
// factorial(n) = {
//     undefined    if n < 0
//     n            if n < 2
//     n * f(n - 1) otherwise
// }
//
// x = factorial(100)
// -----------------------------------------------------------------------------
//     OpAssign  Z_                  VT#Int(100)
//     OpJmp     gbl_scope1
//
// fn_line:
//     OpMul     ret                 x_             VT#Int(2)
//     OpAdd     ret                 ret            VT#Int(1)
//     OpRet
//
// fn_quad:
//     OpExp     ret                 x_             VT#Int(2)
//     OpMul     tmp                 VT#Int(2)      x_
//     OpAdd     ret                 ret            tmp
//     OpAdd     ret                 ret            VT#Int(2)
//     OpRet
//
// fn_f:
//     OpCmp     x_
//     OpIfgte   VT#Int(0)           fn_f_1
//     OpSub     ret                 VT#Int(0)      x_
//     OpJmp     fn_f_return
// fn_f_1:
//     OpAssign  ret                 x_
// fn_f_return:
//     OpRet
//
// fn_factorial:
//     OpCmp     n_
//     OpIfgte   VT#Int(0)           fn_factorial_1
//     OpAssign  ret                 VT#Undef
// fn_factorial_1:
//     OpCmp     n_
//     OpIfgte   VT#Int(2)           fn_factorial_2
//     OpAssign  ret                 n_
//     OpJmp     fn_factorial_return
// fn_factorial_2:
//     OpAssign  tmp                 n_
//     OpSub     n_                  n_             VT#Int(1)
//     OpMul     ret                 temp           ret
// fn_factorial_return:
//     OpRet
//
// gbl_scope1:
//     OpAssign  n_                  VT#Int(100)
//     OpFuncall factorial
//     OpAssign  n_                  VT#Undef
//     OpAssign  x_                  ret
