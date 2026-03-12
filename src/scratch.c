// the language is so simple that it can be represented by a sequence of instructions and without the indirection of a tree
// we will have the same amount of variables as last time: 572
// but, we add another, called 'reg' - an internal register value for function return values
// reg will be at the end of the variable table
// we will also have a 'tmp' register which is also at the end of the memory table used for intermediate values
// the 'reg' & 'tmp' cannot be access through the high level programming interface
// INSPIRED A LOT BY: https://imomath.com/index.cgi?page=asmNotesFunctions

#include <stdint.h>

#define DVM_INSTARGS 3

/* an atomic value in the dyorite language */
typedef struct {
    enum {
        DVM_LITERAL_INTEGER,
        DVM_LITERAL_FLOAT,
        DVM_LITERAL_COMPLEX,
        DVM_LITERAL_UNDEFINED
    } type;
    union {
        int64_t                 integer; /* DVM_LITERAL_INTEGER */
        double                  floot;   /* DVM_LITERAL_FLOAT   */
        struct { double a, b; } complex; /* DVM_LITERAL_COMPLEX */
    } as;
} DVM_Literal;

#define DVM_INT(x)        (DVM_Literal) { .type = DVM_LITERAL_INTEGER,  .as.integer = (x)          }
#define DVM_FLOAT(x)      (DVM_Literal) { .type = DVM_LITERAL_FLOAT,    .as.floot   = (x)          }
#define DVM_COMPLEX(a, b) (DVM_Literal) { .type = DVM_LITERAL_COMPLEX,  .as.complex = { (a), (b) } }
#define DVM_UNDEFINED     (DVM_Literal) { .type = DVM_LITERAL_UNDEFINED                            }

/* key:
   r  = register
   l  = literal
   rl = register | literal
   i  = instruction
*/
typedef enum {
    DVM_OPASSIGN, /* OpAssign  r , rl     */
    DVM_OPACCESS, /* OpAccess  r          */
    DVM_OPADD,    /* OpAdd     r , rl, rl */
    DVM_OPSUB,    /* OpSub     r , rl, rl */
    DVM_OPMUL,    /* OpMul     r , rl, rl */
    DVM_OPDIV,    /* OpDiv     r , rl, rl */
    DVM_OPFDV,    /* OpFdv     r , rl, rl */
    DVM_OPMOD,    /* OpMod     r , rl, rl */
    DVM_OPEXP,    /* OpExp     r , rl, rl */
    DVM_OPFCT,    /* OpFct     r , rl, rl */
    DVM_OPIFEQ,   /* OpIfeq    rl, rl, i  */
    DVM_OPIFNEQ,  /* OpIfneq   rl, rl, i  */
    DVM_OPIFLT,   /* OpIflt    rl, rl, i  */
    DVM_OPIFLTE,  /* OpIflte   rl, rl, i  */
    DVM_OPIFGT,   /* OpIfgt    rl, rl, i  */
    DVM_OPIFGTE,  /* OpIfgte   rl, rl, i  */
    DVM_OPJMP,    /* OpJmp     i          */
    DVM_OPRET,    /* OpRet                */
} DVM_OpCode;

/* a dyorite vm instruction argument */
typedef struct {
    enum {
        DVM_ARGREG,
        DVM_ARGLIT,
        DVM_ARGINS
    } type;
    union {
        uint64_t    r;
        DVM_Literal l;
    } payload;
} DVM_InstArg;

/* a dyorite vm instruction:
   Op<ID> [r|l|rl|ip]*
*/
typedef struct {
    DVM_OpCode  op;
    DVM_InstArg args[DVM_INSTARGS];
} DVM_Inst;

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
//     OpAssign  Z_ , INT(100)
//     OpJmp     outer1
//
// fn_line: # line(x) = 2*x + 1
//     OpMul     ret, INT(2), x_
//     OpAdd     ret, ret   , INT(1)
//     OpRet
//
// fn_quad: # quad(x) = x^2 + 2*x + 1
//     OpExp     ret, x_    , INT(2)
//     OpMul     tmp, INT(2), x_
//     OpAdd     ret, ret   , tmp
//     OpAdd     ret, ret   , INT(1)
//     OpRet
//
// fn_f: # f(x) = { -x if x < 0 }
//     OpIfgte   x_ , INT(0), fn_f_if1
//     OpSub     ret, INT(0), x_
//     OpJmp     fn_f_ret
// fn_f_if1: # f(x) = { ... x otherwise }
//     OpAssign  ret, x_
// fn_f_ret:
//     OpRet
//
// fn_factorial: # factorial(n) = { undefined if n < 0 ... }
//     OpIfgte   _n , INT(0), fn_factorial_if1
//     OpAssign  ret, UNDEF
//     OpJmp     fn_factorial_ret
// fn_factorial_if1: # factorial(n) = { ... n if n < 2 ... }
//     OpIfgte   _n , INT(2), fn_factorial_if2
//     OpAssign  ret, _n
//     OpJmp     fn_factorial_ret
// fn_factorial_if2: # factorial(n) = { ... n * f(n - 1) otherwise }
//     OpFuncall fn_factorial # some stack shit here
//     TODO
// fn_factorial_ret:
//     OpRet
//
// outer1:
//     OpAssign  n_ , INT(100)
//     OpFuncall fn_factorial
//     OpAssign  x_ , ret
