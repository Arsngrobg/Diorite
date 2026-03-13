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
        DVM_INTEGER,
        DVM_DECIMAL,
        DVM_COMPLEX,
        DVM_UNDEFINED
    } type;
    union {
        int64_t                 integer; /* DVM_INTEGER */
        double                  decimal; /* DVM_DECIMAL */
        struct { double a, b; } complex; /* DVM_COMPLEX */
    } as;
} DVM_Literal;

#define DVM_INT(x)        (DVM_Literal) { .type = DVM_INTEGER,  .as.integer = (x)          }
#define DVM_FLOAT(x)      (DVM_Literal) { .type = DVM_DECIMAL,  .as.decimal = (x)          }
#define DVM_COMPLEX(a, b) (DVM_Literal) { .type = DVM_COMPLEX,  .as.complex = { (a), (b) } }
#define DVM_UNDEFINED     (DVM_Literal) { .type = DVM_UNDEFINED                            }

/* key:
   r  = register
   l  = literal
   rl = register | literal
   i  = instruction
*/
typedef enum {
    DVM_SET, /* SET r , rl     */
    DVM_GET, /* GET r          */
    DVM_ADD, /* ADD r , rl, rl */
    DVM_SUB, /* SUB r , rl, rl */
    DVM_MUL, /* MUL r , rl, rl */
    DVM_DIV, /* DIV r , rl, rl */
    DVM_FDV, /* FDV r , rl, rl */
    DVM_MOD, /* MOD r , rl, rl */
    DVM_EXP, /* EXP r , rl, rl */
    DVM_FCT, /* FCT r , rl, rl */
    DVM_CMP, /* CMP rl         */
    DVM_EQL, /* EQL rl, rl, i  */
    DVM_NEQ, /* NEQ rl, rl, i  */
    DVM_LTN, /* LTN rl, rl, i  */
    DVM_LTE, /* LTE rl, rl, i  */
    DVM_GTN, /* GTN rl, rl, i  */
    DVM_GTE, /* GTE rl, rl, i  */
    DVM_JMP, /* JMP i          */
    DVM_RET, /* RET            */
    DVM_PSH, /* PSH rl         */
} DVM_OpCode;

/* a dyorite vm instruction argument */
typedef struct {
    enum {
        DVM_REG,
        DVM_LIT,
        DVM_INS
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
// i = complex(0, 1)
//
// line(x) = 2*x + 1
// quad(x) = x^2 + 2*x + 1
//
// factorial(n) = {
//     undefined    if n < 0
//     1            if n < 2
//     n * f(n - 1) otherwise
// }
//
// x = factorial(100)
// -----------------------------------------------------------------------------
//     SET i_ COMPLEX(0, 100)
//     JMP [outer1]
//
// fn_line: ; line(x) = 2*x + 1
//     MUL ret INT(2) x_
//     ADD ret ret    INT(1)
//     RET
//
// fn_quad: ; quad(x) = x^2 + 2*x + 1
//     EXP ret x_     INT(2)
//     MUL tmp INT(2)
//     ADD ret ret    tmp
//     ADD ret ret    INT(1)
//     RET
//
// fn_factorial: ; factorial(n) = { undefined if n < 0 ... }
//     CMP n_
//     GTE INT(0) [fn_factorial_if1]
//     SET ret UNDEFINED
//     JMP [fn_factorial_ret]
// fn_factorial_if1: ; factorial(n) = { ... 1 if n < 2 ... }
//     CMP n_
//     GTE INT(2) [fn_factorial_if2]
//     SET ret INT(1)
//     JMP [fn_factorial_ret]
// fn_factorial_if2: ; factorial(n) = { ... n * f(n - 1) otherwise }
//     PSH n_
//     PSH [fn_factorial_call1]
//     SUB n_  n_  1
//     JMP [fn_factorial]
// fn_factorial_call1:
//     POP n_
//     MUL ret n_  ret
// fn_factorial_ret:
//     RET
//
// outer1:
//     PSH n_
//     PSH [fn_factorial_call2]
//     SET n_  INT(100)
//     JMP [fn_factorial]
// fn_factorial_call2:
//     POP n_
//     SET x_  ret
