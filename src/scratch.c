// the language is so simple that it can be represented by a sequence of instructions and without the indirection of a tree
// we will have the same amount of variables as last time: 572
// but, we add another, called 'reg' - an internal register value for function return values
// reg will be at the end of the variable table
// we will also have a 'tmp' register which is also at the end of the memory table used for intermediate values
// the 'reg' & 'tmp' cannot be access through the high level programming interface
// INSPIRED A LOT BY: https://imomath.com/index.cgi?page=asmNotesFunctions

// this is an incredibly verbose implementation of a virtual machine for a language
// aggressive optimizations will come later
// bulking then we cut

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

#define DVM_INSTARGC  (3)                   /* the max number of args a dyorite vm has */
#define DVM_USRREGC   ((26 * 2) * (1 + 10)) /* the number of user registers available  */
#define DVM_STCKSIZE  (4 << 20)             /* the max stack size of the dyorite vm    */
#define DVM_UNDEFINED ((DVM_Literal){.tag=DVM_LITERAL_UNDEFINED})

/* a dyorite literal value */
typedef struct {
    enum {
        DVM_LITERAL_INTEGER,
        DVM_LITERAL_DECIMAL,
        DVM_LITERAL_COMPLEX,
        DVM_LITERAL_UNDEFINED
    } tag;
    union {
        int64_t                 integer;
        double                  decimal;
        struct { double a, b; } complex;
    } as;
} DVM_Literal;

/* dyorite vm opcodes
   key:
    r  = register
    l  = literal
    rl = register | literal
    i  = instruction pointer
*/
typedef enum {
    DVM_GET, /* GET r ,        */
    DVM_SET, /* SET rl, rl,    */
    DVM_ADD, /* ADD r , rl, rl */
    DVM_SUB, /* SUB r , rl, rl */
    DVM_MUL, /* MUL r , rl, rl */
    DVM_DIV, /* DIV r , rl, rl */
    DVM_FDV, /* FDV r , rl, rl */
    DVM_MOD, /* MOD r , rl, rl */
    DVM_POW, /* EXP r , rl, rl */
    DVM_FCT, /* FCT r , rl, rl */
    DVM_BEQ, /* BEQ i , rl, rl */
    DVM_BNE, /* BNE i , rl, rl */
    DVM_BLT, /* BLT i , rl, rl */
    DVM_BLE, /* BLE i , rl, rl */
    DVM_BGT, /* BGT i , rl, rl */
    DVM_BGE, /* BGE i , rl, rl */
    DVM_JMP, /* JMP i          */
    DVM_CAL, /* CAL i          */
    DVM_RET, /* RET            */
    DVM_PSH, /* PSH rl         */
    DVM_POP  /* POP r          */
} DVM_OpCode;

/* a dyorite vm instruction */
typedef struct {
    DVM_OpCode code;
    struct {
        enum {
            DVM_ARG_LITERAL,
            DVM_ARG_REGISTER,
            DVM_ARG_INSTRUCTION
        } tag;
        union {
            DVM_Literal lit; /* DVM_ARG_LITERAL     */
            uint16_t    reg; /* DVM_ARG_REGISTER    */
            uint64_t    ins; /* DVM_ARG_INSTRUCTION */
        } as;
    } args[DVM_INSTARGC];
} DVM_Instruction;

/* a dyorite vm program */
typedef struct {
    uint64_t        count;
    DVM_Instruction instructions[];
} DVM_Program;

/* a dyorite vm state */
typedef struct {
    DVM_Program *prog;               /* the currently program */
    uint64_t    ip;                  /* instruction pointer   */
    uint64_t    head;                /* stack top             */
    DVM_Literal ret;                 /* return    register    */
    DVM_Literal tmp;                 /* temporary register    */
    DVM_Literal usr[DVM_USRREGC];    /* user      registers   */
    uint8_t     stack[DVM_STCKSIZE]; /* program stack         */
} DVM;

/* creates a new dyorite vm instance */
DVM *dvm_new() {
    DVM *vm = malloc(sizeof(DVM));
    if (vm == NULL) {
        fprintf(stderr, "[Dyorite] unable to allocate a new Dyorite virtual machine\n");
        return NULL;
    }

    vm->prog = NULL;
    vm->ip   = 0;
    vm->head = 0;
    vm->ret  = DVM_UNDEFINED;
    vm->tmp  = DVM_UNDEFINED;
    for (uint64_t reg = 0; reg < DVM_USRREGC; reg++) {
        vm->usr[reg] = DVM_UNDEFINED;
    }

    return vm;
}

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
//     MUL tmp INT(2) x_
//     ADD ret ret    tmp
//     ADD ret ret    INT(1)
//     RET
//
// fn_factorial: ; factorial(n) = { undefined if n < 0 ... }
//     BGT n_ INT(0) [fn_factorial_if1]
//     SET ret UNDEFINED
//     JMP [fn_factorial_ret]
// fn_factorial_if1: ; factorial(n) = { ... 1 if n < 2 ... }
//     BGT n_ INT(2) [fn_factorial_if2]
//     SET ret INT(1)
//     JMP [fn_factorial_ret]
// fn_factorial_if2: ; factorial(n) = { ... n * f(n - 1) otherwise }
//     PSH n_
//     SUB n_  n_  1
//     CAL [fn_factorial]
//     POP n_
//     MUL ret n_  ret
// fn_factorial_ret:
//     RET
//
// outer1:
//     PSH n_
//     SET n_  INT(100)
//     CAL [fn_factorial]
//     POP n_
//     SET x_  ret
