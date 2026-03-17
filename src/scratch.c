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

#define DVM_INSTARGC           (3)                   /* the max number of args a dyorite vm has */
#define DVM_USRREGC            ((26*2)*(1+10))       /* the number of user registers available  */
#define DVM_STCKSIZE           ((uint64_t)(4 << 20)) /* the max stack size of the dyorite vm    */
#define DVM_INT(x)             ((DVM_Literal){.tag=DVM_LITERAL_INTEGER,.as.integer=(x)})
#define DVM_DEC(x)             ((DVM_Literal){.tag=DVM_LITERAL_DECIMAL,.as.decimal=(x)})
#define DVM_COM(a,b)           ((DVM_Literal){.tag=DVM_LITERAL_COMPLEX,.as.complex={(a),(b)}})
#define DVM_UDF                ((DVM_Literal){.tag=DVM_LITERAL_UNDEFINED})
#define DVM_ARGLIT(l)          ((DVM_InstructionArg){.tag=DVM_ARG_LITERAL,.as.lit=(l)})
#define DVM_ARGREG(r)          ((DVM_InstructionArg){.tag=DVM_ARG_REGISTER,.as.reg=(r)})
#define DVM_ARGINS(i)          ((DVM_InstructionArg){.tag=DVM_ARG_INSTRUCTION,.as.ins=(i)})
#define DVM_INST0(op)          ((DVM_Instruction){.code=(op)})
#define DVM_INST1(op,a1)       ((DVM_Instruction){.code=(op),.args={(a1)}})
#define DVM_INST2(op,a1,a2)    ((DVM_Instruction){.code=(op),.args={(a1),(a2)}})
#define DVM_INST3(op,a1,a2,a3) ((DVM_Instruction){.code=(op),.args={(a1),(a2),(a3)}})
#define DVM_RETREG             DVM_ARGREG(DVM_USRREGC+1)
#define DVM_TMPREG             DVM_ARGREG(DVM_USRREGC+2)

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

/* a dyorite vm instruction argument */
typedef struct {
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
} DVM_InstructionArg;

/* a dyorite vm instruction */
typedef struct {
    DVM_OpCode         code;
    DVM_InstructionArg args[DVM_INSTARGC];
} DVM_Instruction;

/* a dyorite vm program */
typedef struct {
    uint64_t        count;
    DVM_Instruction instructions[];
} DVM_Program;

/* a dyorite vm state */
typedef struct {
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

    vm->ip   = 0;
    vm->head = 0;
    vm->ret  = DVM_UDF;
    vm->tmp  = DVM_UDF;
    for (uint64_t reg = 0; reg < DVM_USRREGC; reg++) {
        vm->usr[reg] = DVM_UDF;
    }

    return vm;
}

// TODO: test this program
static DVM_Program prog = {
    .count = 27,
    .instructions = {
        DVM_INST2(DVM_SET, DVM_ARGREG(88), DVM_ARGLIT(DVM_COM(0, 100))),
        DVM_INST1(DVM_JMP, DVM_ARGINS(22)),

    // fn_line: ; line(x) = 2*x + 1
        DVM_INST3(DVM_MUL, DVM_RETREG, DVM_ARGLIT(DVM_INT(2)), DVM_ARGREG(253)),
        DVM_INST3(DVM_ADD, DVM_RETREG, DVM_RETREG,             DVM_ARGLIT(DVM_INT(1))),
        DVM_INST0(DVM_RET),

    // fn_quad: ; quad(x) = x^2 + 2*x + 1
        DVM_INST3(DVM_POW, DVM_RETREG, DVM_ARGREG(253), DVM_ARGLIT(DVM_INT(2))),
        DVM_INST3(DVM_MUL, DVM_TMPREG, DVM_ARGLIT(2),   DVM_ARGREG(253)),
        DVM_INST3(DVM_ADD, DVM_RETREG, DVM_RETREG,      DVM_TMPREG),
        DVM_INST3(DVM_ADD, DVM_RETREG, DVM_RETREG,      DVM_ARGLIT(DVM_INT(1))),
        DVM_INST0(DVM_RET),

    // fn_factorial: ; factorial(n) = { undefined if n < 0 ... }
        DVM_INST3(DVM_BGT, DVM_ARGREG(143), DVM_ARGLIT(DVM_INT(0)), DVM_ARGINS(13)),
        DVM_INST2(DVM_SET, DVM_RETREG,      DVM_ARGLIT(DVM_UDF)),
        DVM_INST1(DVM_JMP, DVM_ARGINS(21)),
    // fn_factorial_if1: ; factorial(n) = { ... 1 if n < 2 ... }
        DVM_INST3(DVM_BGT, DVM_ARGREG(143), DVM_ARGLIT(DVM_INT(2)), DVM_ARGINS(16)),
        DVM_INST2(DVM_SET, DVM_RETREG,      DVM_ARGLIT(DVM_INT(1))),
        DVM_INST1(DVM_JMP, DVM_ARGINS(21)),
    // fn_factorial_if2: ; factorial(n) = { ... n * f(n - 1) otherwise }
        DVM_INST1(DVM_PSH, DVM_ARGREG(143)),
        DVM_INST3(DVM_SUB, DVM_ARGREG(143), DVM_ARGREG(143),        DVM_ARGLIT(DVM_INT(1))),
        DVM_INST1(DVM_CAL, DVM_ARGINS(10)),
        DVM_INST1(DVM_POP, DVM_ARGREG(143)),
        DVM_INST3(DVM_MUL, DVM_RETREG,      DVM_ARGREG(143),        DVM_RETREG),
    // fn_factorial_ret:
        DVM_INST0(DVM_RET),

    // outer1:
        DVM_INST1(DVM_PSH, DVM_ARGREG(143)),
        DVM_INST2(DVM_SET, DVM_ARGREG(143), DVM_ARGLIT(DVM_INT(100))),
        DVM_INST1(DVM_CAL, DVM_ARGINS(10)),
        DVM_INST1(DVM_POP, DVM_ARGREG(143)),
        DVM_INST2(DVM_SET, DVM_ARGREG(253), DVM_RETREG)
    }
};

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
//     SET i_ COM(0,100)
//     JMP [outer1]
//
// fn_line: ; line(x) = 2*x + 1
//     MUL ret INT(2) x_
//     ADD ret ret    INT(1)
//     RET
//
// fn_quad: ; quad(x) = x^2 + 2*x + 1
//     POW ret x_     INT(2)
//     MUL tmp INT(2) x_
//     ADD ret ret    tmp
//     ADD ret ret    INT(1)
//     RET
//
// fn_factorial: ; factorial(n) = { undefined if n < 0 ... }
//     BGT n_  INT(0) [fn_factorial_if1]
//     SET ret UDF
//     JMP [fn_factorial_ret]
// fn_factorial_if1: ; factorial(n) = { ... 1 if n < 2 ... }
//     BGT n_  INT(2) [fn_factorial_if2]
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
