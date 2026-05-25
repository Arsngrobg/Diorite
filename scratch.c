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
#include <complex.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#define DVM_INSTARGC           (3)                   // the max number of args a dyorite vm has
#define DVM_USRREGC            ((26*2)*(1+10))       // the number of user registers available
#define DVM_STCKSIZE           (((uint64_t)4 << 20)) // the max stack size of the dyorite vm
#define DVM_LITFPTR(l)         _Generic((l),DVM_Literal:(&(l)),DVM_Literal*:(l))
#define DVM_DEC(x)             ((DVM_Literal){.tag=DVM_LITERAL_DECIMAL,.as.decimal=(x)})
#define DVM_COM(a,b)           ((DVM_Literal){.tag=DVM_LITERAL_COMPLEX,.as.complex={(a),(b)}})
#define DVM_UDF                ((DVM_Literal){.tag=DVM_LITERAL_UNDEFINED})
#define DVM_RE(z)              (DVM_LITFPTR(z)->as.complex.a)
#define DVM_IM(z)              (DVM_LITFPTR(z)->as.complex.b)
#define DVM_ARGLIT(l)          ((DVM_InstructionArg){.tag=DVM_ARG_LITERAL,.as.lit=(l)})
#define DVM_ARGREG(r)          ((DVM_InstructionArg){.tag=DVM_ARG_REGISTER,.as.reg=(r)})
#define DVM_ARGINS(i)          ((DVM_InstructionArg){.tag=DVM_ARG_INSTRUCTION,.as.ins=(i)})
#define DVM_INST0(op)          ((DVM_Instruction){.code=(op)})
#define DVM_INST1(op,a1)       ((DVM_Instruction){.code=(op),.args={(a1)}})
#define DVM_INST2(op,a1,a2)    ((DVM_Instruction){.code=(op),.args={(a1),(a2)}})
#define DVM_INST3(op,a1,a2,a3) ((DVM_Instruction){.code=(op),.args={(a1),(a2),(a3)}})
#define DVM_RETREG             DVM_ARGREG(DVM_USRREGC+1)
#define DVM_TMPREG             DVM_ARGREG(DVM_USRREGC+2)

#define DVM_PI                 (3.14159265359)
#define DVM_PI2                (DVM_PI*2)

/* a dyorite literal value */
typedef struct {
    enum {
        DVM_LITERAL_DECIMAL,
        DVM_LITERAL_COMPLEX,
        DVM_LITERAL_UNDEFINED
    } tag;
    union {
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
    DVM_SET, /* SET r , rl,    */
    DVM_ADD, /* ADD r , rl, rl */
    DVM_SUB, /* SUB r , rl, rl */
    DVM_MUL, /* MUL r , rl, rl */
    DVM_DIV, /* DIV r , rl, rl */
    DVM_FDV, /* FDV r , rl, rl */
    DVM_MOD, /* MOD r , rl, rl */
    DVM_POW, /* EXP r , rl, rl */
    DVM_FCT, /* FCT r , rl,    */
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
//     MUL ret DEC(2) x_
//     ADD ret ret    DEC(1)
//     RET
//
// fn_quad: ; quad(x) = x^2 + 2*x + 1
//     POW ret x_     DEC(2)
//     MUL tmp DEC(2) x_
//     ADD ret ret    tmp
//     ADD ret ret    DEC(1)
//     RET
//
// fn_factorial: ; factorial(n) = { undefined if n < 0 ... }
//     BGT n_  DEC(0) [fn_factorial_if1]
//     SET ret UDF
//     JMP [fn_factorial_ret]
// fn_factorial_if1: ; factorial(n) = { ... 1 if n < 2 ... }
//     BGT n_  DEC(2) [fn_factorial_if2]
//     SET ret DEC(1)
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
//     SET n_  DEC(100)
//     CAL [fn_factorial]
//     POP n_
//     SET x_  ret

// TODO: test this program
static DVM_Program prog = {
    .count = 27,
    .instructions = {
        DVM_INST2(DVM_SET, DVM_ARGREG(88), DVM_ARGLIT(DVM_COM(0, 100))),
        DVM_INST1(DVM_JMP, DVM_ARGINS(22)),

    // fn_line: ; line(x) = 2*x + 1
        DVM_INST3(DVM_MUL, DVM_RETREG, DVM_ARGLIT(DVM_DEC(2)), DVM_ARGREG(253)),
        DVM_INST3(DVM_ADD, DVM_RETREG, DVM_RETREG,             DVM_ARGLIT(DVM_DEC(1))),
        DVM_INST0(DVM_RET),

    // fn_quad: ; quad(x) = x^2 + 2*x + 1
        DVM_INST3(DVM_POW, DVM_RETREG, DVM_ARGREG(253), DVM_ARGLIT(DVM_DEC(2))),
        DVM_INST3(DVM_MUL, DVM_TMPREG, DVM_ARGLIT(2),   DVM_ARGREG(253)),
        DVM_INST3(DVM_ADD, DVM_RETREG, DVM_RETREG,      DVM_TMPREG),
        DVM_INST3(DVM_ADD, DVM_RETREG, DVM_RETREG,      DVM_ARGLIT(DVM_DEC(1))),
        DVM_INST0(DVM_RET),

    // fn_factorial: ; factorial(n) = { undefined if n < 0 ... }
        DVM_INST3(DVM_BGT, DVM_ARGREG(143), DVM_ARGLIT(DVM_DEC(0)), DVM_ARGINS(13)),
        DVM_INST2(DVM_SET, DVM_RETREG,      DVM_ARGLIT(DVM_UDF)),
        DVM_INST1(DVM_JMP, DVM_ARGINS(21)),
    // fn_factorial_if1: ; factorial(n) = { ... 1 if n < 2 ... }
        DVM_INST3(DVM_BGT, DVM_ARGREG(143), DVM_ARGLIT(DVM_DEC(2)), DVM_ARGINS(16)),
        DVM_INST2(DVM_SET, DVM_RETREG,      DVM_ARGLIT(DVM_DEC(1))),
        DVM_INST1(DVM_JMP, DVM_ARGINS(21)),
    // fn_factorial_if2: ; factorial(n) = { ... n * f(n - 1) otherwise }
        DVM_INST1(DVM_PSH, DVM_ARGREG(143)),
        DVM_INST3(DVM_SUB, DVM_ARGREG(143), DVM_ARGREG(143),        DVM_ARGLIT(DVM_DEC(1))),
        DVM_INST1(DVM_CAL, DVM_ARGINS(10)),
        DVM_INST1(DVM_POP, DVM_ARGREG(143)),
        DVM_INST3(DVM_MUL, DVM_RETREG,      DVM_ARGREG(143),        DVM_RETREG),
    // fn_factorial_ret:
        DVM_INST0(DVM_RET),

    // outer1:
        DVM_INST1(DVM_PSH, DVM_ARGREG(143)),
        DVM_INST2(DVM_SET, DVM_ARGREG(143), DVM_ARGLIT(DVM_DEC(100))),
        DVM_INST1(DVM_CAL, DVM_ARGINS(10)),
        DVM_INST1(DVM_POP, DVM_ARGREG(143)),
        DVM_INST2(DVM_SET, DVM_ARGREG(253), DVM_RETREG)
    }
};

// docs reference:
// typedef enum {
//     DVM_GET, /* GET r ,        */
//     DVM_SET, /* SET r , rl,    */
//     DVM_ADD, /* ADD r , rl, rl */
//     DVM_SUB, /* SUB r , rl, rl */
//     DVM_MUL, /* MUL r , rl, rl */
//     DVM_DIV, /* DIV r , rl, rl */
//     DVM_FDV, /* FDV r , rl, rl */
//     DVM_MOD, /* MOD r , rl, rl */
//     DVM_POW, /* EXP r , rl, rl */
//     DVM_FCT, /* FCT r , rl     */
//     DVM_BEQ, /* BEQ i , rl, rl */
//     DVM_BNE, /* BNE i , rl, rl */
//     DVM_BLT, /* BLT i , rl, rl */
//     DVM_BLE, /* BLE i , rl, rl */
//     DVM_BGT, /* BGT i , rl, rl */
//     DVM_BGE, /* BGE i , rl, rl */
//     DVM_JMP, /* JMP i          */
//     DVM_CAL, /* CAL i          */
//     DVM_RET, /* RET            */
//     DVM_PSH, /* PSH rl         */
//     DVM_POP  /* POP r          */
// } DVM_OpCode;
//
// typedef struct {
//     enum {
//         DVM_LITERAL_DECIMAL,  (0x0)
//         DVM_LITERAL_COMPLEX,  (0x1)
//         DVM_LITERAL_UNDEFINED (0x2)
//     } tag;
//     union {
//         double                  decimal;
//         struct { double a, b; } complex;
//     } as;
// } DVM_Literal;

// TODO: needs testing
int32_t dvm_exec(DVM *vm, DVM_Program *prog) {
    assert(vm != NULL); assert(prog != NULL);

    vm->ip   = 0;
    vm->head = 0;

    DVM_InstructionArg *arg1;
    DVM_InstructionArg *arg2;
    DVM_InstructionArg *arg3;

    DVM_Literal *rl1;
    DVM_Literal *rl2;
    uint8_t packedtag;

    while (vm->ip < prog->count) {
        DVM_Instruction inst = prog->instructions[vm->ip];
        switch (inst.code) {
            case DVM_GET: /* GET r         */
                arg1 = &inst.args[0];
                switch (arg1->as.lit.tag) {
                    case DVM_LITERAL_DECIMAL:
                        printf("%f\n", arg1->as.lit.as.decimal);
                        break;
                    case DVM_LITERAL_COMPLEX:
                        printf("(%f, %f)\n", arg1->as.lit.as.complex.a, arg1->as.lit.as.complex.b);
                        break;
                    case DVM_LITERAL_UNDEFINED:
                        printf("undefined\n");
                        break;
                }
                break;
            case DVM_SET: /* SET r , rl    */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                vm->usr[arg1->as.reg] = (arg2->tag == DVM_ARG_REGISTER) ? vm->usr[arg2->as.reg] : arg2->as.lit;
                break;
            case DVM_ADD: /* ADD r, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                // packing the bits together to simultaneously switch-case them
                // packedtag = 0000 | 0000
                // runtime shouldn't handle type coersion
                // compiler manages that
                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL + DVM_LITERAL_DECIMAL */
                        vm->usr[arg1->as.reg].tag          = DVM_LITERAL_DECIMAL;
                        vm->usr[arg1->as.reg].as.decimal   = rl1->as.decimal + rl2->as.decimal;
                        break;
                    case 0x11: /* DVM_LITERAL_COMPLEX + DVM_LITERAL_COMPLEX */
                        vm->usr[arg1->as.reg].tag          = DVM_LITERAL_COMPLEX;
                        vm->usr[arg1->as.reg].as.complex.a = rl1->as.complex.a + rl2->as.complex.a;
                        vm->usr[arg1->as.reg].as.complex.b = rl1->as.complex.b + rl2->as.complex.b;
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported addition on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
                break;
            case DVM_SUB: /* SUB r, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL - DVM_LITERAL_DECIMAL */
                        vm->usr[arg1->as.reg].tag          = DVM_LITERAL_DECIMAL;
                        vm->usr[arg1->as.reg].as.decimal   = rl1->as.decimal - rl2->as.decimal;
                        break;
                    case 0x11: /* DVM_LITERAL_COMPLEX - DVM_LITERAL_COMPLEX */
                        vm->usr[arg1->as.reg].tag          = DVM_LITERAL_COMPLEX;
                        vm->usr[arg1->as.reg].as.complex.a = DVM_RE(rl1) - DVM_RE(rl2);
                        vm->usr[arg1->as.reg].as.complex.b = DVM_IM(rl1) - DVM_IM(rl2);
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported subtraction on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
                break;
            case DVM_MUL: /* MUL r, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL * DVM_LITERAL_DECIMAL */
                        vm->usr[arg1->as.reg].tag          = DVM_LITERAL_DECIMAL;
                        vm->usr[arg1->as.reg].as.decimal   = rl1->as.decimal * rl2->as.decimal;
                        break;
                    case 0x11: /* DVM_LITERAL_COMPLEX * DVM_LITERAL_COMPLEX */
                        vm->usr[arg1->as.reg].tag          = DVM_LITERAL_COMPLEX;
                        // (a*c - b*d) + (a*d + b*c)i
                        vm->usr[arg1->as.reg].as.complex.a = (DVM_RE(rl1) * DVM_RE(rl2)) - (DVM_RE(rl1) * DVM_RE(rl2));
                        vm->usr[arg1->as.reg].as.complex.b = (DVM_RE(rl1) * DVM_IM(rl2)) + (DVM_IM(rl1) * DVM_RE(rl2));
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported multiplication on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
                break;
            case DVM_DIV: /* DIV r, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL / DVM_LITERAL_DECIMAL */
                        vm->usr[arg1->as.reg].tag          = DVM_LITERAL_DECIMAL;
                        vm->usr[arg1->as.reg].as.decimal   = rl1->as.decimal * rl2->as.decimal;
                        break;
                    case 0x11: /* DVM_LITERAL_COMPLEX / DVM_LITERAL_COMPLEX */
                        vm->usr[arg1->as.reg].tag          = DVM_LITERAL_COMPLEX;
                        // ((a*c + b*d)/(c^2 + d^2)) + ((b*c - a*c)/(c^2 + d^2))i
                        double quotient = (DVM_RE(rl2) * DVM_RE(rl2)) + (DVM_IM(rl2) * DVM_IM(rl2));
                        if (quotient == 0) {
                            return -1;
                        }
                        vm->usr[arg1->as.reg].as.complex.a = ((DVM_RE(rl1) * DVM_RE(rl2)) + (DVM_IM(rl1) * DVM_IM(rl2))) / quotient;
                        vm->usr[arg1->as.reg].as.complex.b = ((DVM_RE(rl1) * DVM_IM(rl2)) - (DVM_IM(rl1) * DVM_RE(rl2))) / quotient;
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported division on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
                break;
            case DVM_FDV: /* FDV r, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL // DVM_LITERAL_DECIMAL */
                        vm->usr[arg1->as.reg].tag = DVM_LITERAL_DECIMAL;
                        double result = (double) ((int64_t) rl1->as.decimal) / ((int64_t) rl2->as.decimal);
                        if (rl1->as.decimal < 0 || rl2->as.decimal < 0) {
                            result--;
                        }
                        vm->usr[arg1->as.reg].as.decimal = result;
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported floor-division on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
                break;
            case DVM_MOD: /* MOD r, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL % DVM_LITERAL_DECIMAL */
                        vm->usr[arg1->as.reg].tag = DVM_LITERAL_DECIMAL;
                        vm->usr[arg1->as.reg].as.decimal = (double) ((int64_t) rl1->as.decimal) % ((int64_t) rl2->as.decimal);
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported modulo on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
                break;
            case DVM_POW: /* POW r, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL ^ DVM_LITERAL_DECIMAL */
                        vm->usr[arg1->as.reg].tag = DVM_LITERAL_DECIMAL;
                        vm->usr[arg1->as.reg].tag = pow(rl1->as.decimal, rl2->as.decimal);
                        break;
                    case 0x11: /* DVM_LITERAL_COMPLEX ^ DVM_LITERAL_COMPLEX */
                        // dyorite uses the principle value of exponents between two complex numbers
                        //   z^w    = (r^c)(e^-d*theta) * (cos(c*theta + d*ln(r)) + i*sin(c*theta + d*ln(r)))
                        //  theta   = arctan(b/a)
                        //    r     = sqrt(a^2 + b^2)
                        //  |z^w|   = (r^c)(e^-d*theta)
                        // arg(z^w) = c*theta + d*ln(r)
                        //   z^w    = |z^w| * (cos(arg(z^w)) + i*sin(arg(z^w))

                        double theta = atan(DVM_IM(rl2)/DVM_RE(rl1));
                        double r     = sqrt(DVM_RE(rl1)*DVM_IM(rl1) + DVM_RE(rl2)*DVM_IM(rl2));
                        double magzw = pow(r, DVM_RE(rl2)) * exp(-DVM_IM(rl2) * theta);
                        double argzw = DVM_RE(rl2)*theta + DVM_IM(rl2)*log(r);

                        vm->usr[arg1->as.reg].tag          = DVM_LITERAL_COMPLEX;
                        vm->usr[arg1->as.reg].as.complex.a = magzw * cos(argzw);
                        vm->usr[arg1->as.reg].as.complex.a = magzw * sin(argzw);
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported exponent on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
                break;
            case DVM_FCT: /* FCT r, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;

                switch (rl1->tag) {
                    case DVM_LITERAL_DECIMAL:
                        // dyorite implements the gamma function for factorials as we only have decimal numbers
                        // x!            ~= Stirling(x) * A(x)
                        // Stirling(x)    = sqrt(2pi) * (x+g-0.5)^(x-0.5) * e^-(x+g+0.5)
                        // A(x)           = sum( p[k]/(x+k) )
                        // Reflection(x)  = pi/(sin(pi*z)*(1-x)!)

                        // TODO: test whole negative numbers
                        static const int32_t g   = 7;
                        static const double  p[] = {
                            +0000.99999999999980993000000,
                            +0676.52036812188510000000000,
                            -1259.13921672240280000000000,
                            +0771.32342877765313000000000,
                            -0176.61502916214059000000000,
                            +0012.50734327868690500000000,
                            +0000.13857109526572012000000,
                            +0000.00000998436957801957160,
                            +0000.00000015056327351493116
                        };

                        bool   reflected = false;
                        double y1, y2, ax, t;
                        if (z < 0.5) {
                            reflected = true;
                            y1 = PI / sin(PI * z);
                            z = 1 - z;
                        }

                        z -= 1;
                        ax = p[0];
                        for (int32_t i = 1; i < sizeof(p)/sizeof(p[0]); i++) {
                            ax += p[i] / (z + i);
                        }

                        t  = z + g + 0.5;
                        y2 = 2.50662827463 * pow(t, z+0.5) * exp(-t) * ax;

                        vm->usr[arg1->as.reg].tag        = DVM_LITERAL_DECIMAL;
                        vm->usr[arg1->as.reg].as.decimal = reference ? (y1 / y2) : y2;
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported factorial on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
                break;
            case DVM_BEQ: /* BEQ i, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL == DVM_LITERAL_DECIMAL */
                        if (rl1->as.decimal == rl2->as.decimal) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    case 0x11: /* DVM_LITERAL_COMPLEX == DVM_LITERAL_COMPLEX */
                        if (DVM_RE(rl1) == DVM_RE(rl2) && DVM_IM(rl1) == DVM_IM(rl2)) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    case 0x01: /* DVM_LITERAL_DECIMAL == DVM_LITERAL_COMPLEX */
                        if (rl1->as.decimal == DVM_RE(rl2) && DVM_IM(rl2) == 0) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    case 0x10: /* DVM_LITERAL_COMPLEX == DVM_LITERAL_DECIMAL */
                        if (rl1->as.decimal == DVM_RE(rl2) && DVM_IM(rl2) == 0) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    case 0x22: /* all DVM_LITERAL_UNDEFINEDs are equal */
                        vm->ip = arg1->as.ins;
                        break;
                    case 0x01: case 0x02: case 0x10: case 0x20:
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported equality on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
            case DVM_BNE: /* BNE i, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL != DVM_LITERAL_DECIMAL */
                        if (rl1->as.decimal != rl2->as.decimal) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    case 0x11: /* DVM_LITERAL_COMPLEX != DVM_LITERAL_COMPLEX */
                        if (DVM_RE(rl1) != DVM_RE(rl2) && DVM_IM(rl1) != DVM_IM(rl2)) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    case 0x01: /* DVM_LITERAL_DECIMAL != DVM_LITERAL_COMPLEX */
                        if (rl1->as.decimal != DVM_RE(rl2) && DVM_IM(rl2) != 0) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    case 0x10: /* DVM_LITERAL_COMPLEX != DVM_LITERAL_DECIMAL */
                        if (rl1->as.decimal != DVM_RE(rl2) && DVM_IM(rl2) != 0) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    case 0x22:
                        break;
                    case 0x01: case 0x02: case 0x10: case 0x20:
                        vm->ip = arg1->as.ins;
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported inequality on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
            case DVM_BLT: /* BLT i, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL < DVM_LITERAL_DECIMAL */
                        if (rl1->as.decimal < rl2->as.decimal) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported less-than on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
            case DVM_BLE: /* BLE i, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL <= DVM_LITERAL_DECIMAL */
                        if (rl1->as.decimal <= rl2->as.decimal) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported less-than-or-equal on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
            case DVM_BGT: /* BGT i, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL > DVM_LITERAL_DECIMAL */
                        if (rl1->as.decimal > rl2->as.decimal) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported greater-than-or-equal on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
            case DVM_BGE: /* BGE i, rl, rl */
                arg1 = &inst.args[0];
                arg2 = &inst.args[1];
                arg3 = &inst.args[2];

                rl1 = (arg2->tag == DVM_ARG_REGISTER) ? &vm->usr[arg2->as.reg] : &arg2->as.lit;
                rl2 = (arg3->tag == DVM_ARG_REGISTER) ? &vm->usr[arg3->as.reg] : &arg3->as.lit;

                packedtag = (rl1->tag << 4) | (rl2->tag);
                switch (packedtag) {
                    case 0x00: /* DVM_LITERAL_DECIMAL >= DVM_LITERAL_DECIMAL */
                        if (rl1->as.decimal >= rl2->as.decimal) {
                            vm->ip = arg1->as.ins;
                        }
                        break;
                    default:
                        fprintf(stderr, "[Dyorite] unsupported greater-than-or-equal on tags %d & %d\n", rl1->tag, rl2->tag);
                        return -1;
                }
            case DVM_JMP: /* JMP i         */
                arg1 = &inst.args[0];
                vm->ip = arg1->as.ins;
                break;
            case DVM_CAL: /* CAL i         */
                // TODO: rethink stack implementation for dyorite vm
                break;
            case DVM_RET: /* RET           */
                // TODO: rethink stack implementation for dyorite vm
                break;
            case DVM_PSH: /* PSH rl        */
                // TODO: rethink stack implementation for dyorite vm
                break;
            case DVM_POP: /* POP r         */
                // TODO: rethink stack implementation for dyorite vm
        }

        return 0;
    }
}
