#pragma once

#include "chibicc.h"

typedef enum {
  // constants
  IR_CONST,
  IR_GLOBALPTR,
  IR_LOCALPTR,
  // binary ops
  IR_ADD,
  IR_SUB,
  IR_MUL,
  IR_SDIV,
  IR_SMOD,
  IR_UDIV,
  IR_UMOD,
  IR_AND,
  IR_OR,
  IR_XOR,
  IR_SLL,
  IR_SRL,
  IR_SRA,
  // unary ops
  IR_NEG,
  IR_NOT,
  // extension
  IR_UEXT,
  IR_SEXT,
  // bitfield
  IR_UBFE,
  IR_SBFE,
  IR_BFI,
  // comparisons
  IR_EQ,
  IR_NE,
  IR_SLT,
  IR_SLE,
  IR_ULT,
  IR_ULE,
  // call
  IR_CALL,
  IR_RET,
  // variables/memory/constant
  IR_ULOAD,
  IR_SLOAD,
  IR_STORE,
  IR_MEMCPY,
  // terminators
  IR_JP,
  IR_BR,
  IR_SWITCH,

  IR_MAX
} IROpc;

#define IROPC_HASRES(opc) ((opc) < IR_STORE && (opc) != IR_RET)
#define IROPC_ISTERM(opc) ((opc) >= IR_JP)

typedef struct IRLocal IRLocal;
struct IRLocal {
  IRLocal *next;
  int id;
  int numuses;
  int offset;
  bool is_param;
  Obj *obj;
};

enum {
  IRV_INSTR,
  IRV_BLOCK,
};

typedef struct IRValue IRValue;

typedef struct IRUser IRUser;

struct IRValue {
  int vt;
  int id;
  bool visited;
  int numuses;
  IRUser *uses;
};

typedef struct IRInstr IRInstr;
typedef struct IRBlock IRBlock;

struct IRUser {
  IRUser *next;
  IRInstr *user;
  int idx;
};

struct IRInstr {
  IRValue hdr;
  IRInstr *next;
  IRInstr *prev;
  IROpc opc;
  int numops;
  int size; // used for load/store/ext
  union {
    uint64_t cval;
    Obj *gvar;
    IRLocal *lvar;
    IRValue **ops;
    IRInstr **iops;
    IRBlock **bops;
  };
  // for codegen
  int curloc;
  int curuses;
};

struct IRBlock {
  IRValue hdr;
  IRInstr root;
  bool is_entry;
  bool is_exit;

  IRBlock *rpo_next;
};

typedef struct IRFunction IRFunction;
struct IRFunction {
  IRFunction *next;
  IRLocal *locals;
  IRLocal *params;
  IRBlock *entry;
  IRBlock *exit;
  Obj *obj;
  int vctr;
  bool is_leaf;

  int stacksize;
};

typedef struct {
  IRFunction *funs;
  Obj *obj;
} IRProgram;

void ir_add_instr(IRBlock *b, IRInstr *i);
void ir_insert_instr(IRInstr *before, IRInstr *i);
void ir_remove_instr(IRInstr *i);
void ir_erase_instr(IRInstr *i);
void ir_set_op(IRInstr *i, int op, IRValue *v);
void ir_remove_user(IRValue *v, IRInstr *u);

IRProgram *irgen(Obj *p);
void irprint(IRProgram *p, FILE *out);
void ircodegen(IRProgram *p, FILE *out);

void ir_begin_pass(IRValue *v);

void irpass_fix_cfg(IRFunction *f);
void irpass_setup_rpo(IRFunction *f);

#define IRB_ITER(i, b)                                                         \
  for (IRInstr *i = (b)->root.next, *_next = i->next; i != &(b)->root;         \
       i = _next, _next = _next->next)
#define IRB_FIRST(b) ((b)->root.next)
#define IRB_LAST(b) ((b)->root.prev)
#define IRB_ISEMPTY(b) ((b)->root.next == &(b)->root)

#define IR_RUNPASS(pass, p)                                                    \
  for (IRFunction *f = (p)->funs; f; f = f->next)                              \
    pass(f);
