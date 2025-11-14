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
  // variables/memory/constant
  IR_ULOAD,
  IR_SLOAD,
  IR_STORE,
  IR_MEMCPY,
  // terminators
  IR_JP,
  IR_BR,
  IR_SWITCH,
  IR_RET,

  IR_MAX
} IROpc;

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
struct IRUser {
  IRUser *next;
  IRValue *user;
};

struct IRValue {
  int vt;
  int id;
  bool visited;
  int numuses;
  IRUser *uses;
};

typedef struct IRInstr IRInstr;
typedef struct IRBlock IRBlock;

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
  IRInstr *first;
  IRInstr *last;

  IRBlock *rpo_next;
};

typedef struct IRFunction IRFunction;
struct IRFunction {
  IRFunction *next;
  IRLocal *locals;
  IRLocal *params;
  IRBlock *entry;
  Obj *obj;
  int vctr;
  int stacksize;
};

typedef struct {
  IRFunction *funs;
  Obj *obj;
} IRProgram;

void ir_add_instr(IRBlock *b, IRInstr *i);
void ir_remove_instr(IRBlock *b, IRInstr *i);
void ir_add_user(IRValue *v, IRValue *u);
void ir_remove_user(IRValue *v, IRValue *u);
void ir_begin_pass(IRValue *v);
void ir_setup_rpo(IRBlock *b);

IRProgram *irgen(Obj *p);
void irprint(IRProgram *p, FILE *out);
void ircodegen(IRProgram *p, FILE *out);

