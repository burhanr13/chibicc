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
typedef struct IRLocal {
  IRLocal *next;
  int id;
  Obj *obj;
} IRLocal;

enum {
  IRV_INSTR,
  IRV_BLOCK,
};

typedef struct {
  int vt;
  int id;
  bool visited;
} IRValue;

typedef struct IRInstr IRInstr;
typedef struct IRInstr {
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
  };
} IRInstr;

typedef struct IRBlock {
  IRValue hdr;
  IRInstr *first;
  IRInstr *last;
  bool terminated;
} IRBlock;

typedef struct IRFunction IRFunction;
typedef struct IRFunction {
  IRFunction *next;
  IRLocal *locals;
  IRBlock *entry;
  Obj *obj;
  int vctr;
} IRFunction;

typedef struct {
  IRFunction *funs;
  Obj *obj;
} IRProgram;
