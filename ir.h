#pragma once

#include "chibicc.h"

typedef enum {
  IR_CONST,
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
  IR_ZEXT,
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
  // branches
  IR_JP,
  IR_BR,
  IR_CALL,
  IR_RET,
  // variables
  IR_GLOBALPTR,
  IR_LOCALPTR,
  IR_ULOAD,
  IR_SLOAD,
  IR_STORE,
  IR_MEMCPY,

  IR_MAX
} IROpc;

typedef struct IRLocal IRLocal;
typedef struct IRLocal {
  IRLocal *next;
  int id;
  Obj* obj;
} IRLocal;

typedef struct IRBlock IRBlock;

typedef struct IRInst IRInst;
typedef struct IRInst {
  IRInst *next;
  IRInst *prev;
  IRInst *nextarg;
  IROpc opc;
  int id;
  IRInst *ops[2];
  IRBlock *blocks[2];
  Obj *gvar;
  IRLocal *lvar;
  uint64_t val;
  int bf_start;
  int bf_len;
  int mem_size;

  bool visited;
} IRInst;

typedef struct IRBlock {
  int id;
  IRInst *first;
  IRInst *last;
  bool terminated;

  bool visited;
} IRBlock;

typedef struct IRFunction IRFunction;
typedef struct IRFunction {
  IRFunction *next;
  IRLocal* locals;
  IRBlock *entry;
  Obj *obj;
} IRFunction;

typedef struct {
  IRFunction *funs;
  Obj* obj;
} IRProgram;
