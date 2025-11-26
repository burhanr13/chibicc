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
  IR_UBEXT,
  IR_SBEXT,
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
  // memory
  IR_LOAD,
  IR_STORE,
  IR_MEMCPY,
  // terminators
  IR_JP,
  IR_BR,
  IR_SWITCH,

  IR_MAX
} IROpc;

extern const char *ir_opc_names[IR_MAX];

#define IROPC_HASRES(opc) ((opc) < IR_STORE && (opc) != IR_RET)
#define IROPC_ISBINARY(opc)                                                    \
  ((IR_ADD <= (opc) && (opc) <= IR_SRA) || IROPC_ISCMP(opc))
#define IROPC_ISCOMM(opc)                                                      \
  ((opc) == IR_ADD || (opc) == IR_MUL || (opc) == IR_AND || (opc) == IR_OR ||  \
   (opc) == IR_XOR || (opc) == IR_EQ || (opc) == IR_NE)
#define IROPC_ISUNARY(opc) (IR_NEG <= (opc) && (opc) <= IR_SBEXT)
#define IROPC_ISCMP(opc) (IR_EQ <= (opc) && (opc) <= IR_ULE)
#define IROPC_ISMEM(opc) (IR_LOAD <= (opc) && (opc) <= IR_MEMCPY)
#define IROPC_ISTERM(opc) ((opc) >= IR_JP)

enum {
  LOC_UNALLOC,
  LOC_REG,
  LOC_STACK,
};

typedef struct {
  int sclass;
  union {
    int reg;
    int stackoff;
  };
} IRVarLoc;

typedef struct IRLocal IRLocal;
struct IRLocal {
  IRLocal *next;
  int id;
  int numuses;
  bool is_param;
  Obj *obj;
  IRVarLoc curloc;
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
  bool mark;
  int numuses;
  IRUser *uses;
};

typedef struct IRInstr IRInstr;
typedef struct IRBlock IRBlock;

struct IRUser {
  IRUser *next;
  IRInstr *user;
  int idx;
  bool loop;
};

struct IRInstr {
  IRValue hdr;
  IRInstr *next;
  IRInstr *prev;
  IRBlock *parent;
  IROpc opc;
  int numops;
  int size;         // used for load/store/ext
  bool is_volatile; // for memory ops
  union {
    uint64_t cval;
    Obj *gvar;
    IRLocal *lvar;
    IRValue **ops;
    IRInstr **iops;
    IRBlock **bops;
  };
  // for codegen
  int curuses;
  IRVarLoc curloc;
};

struct IRBlock {
  IRValue hdr;
  IRFunction *parent;
  IRInstr root;
  bool is_entry;
  bool is_exit;

  bool start_loop;
  bool post_loop;

  IRBlock *rpo_next;
  IRBlock *idom;

  int tmp;
};

typedef struct IRFunction IRFunction;
struct IRFunction {
  IRFunction *next;
  IRLocal *locals;
  IRLocal *params;
  IRBlock *entry;
  IRBlock *exit;
  Obj *obj;
  bool is_leaf;

  int stacksize;
};

typedef struct {
  IRFunction *funs;
  Obj *obj;
} IRProgram;

IRBlock *ir_block(IRFunction *parent);
void ir_add_instr(IRBlock *b, IRInstr *i);
void ir_insert_instr(IRInstr *before, IRInstr *i);
void ir_remove_instr(IRInstr *i);
void ir_erase_instr(IRInstr *i);
void ir_erase_block(IRBlock *b);
void ir_set_op(IRInstr *i, int op, IRValue *v);
void ir_remove_user(IRValue *v, IRInstr *u, int op);
IRValue *ir_replace(IRValue *old, IRValue *new);
void ir_merge_block(IRBlock *base, IRBlock *extra);
void ir_reset_marks(IRValue *v);

IRInstr *ir_instr(int numops);
IRInstr *ir_const(uint64_t val);
IRInstr *ir_varptr(Obj *var);
IRInstr *ir_unary(IROpc opc, IRInstr *lhs);
IRInstr *ir_binary(IROpc opc, IRInstr *lhs, IRInstr *rhs);
IRInstr *ir_extend(IROpc opc, IRInstr *src, int size);
IRInstr *ir_call(IRInstr *f, int nargs, IRInstr **args);
IRInstr *ir_ret(IRInstr *lhs);
IRInstr *ir_load(Type *ty, IRInstr *addr);
IRInstr *ir_store(Type *ty, IRInstr *addr, IRInstr *data);
IRInstr *ir_jump(IRBlock *dst);
IRInstr *ir_branch(IRInstr *cond, IRBlock *bt, IRBlock *bf);
IRInstr *ir_switch(IRInstr *cond, IRBlock *dfl, int ncase, IRBlock **cases);

IRProgram *irgen(Obj *p);
void irprint(IRProgram *p, FILE *out);
void ircodegen(IRProgram *p, FILE *out);

#define IRB_ITER(i, b)                                                         \
  for (IRInstr *i = (b)->root.next, *_next = i->next; i != &(b)->root;         \
       i = _next, _next = _next->next)
#define IRB_FIRST(b) ((b)->root.next)
#define IRB_LAST(b) ((b)->root.prev)
#define IRB_ISEMPTY(b) ((b)->root.next == &(b)->root)
#define IRB_PREDS(it, _b)                                                      \
  for (struct {                                                                \
         IRUser *u;                                                            \
         IRBlock *b;                                                           \
       } it = {(_b)->hdr.uses};                                                \
       it.u && (it.b = it.u->user->parent, true); it.u = it.u->next)
#define IRB_SUCCS(it, b) IRI_ITERB(it, IRB_LAST(b))
#define IRB_SUCCS_R(it, b) IRI_ITERB_R(it, IRB_LAST(b))
#define IRI_ITER(it, _i)                                                       \
  for (struct {                                                                \
         int idx;                                                              \
         IRValue *v;                                                           \
       } it = {0};                                                             \
       it.idx < (_i)->numops && (it.v = (_i)->ops[it.idx], true); it.idx++)
#define IRI_ITERI(it, _i)                                                      \
  for (                                                                        \
      struct {                                                                 \
        int _idx;                                                              \
        IRInstr *i;                                                            \
      } it = {0};                                                              \
      ({                                                                       \
        while (it._idx < (_i)->numops) {                                       \
          if ((it.i = (_i)->iops[it._idx])->hdr.vt == IRV_INSTR)               \
            break;                                                             \
          it._idx++;                                                           \
        }                                                                      \
      }),                                                                      \
        it._idx < (_i)->numops;                                                \
      it._idx++)
#define IRI_ITERB(it, _i)                                                      \
  for (                                                                        \
      struct {                                                                 \
        int _idx;                                                              \
        IRBlock *b;                                                            \
      } it = {0};                                                              \
      ({                                                                       \
        while (it._idx < (_i)->numops) {                                       \
          if ((it.b = (_i)->bops[it._idx])->hdr.vt == IRV_BLOCK)               \
            break;                                                             \
          it._idx++;                                                           \
        }                                                                      \
      }),                                                                      \
        it._idx < (_i)->numops;                                                \
      it._idx++)
#define IRI_ITERB_R(it, _i)                                                    \
  for (                                                                        \
      struct {                                                                 \
        int _idx;                                                              \
        IRBlock *b;                                                            \
      } it = {(_i)->numops - 1};                                               \
      ({                                                                       \
        while (it._idx >= 0) {                                                 \
          if ((it.b = (_i)->bops[it._idx])->hdr.vt == IRV_BLOCK)               \
            break;                                                             \
          it._idx--;                                                           \
        }                                                                      \
      }),                                                                      \
        it._idx >= 0;                                                          \
      it._idx--)

void irpass_opt_cfg(IRFunction *f);
void irpass_constant_fold(IRFunction *f);
void irpass_dce(IRFunction *f);
