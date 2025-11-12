#include "ir.h"

static IRFunction *cur_fun;
static IRBlock *cur_block;

static IRBlock *new_block() {
  IRBlock *b = calloc(1, sizeof *b);
  b->hdr.vt = IRV_BLOCK;
  b->hdr.id = cur_fun->vctr++;
  return b;
}

static void add_instr(IRInstr *i) {
  if (cur_block->terminated) {
    free(i);
    return;
  }
  if (!cur_block->first) {
    cur_block->first = i;
    cur_block->last = i;
  } else {
    cur_block->last->next = i;
    i->prev = cur_block->last;
    cur_block->last = i;
  }
  if (i->opc >= IR_JP)
    cur_block->terminated = true;
}

static IRInstr *new_instr(int numops) {
  IRInstr *i = calloc(1, sizeof *i);
  i->hdr.vt = IRV_INSTR;
  i->hdr.id = cur_fun->vctr++;
  i->numops = numops;
  i->ops = calloc(numops, sizeof(IRValue *));
  return i;
}

static IRInstr *ir_const(uint64_t val) {
  IRInstr *i = new_instr(0);
  i->opc = IR_CONST;
  i->cval = val;
  return i;
}

static IRInstr *ir_unary(IROpc opc, IRInstr *lhs) {
  IRInstr *i = new_instr(1);
  i->opc = opc;
  i->ops[0] = (IRValue *) lhs;
  return i;
}

static IRInstr *ir_binary(IROpc opc, IRInstr *lhs, IRInstr *rhs) {
  IRInstr *i = new_instr(2);
  i->opc = opc;
  i->ops[0] = (IRValue *) lhs;
  i->ops[1] = (IRValue *) rhs;
  return i;
}

static IRInstr *ir_varptr(Obj *var) {
  IRInstr *i = new_instr(0);
  if (var->is_local) {
    i->opc = IR_LOCALPTR;
    i->lvar = var->irlocal;
  } else {
    i->opc = IR_GLOBALPTR;
    i->gvar = var;
  }
  return i;
}

static IRInstr *ir_load(Type *ty, IRInstr *addr) {
  switch (ty->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION:
  case TY_FUNC:
  case TY_VLA: return addr;
  }
  IRInstr *i = new_instr(1);
  i->opc = ty->is_unsigned ? IR_ULOAD : IR_SLOAD;
  i->ops[0] = (IRValue *) addr;
  i->size = ty->size;
  return i;
}

static IRInstr *ir_store(Type *ty, IRInstr *addr, IRInstr *data) {
  IRInstr *i = new_instr(2);
  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION: i->opc = IR_MEMCPY; break;
  default: i->opc = IR_STORE; break;
  }
  i->ops[0] = (IRValue *) addr;
  i->ops[1] = (IRValue *) data;
  i->size = ty->size;
  return i;
}

static IRInstr *ir_bitfield(IROpc opc, IRInstr *src, IRInstr *dst, int start,
                            int len) {
  IRInstr *i = new_instr(opc == IR_BFI ? 4 : 3);
  i->opc = opc;
  i->ops[0] = (IRValue *) src;
  if (opc == IR_BFI) {
    i->ops[1] = (IRValue *) dst;
    i->ops[2] = (IRValue *) ir_const(start);
    i->ops[3] = (IRValue *) ir_const(len);
  } else {
    i->ops[1] = (IRValue *) ir_const(start);
    i->ops[2] = (IRValue *) ir_const(len);
  }
  return i;
}

static IRInstr *ir_cast(IRInstr *src, Type *from, Type *to) {
  if (from->size >= to->size)
    return src;
  IRInstr *i = new_instr(1);
  i->opc = from->is_unsigned ? IR_UEXT : IR_SEXT;
  i->size = from->size;
  i->ops[0] = (IRValue *) src;
  return i;
}

static IRInstr *ir_call(IRInstr *f, int nargs, IRInstr **args) {
  IRInstr *i = new_instr(1 + nargs);
  i->opc = IR_CALL;
  i->ops[0] = (IRValue *) f;
  memcpy(&i->ops[1], args, nargs * sizeof(IRInstr *));
  return i;
}

static IRInstr *ir_ret(IRInstr *lhs) {
  IRInstr *i = new_instr(lhs ? 1 : 0);
  i->opc = IR_RET;
  if (lhs)
    i->ops[0] = (IRValue *) lhs;
  return i;
}

static IRInstr *ir_branch(IRInstr *cond, IRBlock *bt, IRBlock *bf) {
  IRInstr *i = new_instr(3);
  i->opc = IR_BR;
  i->ops[0] = (IRValue *) cond;
  i->ops[1] = (IRValue *) bt;
  i->ops[2] = (IRValue *) bf;
  return i;
}

static IRInstr *ir_jump(IRBlock *dst) {
  IRInstr *i = new_instr(1);
  i->opc = IR_JP;
  i->ops[0] = (IRValue *) dst;
  return i;
}

static IRInstr *gen_expr(Node *e);

static IRInstr *gen_addr(Node *e) {
  switch (e->kind) {
  case ND_VAR: return ir_varptr(e->var);
  case ND_DEREF: return gen_expr(e->lhs);
  case ND_MEMBER:
    return ir_binary(IR_ADD, gen_addr(e->lhs), ir_const(e->member->offset));
  case ND_COMMA:
  case ND_FUNCALL:
  case ND_ASSIGN:
  case ND_COND:
  case ND_VLA_PTR: return ir_const(0); // TODO
  }

  error_tok(e->tok, "not an lvalue");
}

static IRInstr *gen_expr(Node *e) {
  switch (e->kind) {
  case ND_NULL_EXPR: return ir_const(0);
  case ND_NUM: return ir_const(e->val);
  case ND_NEG: return ir_unary(IR_NEG, gen_expr(e->lhs));
  case ND_VAR: return ir_load(e->ty, gen_addr(e));
  case ND_MEMBER: {
    IRInstr *addr = gen_addr(e);
    IRInstr *res = ir_load(e->ty, addr);
    if (e->member->is_bitfield) {
      res = ir_bitfield(e->member->ty->is_unsigned ? IR_UBFE : IR_SBFE, res,
                        NULL, e->member->bit_offset, e->member->bit_width);
    }
    return res;
  }
  case ND_DEREF: return ir_load(e->ty, gen_expr(e->lhs));
  case ND_ADDR: return gen_addr(e->lhs);
  case ND_ASSIGN: {
    IRInstr *val = gen_expr(e->rhs);
    IRInstr *addr = gen_addr(e->lhs);
    IRInstr *stval = val;
    if (e->lhs->kind == ND_MEMBER && e->lhs->member->is_bitfield) {
      IRInstr *dst = ir_load(e->lhs->member->ty, addr);
      stval = ir_bitfield(IR_BFI, val, dst, e->lhs->member->bit_offset,
                          e->lhs->member->bit_width);
    }
    add_instr(ir_store(e->ty, addr, stval));
    return val;
  }
  case ND_STMT_EXPR: return ir_const(0); // TODO
  case ND_COMMA: add_instr(gen_expr(e->lhs)); return gen_expr(e->rhs);
  case ND_CAST: return ir_cast(gen_expr(e->lhs), e->lhs->ty, e->ty);
  case ND_MEMZERO: return ir_const(0); // TODO
  case ND_LOGAND:
  case ND_LOGOR:
  case ND_COND: return ir_const(0); // TODO
  case ND_NOT: return ir_binary(IR_EQ, gen_expr(e->lhs), ir_const(0));
  case ND_BITNOT: return ir_unary(IR_NOT, gen_expr(e->lhs));
  case ND_FUNCALL: {
    int nargs = 0;
    for (Node *a = e->args; a; a = a->next, nargs++)
      ;
    IRInstr *args[nargs];
    int ai = 0;
    for (Node *a = e->args; a; a = a->next, ai++)
      args[ai] = gen_expr(a);
    return ir_call(gen_expr(e->lhs), nargs, args);
  }
  }
  switch (e->kind) {
#define BIN(nd, ir)                                                            \
  case nd: return ir_binary(ir, gen_expr(e->lhs), gen_expr(e->rhs))
#define BINUS(nd, sir, uir)                                                    \
  case nd:                                                                     \
    return ir_binary(e->ty->is_unsigned ? uir : sir, gen_expr(e->lhs),         \
                     gen_expr(e->rhs))
#define BINLHSUS(nd, sir, uir)                                                 \
  case nd:                                                                     \
    return ir_binary(e->lhs->ty->is_unsigned ? uir : sir, gen_expr(e->lhs),    \
                     gen_expr(e->rhs))
    BIN(ND_ADD, IR_ADD);
    BIN(ND_SUB, IR_SUB);
    BIN(ND_MUL, IR_MUL);
    BINUS(ND_DIV, IR_SDIV, IR_UDIV);
    BINUS(ND_MOD, IR_SMOD, IR_UMOD);
    BIN(ND_BITAND, IR_AND);
    BIN(ND_BITOR, IR_OR);
    BIN(ND_BITXOR, IR_XOR);
    BIN(ND_EQ, IR_EQ);
    BIN(ND_NE, IR_NE);
    BINLHSUS(ND_LT, IR_SLT, IR_ULT);
    BINLHSUS(ND_LE, IR_SLE, IR_ULE);
    BIN(ND_SHL, IR_SLL);
    BINLHSUS(ND_SHR, IR_SRA, IR_SRL);
#undef BIN
#undef BINUS
#undef BINLHSUS
  }
  error_tok(e->tok, "invalid expression");
}

static void gen_condbr(Node *c, IRBlock *bt, IRBlock *bf) {
  switch (c->kind) {
  case ND_NOT: gen_condbr(c->lhs, bf, bt); return;
  case ND_LOGAND:
  case ND_LOGOR: {
    IRBlock *mid = new_block();
    if (c->kind == ND_LOGAND) {
      gen_condbr(c->lhs, mid, bf);
    } else {
      gen_condbr(c->lhs, bt, mid);
    }
    cur_block = mid;
    gen_condbr(c->rhs, bt, bf);
    return;
  }
  case ND_EQ:
  case ND_NE:
  case ND_LE:
  case ND_LT: add_instr(ir_branch(gen_expr(c), bt, bf)); return;
  default:
    add_instr(ir_branch(ir_binary(IR_NE, gen_expr(c), ir_const(0)), bt, bf));
    return;
  }
}

static void gen_stmt(Node *s) {
  switch (s->kind) {
  case ND_IF: {
    IRBlock *then = new_block();
    IRBlock *post = new_block();
    IRBlock *els = s->els ? new_block() : post;
    gen_condbr(s->cond, then, els);
    cur_block = then;
    gen_stmt(s->then);
    add_instr(ir_jump(post));
    cur_block = els;
    if (s->els) {
      gen_stmt(s->els);
      add_instr(ir_jump(post));
      cur_block = post;
    }
    return;
  }
  case ND_FOR: {
    if (s->init)
      gen_stmt(s->init);
    IRBlock *body = new_block();
    IRBlock *post = new_block();
    IRBlock *cond = body;
    if (s->cond) {
      cond = new_block();
      add_instr(ir_jump(cond));
      cur_block = cond;
      gen_condbr(s->cond, body, post);
    } else {
      add_instr(ir_jump(body));
    }
    cur_block = body;
    gen_stmt(s->then);
    if (s->inc)
      add_instr(gen_expr(s->inc));
    add_instr(ir_jump(cond));
    cur_block = post;
    return;
  }
  case ND_DO: {
    IRBlock *body = new_block();
    IRBlock *post = new_block();
    add_instr(ir_jump(body));
    cur_block = body;
    gen_stmt(s->then);
    gen_condbr(s->cond, body, post);
    cur_block = post;
    return;
  }
  case ND_SWITCH:
  case ND_CASE:
  case ND_GOTO:
  case ND_GOTO_EXPR:
  case ND_LABEL: return; // TODO
  case ND_BLOCK:
    for (Node *n = s->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_RETURN: add_instr(ir_ret(s->lhs ? gen_expr(s->lhs) : NULL)); return;
  case ND_EXPR_STMT: add_instr(gen_expr(s->lhs)); return;
  case ND_ASM: return; // TODO
  }
}

static IRLocal *new_irlocal(Obj *v) {
  if (v->irlocal)
    return v->irlocal;
  IRLocal *iv = calloc(1, sizeof *iv);
  iv->id = cur_fun->vctr++;
  iv->obj = v;
  v->irlocal = iv;
  return iv;
}

static IRFunction *gen_function(Obj *f) {
  if (f->irfun)
    return f->irfun;

  IRFunction *fun = calloc(1, sizeof *fun);
  fun->obj = f;
  f->irfun = fun;
  cur_fun = fun;

  IRLocal **local = &fun->locals;
  for (Obj *v = f->locals; v; v = v->next) {
    *local = new_irlocal(v);
    local = &(*local)->next;
  }

  fun->entry = new_block();
  cur_block = fun->entry;
  gen_stmt(f->body);
  add_instr(ir_ret(!strcmp(f->name, "main") ? ir_const(0) : NULL));
  return fun;
}

IRProgram *irgen(Obj *p) {
  IRProgram *prog = calloc(1, sizeof *prog);
  prog->obj = p;
  IRFunction **f = &prog->funs;
  for (Obj *o = p; o; o = o->next) {
    if (!o->is_definition)
      continue;
    if (o->is_function) {
      *f = gen_function(o);
      f = &(*f)->next;
    }
  }
  return prog;
}

void ir_clear_visited(IRValue *v) {
  if (!v->visited)
    return;
  v->visited = false;
  if (v->vt == IRV_INSTR) {
    IRInstr *i = (IRInstr *) v;
    for (int o = 0; o < i->numops; o++)
      ir_clear_visited(i->ops[o]);
  } else if (v->vt == IRV_BLOCK) {
    IRBlock *b = (IRBlock *) v;
    for (IRInstr *i = b->first; i; i = i->next) {
      ir_clear_visited((IRValue *) i);
    }
  }
}

const char *ir_opc_names[IR_MAX] = {
    "const", "globalptr", "localptr", "add",   "sub",   "mul",    "sdiv",
    "smod",  "udiv",      "umod",     "and",   "or",    "xor",    "sll",
    "srl",   "sra",       "neg",      "not",   "uext",  "sext",   "ubfe",
    "sbfe",  "bfi",       "eq",       "ne",    "slt",   "sle",    "ult",
    "ule",   "call",      "uload",    "sload", "store", "memcpy", "jp",
    "br",    "switch",    "ret",
};

#define P(fmt, ...) fprintf(out, fmt "\n" __VA_OPT__(, ) __VA_ARGS__)
#define I(fmt, ...) fprintf(out, fmt __VA_OPT__(, ) __VA_ARGS__)

void irprint_v(IRValue *v, FILE *out) {
  if (v->visited)
    return;
  v->visited = true;
  if (v->vt == IRV_INSTR) {
    IRInstr *i = (IRInstr *) v;
    for (int o = 0; o < i->numops; o++) {
      if (i->ops[o]->vt == IRV_INSTR)
        irprint_v(i->ops[o], out);
    }

    I("  ");
    if (i->opc < IR_STORE)
      I("%%%d = ", i->hdr.id);
    I("%s ", ir_opc_names[i->opc]);
    for (int o = 0; o < i->numops; o++) {
      I("%%%d ", i->ops[o]->id);
    }

    switch (i->opc) {
    case IR_CONST: I("%ld", i->cval); break;
    case IR_GLOBALPTR: I("%%%s", i->gvar->name); break;
    case IR_LOCALPTR: I("%%%d", i->lvar->id); break;
    case IR_UEXT:
    case IR_SEXT:
    case IR_ULOAD:
    case IR_SLOAD:
    case IR_STORE:
    case IR_MEMCPY: I("%d", i->size); break;
    }

    I("\n");

    for (int o = 0; o < i->numops; o++) {
      if (i->ops[o]->vt == IRV_BLOCK)
        irprint_v(i->ops[o], out);
    }
  } else if (v->vt == IRV_BLOCK) {
    IRBlock *b = (IRBlock *) v;
    P("B%d:", b->hdr.id);
    for (IRInstr *i = b->first; i; i = i->next) {
      irprint_v((IRValue *) i, out);
    }
  }
}

void irprint(IRProgram *p, FILE *out) {
  for (IRFunction *f = p->funs; f; f = f->next) {
    P("\nfunction%s %s", f->obj->is_static ? " static" : "", f->obj->name);
    for (IRLocal *v = f->locals; v; v = v->next) {
      P("local %%%d (%s) %d %d", v->id, v->obj->name, v->obj->ty->size,
        v->obj->align);
    }
    ir_clear_visited((IRValue *) f->entry);
    irprint_v((IRValue *) f->entry, out);
  }
}

#undef P
#undef I

void codegen(Obj *prog, FILE *out) {
  irprint(irgen(prog), out);
}