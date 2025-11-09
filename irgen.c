#include "ir.h"

static int ctr = 0;

static IRBlock *cur_block;

static IRBlock *new_block() {
  IRBlock *b = calloc(1, sizeof *b);
  b->id = ctr++;
  return b;
}

static void add_inst(IRInst *i) {
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
  switch (i->opc) {
  case IR_BR:
  case IR_JP:
  case IR_RET: cur_block->terminated = true;
  }
}

static IRInst *new_inst() {
  IRInst *i = calloc(1, sizeof *i);
  i->id = ctr++;
  return i;
}

static IRInst *ir_const(uint64_t val) {
  IRInst *i = new_inst();
  i->opc = IR_CONST;
  i->val = val;
  return i;
}

static IRInst *ir_unary(IROpc opc, IRInst *lhs) {
  IRInst *i = new_inst();
  i->opc = opc;
  i->ops[0] = lhs;
  return i;
}

static IRInst *ir_binary(IROpc opc, IRInst *lhs, IRInst *rhs) {
  IRInst *i = new_inst();
  i->opc = opc;
  i->ops[0] = lhs;
  i->ops[1] = rhs;
  return i;
}

static IRInst *ir_varptr(Obj *var) {
  IRInst *i = new_inst();
  if (var->is_local) {
    i->opc = IR_LOCALPTR;
    i->lvar = var->irlocal;
  } else {
    i->opc = IR_GLOBALPTR;
    i->gvar = var;
  }
  return i;
}

static IRInst *ir_load(Type *ty, IRInst *addr) {
  switch (ty->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION:
  case TY_FUNC:
  case TY_VLA: return addr;
  }
  IRInst *i = new_inst();
  i->opc = ty->is_unsigned ? IR_ULOAD : IR_SLOAD;
  i->ops[0] = addr;
  i->mem_size = ty->size;
  return i;
}

static IRInst *ir_store(Type *ty, IRInst *addr, IRInst *data) {
  IRInst *i = new_inst();
  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION: i->opc = IR_MEMCPY; break;
  default: i->opc = IR_STORE; break;
  }
  i->ops[0] = addr;
  i->ops[1] = data;
  i->mem_size = ty->size;
  return i;
}

static IRInst *ir_bitfield(IROpc opc, IRInst *src, IRInst *dst, int start,
                           int len) {
  IRInst *i = new_inst();
  i->opc = opc;
  i->ops[0] = src;
  i->ops[1] = dst;
  i->bf_start = start;
  i->bf_len = len;
  return i;
}

static IRInst *ir_cast(IRInst *src, Type *from, Type *to) {
  if (from->size >= to->size)
    return src;
  return ir_bitfield(from->is_unsigned ? IR_ZEXT : IR_SEXT, src, NULL, 0,
                     from->size * 8);
}

static IRInst *ir_branch(IRInst *cond, IRBlock *bt, IRBlock *bf) {
  IRInst *i = new_inst();
  i->opc = IR_BR;
  i->ops[0] = cond;
  i->blocks[0] = bt;
  i->blocks[1] = bf;
  return i;
}

static IRInst *ir_jump(IRBlock *dst) {
  IRInst *i = new_inst();
  i->opc = IR_JP;
  i->blocks[0] = dst;
  return i;
}

static IRInst *gen_expr(Node *e);

static IRInst *gen_addr(Node *e) {
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

static IRInst *gen_expr(Node *e) {
  switch (e->kind) {
  case ND_NUM: return ir_const(e->val);
  case ND_NEG: return ir_unary(IR_NEG, gen_expr(e->lhs));
  case ND_VAR: return ir_load(e->ty, gen_addr(e));
  case ND_MEMBER: {
    IRInst *addr = gen_addr(e);
    IRInst *res = ir_load(e->ty, addr);
    if (e->member->is_bitfield) {
      // TODO: bitfield
      res = ir_bitfield(e->member->ty->is_unsigned ? IR_UBFE : IR_SBFE, res,
                        NULL, e->member->bit_offset, e->member->bit_width);
    }
    return res;
  }
  case ND_DEREF: return ir_load(e->ty, gen_expr(e->lhs));
  case ND_ADDR: return gen_addr(e->lhs);
  case ND_ASSIGN: {
    IRInst *val = gen_expr(e->rhs);
    IRInst *addr = gen_addr(e->lhs);
    if (e->lhs->kind == ND_MEMBER && e->lhs->member->is_bitfield) {
      IRInst *dst = ir_load(e->lhs->member->ty, addr);
      val = ir_bitfield(IR_BFI, val, dst, e->lhs->member->bit_offset,
                        e->lhs->member->bit_width);
    }
    return ir_store(e->ty, addr, val);
  }
  case ND_STMT_EXPR:
  case ND_COMMA: return ir_const(0); // TODO
  case ND_CAST: return ir_cast(gen_expr(e->lhs), e->lhs->ty, e->ty);
  case ND_MEMZERO: return ir_const(0); // TODO
  case ND_LOGAND:
  case ND_LOGOR:
  case ND_COND: return ir_const(0); // TODO
  case ND_NOT: return ir_binary(IR_EQ, gen_expr(e->lhs), ir_const(0));
  case ND_BITNOT: return ir_unary(IR_NOT, gen_expr(e->lhs));
  case ND_FUNCALL: {
    IRInst *args = NULL;
    IRInst **lastarg = &args;
    for (Node *a = e->args; a; a = a->next) {
      *lastarg = gen_expr(a);
      lastarg = &(*lastarg)->nextarg;
    }
    return ir_binary(IR_CALL, gen_expr(e->lhs), args);
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
  case ND_LT: add_inst(ir_branch(gen_expr(c), bt, bf)); return;
  default:
    add_inst(ir_branch(ir_binary(IR_NE, gen_expr(c), ir_const(0)), bt, bf));
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
    add_inst(ir_jump(post));
    cur_block = els;
    if (s->els) {
      gen_stmt(s->els);
      add_inst(ir_jump(post));
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
      add_inst(ir_jump(cond));
      cur_block = cond;
      gen_condbr(s->cond, body, post);
    } else {
      add_inst(ir_jump(body));
    }
    cur_block = body;
    gen_stmt(s->then);
    if (s->inc)
      add_inst(gen_expr(s->inc));
    add_inst(ir_jump(cond));
    cur_block = post;
    return;
  }
  case ND_DO: {
    IRBlock *body = new_block();
    IRBlock *post = new_block();
    add_inst(ir_jump(body));
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
  case ND_RETURN:
    add_inst(ir_unary(IR_RET, s->lhs ? gen_expr(s->lhs) : NULL));
    return;
  case ND_EXPR_STMT: add_inst(gen_expr(s->lhs)); return;
  case ND_ASM: return; // TODO
  }
}

static IRLocal *new_irlocal(Obj *v) {
  if (v->irlocal)
    return v->irlocal;
  IRLocal *iv = calloc(1, sizeof *iv);
  iv->id = ctr++;
  iv->obj = v;
  v->irlocal = iv;
  return iv;
}

static IRFunction *gen_function(Obj *f) {
  if (f->irfun)
    return f->irfun;

  ctr = 0;

  IRFunction *fun = calloc(1, sizeof *fun);
  fun->obj = f;
  f->irfun = fun;

  IRLocal **local = &fun->locals;
  for (Obj *v = f->locals; v; v = v->next) {
    *local = new_irlocal(v);
    local = &(*local)->next;
  }

  fun->entry = new_block();
  cur_block = fun->entry;
  gen_stmt(f->body);
  add_inst(ir_unary(IR_RET, !strcmp(f->name, "main") ? ir_const(0) : NULL));
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

void ir_clear_visited_block(IRBlock *b);

void ir_clear_visited_inst(IRInst *i) {
  if (!i->visited)
    return;
  i->visited = false;
  if (i->ops[0])
    ir_clear_visited_inst(i->ops[0]);
  if (i->ops[1])
    ir_clear_visited_inst(i->ops[1]);
  if (i->blocks[0])
    ir_clear_visited_block(i->blocks[0]);
  if (i->blocks[1])
    ir_clear_visited_block(i->blocks[1]);

  if (i->nextarg)
    ir_clear_visited_inst(i->nextarg);
}

void ir_clear_visited_block(IRBlock *b) {
  if (!b->visited)
    return;
  b->visited = false;
  for (IRInst *i = b->first; i; i = i->next) {
    ir_clear_visited_inst(i);
  }
}

const char *ir_opc_names[IR_MAX] = {
    "const", "add",    "sub",  "mul",       "sdiv",     "smod",  "udiv",
    "umod",  "and",    "or",   "xor",       "sll",      "srl",   "sra",
    "neg",   "not",    "zext", "sext",      "ubfe",     "sbfe",  "bfi",
    "eq",    "ne",     "slt",  "sle",       "ult",      "ule",   "jp",
    "br",    "call",   "ret",  "globalptr", "localptr", "uload", "sload",
    "store", "memcpy",
};

#define P(fmt, ...) fprintf(out, fmt "\n" __VA_OPT__(, ) __VA_ARGS__)
#define I(fmt, ...) fprintf(out, fmt __VA_OPT__(, ) __VA_ARGS__)

void irprint_block(IRBlock *b, FILE *out);

void irprint_inst(IRInst *i, FILE *out) {
  if (i->visited)
    return;
  i->visited = true;
  if (i->ops[0])
    irprint_inst(i->ops[0], out);
  if (i->ops[1])
    irprint_inst(i->ops[1], out);

  I("  ");
  switch (i->opc) {
  case IR_STORE:
  case IR_MEMCPY:
  case IR_BR:
  case IR_JP:
  case IR_RET: break;
  default: I("%%%d = ", i->id);
  }
  I("%s ", ir_opc_names[i->opc]);
  if (i->ops[0])
    I("%%%d ", i->ops[0]->id);
  if (i->ops[1]) {
    for (IRInst *a = i->ops[1]; a; a = a->nextarg)
      I("%%%d ", a->id);
  }
  if (i->blocks[0])
    I("%%B%d ", i->blocks[0]->id);
  if (i->blocks[1])
    I("%%B%d ", i->blocks[1]->id);

  if (i->lvar)
    I("%%%d", i->lvar->id);
  if (i->gvar)
    I("%%%s", i->gvar->name);

  switch (i->opc) {
  case IR_CONST: I("%ld", i->val); break;
  case IR_ULOAD:
  case IR_SLOAD:
  case IR_STORE:
  case IR_MEMCPY: I("%d", i->mem_size); break;
  case IR_ZEXT:
  case IR_SEXT: I("%d", i->bf_len); break;
  case IR_UBFE:
  case IR_SBFE:
  case IR_BFI: I("%d %d", i->bf_start, i->bf_len); break;
  }

  I("\n");

  if (i->blocks[0])
    irprint_block(i->blocks[0], out);
  if (i->blocks[1])
    irprint_block(i->blocks[1], out);

  if (i->nextarg)
    irprint_inst(i->nextarg, out);
}

void irprint_block(IRBlock *b, FILE *out) {
  if (b->visited)
    return;
  b->visited = true;
  P("B%d:", b->id);
  for (IRInst *i = b->first; i; i = i->next) {
    irprint_inst(i, out);
  }
}

void irprint(IRProgram *p, FILE *out) {
  for (IRFunction *f = p->funs; f; f = f->next) {
    P("\nfunction%s %s", f->obj->is_static ? " static" : "", f->obj->name);
    for (IRLocal *v = f->locals; v; v = v->next) {
      P("local %%%d (%s) %d %d", v->id, v->obj->name, v->obj->ty->size,
        v->obj->align);
    }
    ir_clear_visited_block(f->entry);
    irprint_block(f->entry, out);
  }
}

#undef P
#undef I

void codegen(Obj *prog, FILE *out) {
  irprint(irgen(prog), out);
}