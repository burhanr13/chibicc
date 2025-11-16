#include "ir.h"

static IRFunction *cur_fun;
static IRBlock *cur_block;

static IRBlock *new_block() {
  IRBlock *b = calloc(1, sizeof *b);
  b->hdr.vt = IRV_BLOCK;
  b->hdr.id = cur_fun->vctr++;
  b->root.next = b->root.prev = &b->root;
  return b;
}

static void add_instr(IRInstr *i) {
  ir_add_instr(cur_block, i);
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
  ir_set_op(i, 0, (IRValue *) lhs);
  return i;
}

static IRInstr *ir_binary(IROpc opc, IRInstr *lhs, IRInstr *rhs) {
  IRInstr *i = new_instr(2);
  i->opc = opc;
  ir_set_op(i, 0, (IRValue *) lhs);
  ir_set_op(i, 1, (IRValue *) rhs);
  return i;
}

static IRInstr *ir_varptr(Obj *var) {
  IRInstr *i = new_instr(0);
  if (var->is_local) {
    i->opc = IR_LOCALPTR;
    i->lvar = var->irlocal;
    var->irlocal->numuses++;
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
  i->opc = IR_LOAD;
  ir_set_op(i, 0, (IRValue *) addr);
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
  ir_set_op(i, 0, (IRValue *) addr);
  ir_set_op(i, 1, (IRValue *) data);
  i->size = ty->size;
  return i;
}

static IRInstr *ir_bitfield(IROpc opc, IRInstr *src, IRInstr *dst, int start,
                            int len) {
  IRInstr *i = new_instr(opc == IR_BFI ? 4 : 3);
  i->opc = opc;
  ir_set_op(i, 0, (IRValue *) src);
  if (opc == IR_BFI) {
    ir_set_op(i, 1, (IRValue *) dst);
    ir_set_op(i, 2, (IRValue *) ir_const(start));
    ir_set_op(i, 3, (IRValue *) ir_const(len));
  } else {
    ir_set_op(i, 1, (IRValue *) ir_const(start));
    ir_set_op(i, 2, (IRValue *) ir_const(len));
  }
  return i;
}

static IRInstr *ir_cast(IRInstr *src, Type *from, Type *to) {
  if (from->size >= to->size || from->kind == TY_ARRAY)
    return src;
  IRInstr *i = new_instr(1);
  i->opc = from->is_unsigned ? IR_UEXT : IR_SEXT;
  if (from->size == 3) {
    printf("wtf\n");
  }
  i->size = from->size;
  ir_set_op(i, 0, (IRValue *) src);
  return i;
}

static IRInstr *ir_call(IRInstr *f, int nargs, IRInstr **args) {
  IRInstr *i = new_instr(1 + nargs);
  i->opc = IR_CALL;
  ir_set_op(i, 0, (IRValue *) f);
  for (int a = 0; a < nargs; a++) {
    ir_set_op(i, 1 + a, (IRValue *) args[a]);
  }
  return i;
}

static IRInstr *ir_ret(IRInstr *lhs) {
  IRInstr *i = new_instr(1);
  i->opc = IR_RET;
  ir_set_op(i, 0, (IRValue *) lhs);
  return i;
}

static IRInstr *ir_branch(IRInstr *cond, IRBlock *bt, IRBlock *bf) {
  IRInstr *i = new_instr(3);
  i->opc = IR_BR;
  ir_set_op(i, 0, (IRValue *) cond);
  ir_set_op(i, 1, (IRValue *) bt);
  ir_set_op(i, 2, (IRValue *) bf);
  return i;
}

static IRInstr *ir_jump(IRBlock *dst) {
  IRInstr *i = new_instr(1);
  i->opc = IR_JP;
  ir_set_op(i, 0, (IRValue *) dst);
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
  case ND_VLA_PTR:
    printf("stubbed: addr of comma/funcall/assign/cond/vla_ptr\n");
    return ir_const(0); // TODO
  }

  error_tok(e->tok, "not an lvalue");
}

static IRInstr *gen_expr(Node *e) {
  switch (e->kind) {
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
  case ND_STMT_EXPR: printf("stubbed: stmt expr\n"); return ir_const(0); // TODO
  case ND_COMMA:
    if (e->lhs->kind != ND_NULL_EXPR)
      add_instr(gen_expr(e->lhs));
    return gen_expr(e->rhs);
  case ND_CAST: return ir_cast(gen_expr(e->lhs), e->lhs->ty, e->ty);
  case ND_MEMZERO: printf("stubbed: memzero\n"); return ir_const(0); // TODO
  case ND_LOGAND:
  case ND_LOGOR:
  case ND_COND: printf("stubbed: land/lor/cond\n"); return ir_const(0); // TODO
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
  case ND_LABEL: printf("stubbed: switch/case/goto/label\n"); return; // TODO
  case ND_BLOCK:
    for (Node *n = s->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_RETURN:
    if (s->lhs)
      add_instr(ir_ret(gen_expr(s->lhs)));
    add_instr(ir_jump(cur_fun->exit));
    return;
  case ND_EXPR_STMT:
    if (s->lhs->kind != ND_NULL_EXPR)
      add_instr(gen_expr(s->lhs));
    return;
  case ND_ASM: printf("stubbed: asm\n"); return; // TODO
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
  if (f->params)
    fun->params = f->params->irlocal;
  for (IRLocal *p = fun->params; p; p = p->next) {
    p->is_param = true;
  }

  fun->entry = new_block();
  fun->entry->is_entry = true;
  fun->exit = new_block();
  fun->exit->is_exit = true;
  cur_block = fun->entry;
  gen_stmt(f->body);
  if (!strcmp(f->name, "main"))
    add_instr(ir_ret(ir_const(0)));
  add_instr(ir_jump(fun->exit));
  return fun;
}

IRProgram *irgen(Obj *p) {
  IRProgram *prog = calloc(1, sizeof *prog);
  prog->obj = p;
  IRFunction **f = &prog->funs;
  for (Obj *o = p; o; o = o->next) {
    if (!o->is_function || !o->is_definition || !o->is_live)
      continue;
    *f = gen_function(o);
    f = &(*f)->next;
  }
  return prog;
}
