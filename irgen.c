#include "ir.h"

static IRFunction *cur_fun;
static IRBlock *cur_block;

static struct LabelNode {
  IRBlock *b;
  const char *label;
  struct LabelNode *next;
} *labels;

static struct GotoNode {
  IRInstr *i;
  const char *label;
  struct GotoNode *next;
} *gotos;

static struct SwitchCase {
  Node *sw;
  int curcase;
  IRBlock *dfl;
  struct {
    int begin, end;
    IRBlock *b;
  } cases[];
} *curswitch;

static void add_label(const char *label, IRBlock *b) {
  struct LabelNode *n = malloc(sizeof *n);
  n->b = b;
  n->label = label;
  n->next = labels;
  labels = n;
}

static void add_goto(const char *label, IRInstr *i) {
  assert(i->opc == IR_JP && i->bops[0] == NULL);
  struct GotoNode *n = malloc(sizeof *n);
  n->i = i;
  n->label = label;
  n->next = gotos;
  gotos = n;
}

static void resolve_gotos() {
  while (gotos) {
    for (struct LabelNode *l = labels; l; l = l->next) {
      if (!strcmp(gotos->label, l->label)) {
        ir_set_op(gotos->i, 0, (IRValue *) l->b);
        break;
      }
    }
    struct GotoNode *tmp = gotos;
    gotos = gotos->next;
    free(tmp);
  }
  while (labels) {
    struct LabelNode *tmp = labels;
    labels = labels->next;
    free(tmp);
  }
}

static IRBlock *new_block() {
  return ir_block(cur_fun);
}

static void add_instr(IRInstr *i) {
  ir_add_instr(cur_block, i);
}

IRInstr *ir_instr(int numops) {
  IRInstr *i = calloc(1, sizeof *i);
  i->hdr.vt = IRV_INSTR;
  i->numops = numops;
  i->ops = calloc(numops, sizeof(IRValue *));
  return i;
}

IRInstr *ir_const(uint64_t val) {
  IRInstr *i = ir_instr(0);
  i->opc = IR_CONST;
  i->cval = val;
  return i;
}

IRInstr *ir_unary(IROpc opc, IRInstr *lhs) {
  IRInstr *i = ir_instr(1);
  i->opc = opc;
  ir_set_op(i, 0, (IRValue *) lhs);
  return i;
}

IRInstr *ir_binary(IROpc opc, IRInstr *lhs, IRInstr *rhs) {
  IRInstr *i = ir_instr(2);
  i->opc = opc;
  ir_set_op(i, 0, (IRValue *) lhs);
  ir_set_op(i, 1, (IRValue *) rhs);
  return i;
}

IRInstr *ir_varptr(Obj *var) {
  IRInstr *i = ir_instr(0);
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

IRInstr *ir_load(Type *ty, IRInstr *addr) {
  switch (ty->kind) {
  case TY_ARRAY:
  case TY_FUNC:
  case TY_VLA: return addr;
  }
  if (ty->size > PTR_SIZE)
    return addr;
  IRInstr *i = ir_instr(1);
  i->opc = IR_LOAD;
  ir_set_op(i, 0, (IRValue *) addr);
  i->size = ty->size;
  i->is_volatile = ty->is_volatile;
  return i;
}

IRInstr *ir_store(Type *ty, IRInstr *addr, IRInstr *data) {
  IRInstr *i = ir_instr(2);
  if (ty->size > PTR_SIZE)
    i->opc = IR_MEMCPY;
  else
    i->opc = IR_STORE;
  ir_set_op(i, 0, (IRValue *) addr);
  ir_set_op(i, 1, (IRValue *) data);
  i->size = ty->size;
  i->is_volatile = ty->is_volatile;
  return i;
}

IRInstr *ir_extend(IROpc opc, IRInstr *src, int size) {
  IRInstr *i = ir_instr(1);
  i->opc = opc;
  i->size = size;
  ir_set_op(i, 0, (IRValue *) src);
  return i;
}

IRInstr *ir_call(IRInstr *f, int nargs, IRInstr **args) {
  IRInstr *i = ir_instr(1 + nargs);
  i->opc = IR_CALL;
  ir_set_op(i, 0, (IRValue *) f);
  for (int a = 0; a < nargs; a++) {
    ir_set_op(i, 1 + a, (IRValue *) args[a]);
  }
  return i;
}

IRInstr *ir_ret(IRInstr *lhs) {
  IRInstr *i = ir_instr(1);
  i->opc = IR_RET;
  ir_set_op(i, 0, (IRValue *) lhs);
  return i;
}

IRInstr *ir_branch(IRInstr *cond, IRBlock *bt, IRBlock *bf) {
  IRInstr *i = ir_instr(3);
  i->opc = IR_BR;
  ir_set_op(i, 0, (IRValue *) cond);
  ir_set_op(i, 1, (IRValue *) bt);
  ir_set_op(i, 2, (IRValue *) bf);
  return i;
}

IRInstr *ir_jump(IRBlock *dst) {
  IRInstr *i = ir_instr(1);
  i->opc = IR_JP;
  ir_set_op(i, 0, (IRValue *) dst);
  return i;
}

IRInstr *ir_switch(IRInstr *cond, IRBlock *dfl, int ncase, IRBlock **cases) {
  IRInstr *i = ir_instr(2 + ncase);
  i->opc = IR_SWITCH;
  ir_set_op(i, 0, (IRValue *) cond);
  ir_set_op(i, 1, (IRValue *) dfl);
  // jumps to case n if cond == n, so cases are always contiguous and start at 0
  // so dont need to store compare value for each case
  for (int c = 0; c < ncase; c++) {
    ir_set_op(i, 2 + c, (IRValue *) cases[c]);
  }
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

static IRInstr *gen_expr(Node *e) {
  switch (e->kind) {
  case ND_NULL_EXPR: return ir_const(0);
  case ND_NUM: return ir_const(e->val);
  case ND_NEG: return ir_unary(IR_NEG, gen_expr(e->lhs));
  case ND_VAR: return ir_load(e->var->ty, gen_addr(e));
  case ND_MEMBER: {
    IRInstr *addr = gen_addr(e);
    IRInstr *res = ir_load(e->ty, addr);
    if (e->member->is_bitfield) {
      res = ir_extend(e->member->ty->is_unsigned ? IR_UBEXT : IR_SEXT,
                      ir_binary(IR_SRL, res, ir_const(e->member->bit_offset)),
                      e->member->bit_width);
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
      uint64_t mask = -1;
      mask >>= 64 - e->lhs->member->bit_width;
      mask <<= e->lhs->member->bit_offset;
      stval = ir_binary(
          IR_OR, ir_binary(IR_AND, dst, ir_const(~mask)),
          ir_binary(IR_SLL, ir_extend(IR_UBEXT, val, e->lhs->member->bit_width),
                    ir_const(e->lhs->member->bit_offset)));
    }
    add_instr(ir_store(e->ty, addr, stval));
    return val;
  }
  case ND_STMT_EXPR: printf("stubbed: stmt expr\n"); return ir_const(0); // TODO
  case ND_COMMA: add_instr(gen_expr(e->lhs)); return gen_expr(e->rhs);
  case ND_CAST: {
    int effectivesize = e->lhs->ty->size;
    switch (e->lhs->ty->kind) {
    case TY_FUNC:
    case TY_ARRAY:
    case TY_VLA: effectivesize = PTR_SIZE;
    }
    if (effectivesize >= e->ty->size)
      return gen_expr(e->lhs);
    return ir_extend(e->lhs->ty->is_unsigned ? IR_UEXT : IR_SEXT,
                     gen_expr(e->lhs), effectivesize);
  }
  case ND_MEMZERO: printf("stubbed: memzero\n"); return ir_const(0); // TODO
  case ND_LOGAND: {
    IRBlock *bt = new_block();
    IRBlock *post = new_block();
    add_instr(ir_store(ty_int, ir_varptr(e->var), ir_const(0)));
    gen_condbr(e->lhs, bt, post);

    cur_block = bt;
    add_instr(ir_store(ty_int, ir_varptr(e->var),
                       ir_binary(IR_NE, gen_expr(e->rhs), ir_const(0))));
    add_instr(ir_jump(post));

    cur_block = post;
    return ir_load(ty_int, ir_varptr(e->var));
  }
  case ND_LOGOR: {
    IRBlock *bf = new_block();
    IRBlock *post = new_block();
    add_instr(ir_store(ty_int, ir_varptr(e->var), ir_const(1)));
    gen_condbr(e->lhs, post, bf);

    cur_block = bf;
    add_instr(ir_store(ty_int, ir_varptr(e->var),
                       ir_binary(IR_NE, gen_expr(e->rhs), ir_const(0))));
    add_instr(ir_jump(post));

    cur_block = post;
    return ir_load(ty_int, ir_varptr(e->var));
  }
  case ND_COND: {
    IRBlock *bt = new_block();
    IRBlock *bf = new_block();
    IRBlock *post = new_block();
    gen_condbr(e->cond, bt, bf);

    cur_block = bt;
    add_instr(ir_store(e->ty, ir_varptr(e->var), gen_expr(e->then)));
    add_instr(ir_jump(post));

    cur_block = bf;
    add_instr(ir_store(e->ty, ir_varptr(e->var), gen_expr(e->els)));
    add_instr(ir_jump(post));

    cur_block = post;
    return ir_load(e->ty, ir_varptr(e->var));
  }
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

static int compare_cases(typeof(*curswitch->cases) *a,
                         typeof(*curswitch->cases) *b) {
  return a->begin - b->begin;
}

static void gen_switch(IRInstr *cond, int starti, int endi) {
  int begin = curswitch->cases[starti].begin;
  int end = curswitch->cases[endi].end;
  int ncase = end + 1 - begin;
  IRBlock *cases[ncase];
  for (int i = starti; i <= endi; i++) {
    typeof(*curswitch->cases) *c = &curswitch->cases[i];
    if (i > starti) {
      for (int j = c[-1].end + 1; j < c->begin; j++) {
        cases[j - begin] = curswitch->dfl;
      }
    }
    for (int j = c->begin; j <= c->end; j++) {
      cases[j - begin] = c->b;
    }
  }

  IRBlock *nextblock = new_block();
  add_instr(ir_switch(ir_binary(IR_SUB, cond, ir_const(begin)), nextblock,
                      ncase, cases));

  cur_block = nextblock;
  return;
}

static void gen_stmt(Node *s) {
  switch (s->kind) {
  case ND_IF: {
    IRBlock *then = new_block();
    IRBlock *els = new_block();
    IRBlock *post = new_block();
    gen_condbr(s->cond, then, els);

    cur_block = then;
    gen_stmt(s->then);
    add_instr(ir_jump(post));

    cur_block = els;
    if (s->els)
      gen_stmt(s->els);
    add_instr(ir_jump(post));

    cur_block = post;
    return;
  }
  case ND_FOR: {
    if (s->init)
      gen_stmt(s->init);
    IRBlock *cond = new_block();
    IRBlock *body = new_block();
    IRBlock *cont = new_block();
    IRBlock *post = new_block();
    add_instr(ir_jump(cond));

    cur_block = cond;
    if (s->cond) {
      gen_condbr(s->cond, body, post);
    } else {
      add_instr(ir_jump(body));
    }

    cur_block = body;
    gen_stmt(s->then);
    add_instr(ir_jump(cont));

    cur_block = cont;
    add_label(s->cont_label, cont);
    if (s->inc)
      add_instr(gen_expr(s->inc));
    add_instr(ir_jump(cond));

    cur_block = post;
    add_label(s->brk_label, post);
    return;
  }
  case ND_DO: {
    IRBlock *body = new_block();
    IRBlock *cond = new_block();
    IRBlock *post = new_block();
    add_instr(ir_jump(body));

    cur_block = body;
    gen_stmt(s->then);
    add_instr(ir_jump(cond));

    cur_block = cond;
    add_label(s->cont_label, cond);
    gen_condbr(s->cond, body, post);

    cur_block = post;
    add_label(s->brk_label, post);
    return;
  }
  case ND_SWITCH: {
    IRBlock *body = new_block();
    IRBlock *post = new_block();
    IRBlock *start = cur_block;

    int ncase = 0;
    for (Node *c = s->case_next; c; c = c->case_next, ncase++)
      ;
    struct SwitchCase *old = curswitch;
    curswitch = alloca(sizeof *curswitch + ncase * sizeof *curswitch->cases);
    curswitch->sw = s;
    curswitch->curcase = 0;
    curswitch->dfl = post;

    cur_block = body;
    gen_stmt(s->then);
    add_instr(ir_jump(post));

    cur_block = start;
    IRInstr *e = gen_expr(s->cond);

    qsort(curswitch->cases, ncase, sizeof *curswitch->cases,
          (void*) compare_cases);

    int rangestart = 0;
    for (int ci = 0; ci < ncase; ci++) {
      typeof(*curswitch->cases) *c = &curswitch->cases[ci];
      if (c->end < c->begin)
        continue;
      if (ci > rangestart && c->begin - (c[-1].end + 1) > 64) {
        gen_switch(e, rangestart, ci - 1);
        rangestart = ci;
      }
      if (c->end + 1 - c->begin > 64) {
        if (ci > rangestart)
          gen_switch(e, rangestart, ci - 1);
        IRBlock *b = new_block();
        add_instr(ir_branch(ir_binary(IR_ULE,
                                      ir_binary(IR_SUB, e, ir_const(c->begin)),
                                      ir_const(c->end - c->begin)),
                            c->b, b));
        cur_block = b;
        rangestart = ci + 1;
      }
    }

    if (rangestart < ncase)
      gen_switch(e, rangestart, ncase - 1);

    add_instr(ir_jump(curswitch->dfl));

    curswitch = old;
    cur_block = post;
    add_label(s->brk_label, post);
    ir_erase_block(body);
    return;
  }
  case ND_CASE: {
    IRBlock *nextblock = new_block();
    add_instr(ir_jump(nextblock));
    cur_block = nextblock;
    gen_stmt(s->lhs);

    if (s == curswitch->sw->default_case)
      curswitch->dfl = nextblock;
    else {
      curswitch->cases[curswitch->curcase].begin = s->begin;
      curswitch->cases[curswitch->curcase].end = s->end;
      curswitch->cases[curswitch->curcase].b = nextblock;
      curswitch->curcase++;
    }
    return;
  }
  case ND_GOTO: {
    IRInstr *gotoinst = ir_jump(NULL);
    add_goto(s->unique_label, gotoinst);
    add_instr(gotoinst);
    return;
  }
  case ND_LABEL: {
    IRBlock *nextblock = new_block();
    add_instr(ir_jump(nextblock));
    cur_block = nextblock;
    add_label(s->unique_label, nextblock);
    gen_stmt(s->lhs);
    return;
  }
  case ND_GOTO_EXPR: printf("stubbed: goto expr\n"); return; // TODO
  case ND_BLOCK:
    for (Node *n = s->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_RETURN:
    if (s->lhs)
      add_instr(ir_ret(gen_expr(s->lhs)));
    add_instr(ir_jump(cur_fun->exit));
    return;
  case ND_EXPR_STMT: add_instr(gen_expr(s->lhs)); return;
  case ND_ASM: printf("stubbed: asm\n"); return; // TODO
  }
}

static IRLocal *new_irlocal(Obj *v) {
  if (v->irlocal)
    return v->irlocal;
  IRLocal *iv = calloc(1, sizeof *iv);
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

  resolve_gotos();

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
