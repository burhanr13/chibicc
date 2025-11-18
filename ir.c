#include "ir.h"
#include "irpass.h"

IRBlock *ir_block(IRFunction *parent) {
  IRBlock *b = calloc(1, sizeof *b);
  b->hdr.vt = IRV_BLOCK;
  b->root.next = b->root.prev = &b->root;
  b->root.parent = b;
  b->parent = parent;
  return b;
}

void ir_add_instr(IRBlock *b, IRInstr *i) {
  ir_insert_instr(&b->root, i);
}

void ir_insert_instr(IRInstr *before, IRInstr *i) {
  assert(!i->parent && !i->prev && !i->next);
  i->next = before;
  i->prev = before->prev;
  i->next->prev = i;
  i->prev->next = i;
  i->parent = before->parent;
}

void ir_remove_instr(IRInstr *i) {
  if (!(i->prev && i->next))
    return;
  i->prev->next = i->next;
  i->next->prev = i->prev;
  i->prev = i->next = NULL;
  i->parent = NULL;
}

void ir_erase_instr(IRInstr *i) {
  assert(i->hdr.numuses == 0 && !i->hdr.uses);
  ir_remove_instr(i);
  for (int o = 0; o < i->numops; o++) {
    ir_remove_user(i->ops[o], i, o);
  }
  if (i->opc == IR_LOCALPTR)
    i->lvar->numuses--;
  free(i);
}

void ir_set_op(IRInstr *i, int op, IRValue *v) {
  if (i->ops[op] == v)
    return;
  assert(i != (IRInstr *) v);
  IRValue *old = i->ops[op];
  i->ops[op] = v;
  v->numuses++;
  IRUser *n = malloc(sizeof *n);
  n->next = v->uses;
  n->user = i;
  n->idx = op;
  v->uses = n;
  if (old)
    ir_remove_user(old, i, op);
}

void ir_remove_user(IRValue *v, IRInstr *user, int op) {
  if (user) {
    IRUser **u = &v->uses;
    while (*u && ((*u)->user != user || (*u)->idx != op))
      u = &(*u)->next;
    if (*u) {
      v->numuses--;
      IRUser *tmp = *u;
      *u = (*u)->next;
      free(tmp);
    }
  }
  if (!v->uses) {
    if (v->vt == IRV_INSTR) {
      IRInstr *i = (IRInstr *) v;
      if (!i->parent)
        ir_erase_instr((IRInstr *) v);
    } else if (v->vt == IRV_BLOCK) {
      IRBlock *b = (IRBlock *) v;
      assert(!b->is_entry);
      if (!b->is_exit) {
        while (!IRB_ISEMPTY(b))
          ir_erase_instr(IRB_FIRST(b));
        free(b);
      }
    }
  }
}

IRValue *ir_replace(IRValue *old, IRValue *new) {
  if (old == new)
    return new;
  assert(old->vt == new->vt);

  if (!old->uses) {
    assert(old->vt == IRV_INSTR && new->vt == IRV_INSTR &&
           ((IRInstr *) old)->prev && ((IRInstr *) old)->next);
    ir_insert_instr((IRInstr *) old, (IRInstr *) new);
    ir_erase_instr((IRInstr *) old);
    return new;
  }
  while (old->uses->next) {
    ir_set_op(old->uses->user, old->uses->idx, new);
  }
  ir_set_op(old->uses->user, old->uses->idx, new);
  return new;
}

void ir_merge_block(IRBlock *base, IRBlock *extra) {
  assert(IRB_LAST(base)->opc == IR_JP && IRB_LAST(base)->bops[0] == extra &&
         extra->hdr.numuses == 1 && extra->hdr.uses->user == IRB_LAST(base) &&
         base != extra);
  IRInstr *oldterm = IRB_LAST(base);
  while (!IRB_ISEMPTY(extra)) {
    IRInstr *i = IRB_FIRST(extra);
    ir_remove_instr(i);
    ir_add_instr(base, i);
  }
  ir_erase_instr(oldterm);
}

void ir_begin_pass(IRValue *v) {
  if (v->vt == IRV_INSTR) {
    v->visited = false;
    IRInstr *i = (IRInstr *) v;
    for (int o = 0; o < i->numops; o++) {
      ir_begin_pass(i->ops[o]);
    }
  } else if (v->vt == IRV_BLOCK) {
    if (!v->visited)
      return;
    v->visited = false;
    IRBlock *b = (IRBlock *) v;
    IRB_ITER(i, b) {
      ir_begin_pass((IRValue *) i);
    }
  }
}

BPASS_BEGIN(setup_rpo, IRBlock **res)
  if (b->is_exit)
    return;

  switch (IRB_LAST(b)->opc) {
  case IR_JP: PASS_REC(setup_rpo, IRB_LAST(b)->bops[0], res); break;
  case IR_BR:
    if (IRB_LAST(b)->bops[1]->is_post_loop) {
      PASS_REC(setup_rpo, IRB_LAST(b)->bops[1], res);
      PASS_REC(setup_rpo, IRB_LAST(b)->bops[2], res);
    } else {
      PASS_REC(setup_rpo, IRB_LAST(b)->bops[2], res);
      PASS_REC(setup_rpo, IRB_LAST(b)->bops[1], res);
    }
    break;
  }

  b->rpo_next = *res;
  *res = b;
BPASS_END(setup_rpo, &(IRBlock *) {f->exit})

IPASS_BEGIN(numbering, int *counter)
  IPASS_REC_ALL(numbering, counter);
  if (IROPC_HASRES(i->opc))
    i->hdr.id = (*counter)++;
  IPASS_BBEGIN(numbering, ({
                 int lcount = 0;
                 for (IRLocal *l = f->locals; l; l = l->next)
                   l->id = lcount++;
               });
               int counter = 0)
  b->hdr.id = counter++;
IPASS_END(numbering, &counter)

IPASS_BEGIN(calc_leaf, IRFunction *f)
  IPASS_REC_ALL(calc_leaf, f);
  if (i->opc == IR_CALL || i->opc == IR_MUL || i->opc == IR_UDIV ||
      i->opc == IR_SDIV)
    f->is_leaf = false;
  IPASS_BBEGIN(calc_leaf, f->is_leaf = true)
IPASS_END(calc_leaf, f)

const char *ir_opc_names[IR_MAX] = {
    "const", "globalptr", "localptr", "add",  "sub",  "mul",  "sdiv", "smod",
    "udiv",  "umod",      "and",      "or",   "xor",  "sll",  "srl",  "sra",
    "neg",   "not",       "uext",     "sext", "ubfe", "sbfe", "bfi",  "eq",
    "ne",    "slt",       "sle",      "ult",  "ule",  "call", "ret",  "load",
    "store", "memcpy",    "jp",       "br",
};

#define P(fmt, ...) fprintf(output_file, fmt "\n" __VA_OPT__(, ) __VA_ARGS__)
#define I(fmt, ...) fprintf(output_file, fmt __VA_OPT__(, ) __VA_ARGS__)

static FILE *output_file;

IPASS_BEGIN(print)
  IPASS_REC_ALL(print);

  I("  ");
  if (IROPC_HASRES(i->opc))
    I("%%%d = ", i->hdr.id);
  I("%s ", ir_opc_names[i->opc]);
  for (int o = 0; o < i->numops; o++) {
    I("%%%d ", i->ops[o]->id);
  }

  switch (i->opc) {
  case IR_CONST: I("%ld", i->cval); break;
  case IR_GLOBALPTR: I("$%s", i->gvar->name); break;
  case IR_LOCALPTR: I("$%d", i->lvar->id); break;
  case IR_UEXT:
  case IR_SEXT:
  case IR_LOAD:
  case IR_STORE:
  case IR_MEMCPY: I("%d", i->size); break;
  }
  I("\n");
  IPASS_BBEGIN(print)
  if (b->is_entry)
    P("B%d: ; entry", b->hdr.id);
  else if (b->is_exit)
    P("B%d: ; exit", b->hdr.id);
  else
    P("B%d:", b->hdr.id);
IPASS_END(print)

void irprint(IRProgram *p, FILE *out) {
  output_file = out;
  for (IRFunction *f = p->funs; f; f = f->next) {
    P("\nfunction%s %s", f->obj->is_static ? " static" : "", f->obj->name);
    for (IRLocal *v = f->locals; v; v = v->next) {
      P("local $%d (%s) %d %d ; %d uses", v->id, v->obj->name,
        v->obj->ty->size, v->obj->align, v->numuses);
    }
    irpass_print(f);
  }
}

#undef P
#undef I

void codegen(Obj *prog, FILE *out) {
  IRProgram *p = irgen(prog);

  IR_RUNPASS(irpass_opt_cfg, p);
  IR_RUNPASS(irpass_setup_rpo, p);

  IR_RUNPASS(irpass_constant_fold, p);

  IR_RUNPASS(irpass_opt_cfg, p);
  IR_RUNPASS(irpass_setup_rpo, p);
  //  TODO: optimize more

  IR_RUNPASS(irpass_numbering, p);
  IR_RUNPASS(irpass_calc_leaf, p);

  if (opt_emitir) {
    irprint(p, out);
  } else {
    ircodegen(p, out);
  }
}
