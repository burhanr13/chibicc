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
  IRI_ITER(it, i) {
    ir_remove_user(it.v, i, it.idx);
  }
  if (i->opc == IR_LOCALPTR)
    i->lvar->numuses--;
  free(i);
}

void ir_erase_block(IRBlock *b) {
  assert(b->hdr.numuses == 0 && !b->hdr.uses && !b->is_entry && !b->is_exit);
  while (!IRB_ISEMPTY(b))
    ir_erase_instr(IRB_FIRST(b));
  free(b);
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
        ir_erase_instr(i);
    } else if (v->vt == IRV_BLOCK) {
      IRBlock *b = (IRBlock *) v;
      if (!b->is_exit)
        ir_erase_block(b);
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

void ir_reset_marks(IRValue *v) {
  if (v->vt == IRV_INSTR) {
    v->mark = false;
    IRI_ITER(it, (IRInstr *) v) ir_reset_marks(it.v);
  } else if (v->vt == IRV_BLOCK) {
    if (!v->mark)
      return;
    v->mark = false;
    IRB_ITER(i, (IRBlock *) v) {
      ir_reset_marks((IRValue *) i);
    }
  }
}

BPASS_BEGIN(setup_rpo, IRBlock **res)
  if (b->is_exit)
    return;

  IRB_SUCCS_R(it, b) {
    if (b->post_loop)
      PASS_REC(setup_rpo, it.b, res);
  }
  IRB_SUCCS_R(it, b) {
    if (!b->post_loop)
      PASS_REC(setup_rpo, it.b, res);
  }

  b->rpo_next = *res;
  *res = b;
BPASS_END(setup_rpo, &(IRBlock *) {f->exit})

static void loops_backwards(IRBlock *b) {
  if (b->hdr.mark)
    return;
  b->hdr.mark = true;
  IRB_PREDS(it, b) loops_backwards(it.b);
}

static void loops_forwards(IRBlock *b, IRBlock *start) {
  if (!b->hdr.mark) {
    if (b != start)
      b->post_loop = true;
    return;
  }
  b->hdr.mark = false;
  IRB_SUCCS(it, b) loops_forwards(it.b, start);
}

void irpass_calc_loops(IRFunction *f) {
  for (IRBlock *b = f->entry; b; b = b->rpo_next) {
    b->hdr.mark = true;

    IRB_PREDS(it, b) {
      if (!it.b->hdr.mark) {
        it.u->loop = true;
        b->start_loop = true;
        loops_backwards(it.b);
        loops_forwards(b, b);
        b->hdr.mark = true;
      }
    }
  }
  for (IRBlock *b = f->entry; b; b = b->rpo_next)
    b->hdr.mark = false;
}

void irpass_calc_idoms(IRFunction *f) {
  for (IRBlock *b = f->entry; b; b = b->rpo_next) {
    if (b == f->entry) {
      b->idom = NULL;
      continue;
    }
    if (b->hdr.numuses == 1) {
      b->idom = b->hdr.uses->user->parent;
      continue;
    }

    int nonlooppreds = 0;
    IRB_PREDS(it, b) {
      if (it.u->loop)
        continue;
      nonlooppreds++;
      for (IRBlock *b = it.b; b; b = b->idom) {
        b->tmp++;
      }
    }

    for (IRBlock *b1 = f->entry; b1 != b; b1 = b1->rpo_next) {
      if (b1->tmp == nonlooppreds)
        b->idom = b1;
      b1->tmp = 0;
    }
  }
}

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

IPASS_BEGIN(final, IRFunction *f)
  IPASS_REC_ALL(final, f);
  if (i->opc == IR_CALL || i->opc == IR_MUL || i->opc == IR_UDIV ||
      i->opc == IR_SDIV)
    f->is_leaf = false;
  IPASS_BBEGIN(final, f->is_leaf = true)
IPASS_END(final, f)

const char *ir_opc_names[IR_MAX] = {
    "const", "globalptr", "localptr", "add",   "sub",  "mul",
    "sdiv",  "smod",      "udiv",     "umod",  "and",  "or",
    "xor",   "sll",       "srl",      "sra",   "neg",  "not",
    "uext",  "sext",      "ubext",    "sbext", "eq",   "ne",
    "slt",   "sle",       "ult",      "ule",   "call", "ret",
    "load",  "store",     "memcpy",   "jp",    "br",   "switch",
};

#define P(fmt, ...) fprintf(output_file, fmt __VA_OPT__(, ) __VA_ARGS__)
#define L(fmt, ...) P(fmt "\n", __VA_ARGS__)

static FILE *output_file;

IPASS_BEGIN(print)
  IPASS_REC_ALL(print);

  P("  ");
  if (IROPC_HASRES(i->opc))
    P("%%%d = ", i->hdr.id);
  P("%s ", ir_opc_names[i->opc]);
  IRI_ITER(it, i) {
    P("%%%d ", it.v->id);
  }

  switch (i->opc) {
  case IR_CONST: P("%ld", i->cval); break;
  case IR_GLOBALPTR: P("$%s", i->gvar->name); break;
  case IR_LOCALPTR: P("$%d", i->lvar->id); break;
  case IR_UEXT:
  case IR_SEXT:
  case IR_LOAD:
  case IR_STORE:
  case IR_MEMCPY: P("%d", i->size); break;
  }
  P("\n");
  IPASS_BBEGIN(print)
  if (b->is_entry)
    P("B%d: ; entry ", b->hdr.id);
  else if (b->is_exit)
    P("B%d: ; exit ", b->hdr.id);
  else
    P("B%d: ; ", b->hdr.id);
  if (!b->is_entry) {
    P("idom: B%d preds: ", b->idom->hdr.id);
    IRB_PREDS(it, b) {
      P("B%d ", it.b->hdr.id);
    }
  }
  if (!b->is_exit) {
    P("succs: ");
    IRB_SUCCS(it, b) {
      P("B%d ", it.b->hdr.id);
    }
  }
  L("%s%s", b->start_loop ? " (start loop)" : "",
    b->post_loop ? " (post loop)" : "");
IPASS_END(print)

void irprint(IRProgram *p, FILE *out) {
  output_file = out;
  for (IRFunction *f = p->funs; f; f = f->next) {
    L("\nfunction%s %s", f->obj->is_static ? " static" : "", f->obj->name);
    for (IRLocal *v = f->locals; v; v = v->next) {
      L("local $%d (%s) %d %d ; %d uses", v->id, v->obj->name, v->obj->ty->size,
        v->obj->align, v->numuses);
    }
    irpass_print(f);
  }
}

#undef P
#undef L

void codegen(Obj *prog, FILE *out) {
  IRProgram *p = irgen(prog);

  IR_RUNPASS(irpass_opt_cfg, p);
  IR_RUNPASS(irpass_setup_rpo, p);

  IR_RUNPASS(irpass_constant_fold, p);

  IR_RUNPASS(irpass_opt_cfg, p);
  IR_RUNPASS(irpass_setup_rpo, p);
  IR_RUNPASS(irpass_calc_loops, p);
  IR_RUNPASS(irpass_setup_rpo, p);
  IR_RUNPASS(irpass_calc_idoms, p);

  //  TODO: optimize more

  IR_RUNPASS(irpass_dce, p);

  IR_RUNPASS(irpass_numbering, p);
  IR_RUNPASS(irpass_final, p);

  if (opt_emitir) {
    irprint(p, out);
  } else {
    ircodegen(p, out);
  }
}
