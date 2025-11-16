#include "ir.h"
#include "irpass.h"

#define EMIT_IR 0

void codegen(Obj *prog, FILE *out) {
  IRProgram *p = irgen(prog);
  IR_RUNPASS(irpass_fix_cfg, p);
  IR_RUNPASS(irpass_setup_rpo, p);
// TODO: optimize
#if EMIT_IR
  irprint(p, out);
#else
  ircodegen(p, out);
#endif
}

void ir_add_instr(IRBlock *b, IRInstr *i) {
  ir_insert_instr(&b->root, i);
}

void ir_insert_instr(IRInstr *before, IRInstr *i) {
  i->next = before;
  i->prev = before->prev;
  i->next->prev = i;
  i->prev->next = i;
}

void ir_remove_instr(IRInstr *i) {
  if (!(i->prev && i->next))
    return;
  i->prev->next = i->next;
  i->next->prev = i->prev;
  i->prev = i->next = NULL;
}

void ir_erase_instr(IRInstr *i) {
  assert(i->hdr.numuses == 0 && !i->hdr.uses);
  ir_remove_instr(i);
  for (int o = 0; o < i->numops; o++) {
    ir_remove_user(i->ops[o], i);
  }
  if (i->opc == IR_LOCALPTR)
    i->lvar->numuses--;
  free(i);
}

void ir_set_op(IRInstr *i, int op, IRValue *v) {
  if (i->ops[op]) {
    ir_remove_user(i->ops[op], i);
  }
  i->ops[op] = v;
  v->numuses++;
  IRUser *n = malloc(sizeof *n);
  n->next = v->uses;
  n->user = i;
  n->idx = op;
  v->uses = n;
}

void ir_remove_user(IRValue *v, IRInstr *user) {
  if (user) {
    IRUser **u = &v->uses;
    while (*u && (*u)->user != user)
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
      ir_erase_instr((IRInstr *) v);
    } else if (v->vt == IRV_BLOCK) {
      IRBlock *b = (IRBlock *) v;
      while (!IRB_ISEMPTY(b))
        ir_remove_user((IRValue *) IRB_FIRST(b), NULL);
      free(b);
    }
  }
}

void ir_begin_pass(IRValue *v) {
  if (!v->visited)
    return;
  v->visited = false;
  if (v->vt == IRV_INSTR) {
    IRInstr *i = (IRInstr *) v;
    for (int o = 0; o < i->numops; o++)
      ir_begin_pass(i->ops[o]);
  } else if (v->vt == IRV_BLOCK) {
    IRBlock *b = (IRBlock *) v;
    IRB_ITER(i, b) {
      i->hdr.visited = true;
      ir_begin_pass((IRValue *) i);
    }
  }
}

void ir_replace(IRValue *old, IRValue *new) {
  if (!old->uses) {
    assert(old->vt == IRV_INSTR && new->vt == IRV_INSTR &&
           ((IRInstr *) old)->prev && ((IRInstr *) old)->next);
    ir_insert_instr((IRInstr *) old, (IRInstr *) new);
    ir_erase_instr((IRInstr *) old);
    return;
  }
  while (old->uses->next) {
    ir_set_op(old->uses->user, old->uses->idx, new);
  }
  ir_set_op(old->uses->user, old->uses->idx, new);
}

BPASS_BEGIN(fix_cfg) {
  if (b->is_exit)
    return;
  bool found_term = false;
  IRB_ITER(i, b) {
    if (found_term) {
      ir_erase_instr(i);
    } else if (IROPC_ISTERM(i->opc))
      found_term = true;
  }
  assert(found_term && !IRB_ISEMPTY(b));

  switch (IRB_LAST(b)->opc) {
  case IR_JP: BPASS_REC(fix_cfg, IRB_LAST(b)->bops[0]); break;
  case IR_BR:
    BPASS_REC(fix_cfg, IRB_LAST(b)->bops[1]);
    BPASS_REC(fix_cfg, IRB_LAST(b)->bops[2]);
    break;
  }

  if (!b->is_entry && IRB_FIRST(b) == IRB_LAST(b) &&
      IRB_LAST(b)->opc == IR_JP) {
    ir_replace((IRValue *) b, (IRValue *) IRB_LAST(b)->bops[0]);
  }
}
BPASS_END(fix_cfg)

BPASS_BEGIN(setup_rpo, IRBlock **res) {
  if (b->is_exit)
    return;
  switch (IRB_LAST(b)->opc) {
  case IR_JP: BPASS_REC(setup_rpo, IRB_LAST(b)->bops[0], res); break;
  case IR_BR:
    BPASS_REC(setup_rpo, IRB_LAST(b)->bops[1], res);
    BPASS_REC(setup_rpo, IRB_LAST(b)->bops[2], res);
    break;
  }
  b->rpo_next = *res;
  *res = b;
}
BPASS_END(setup_rpo, &(IRBlock *) {f->exit})

const char *ir_opc_names[IR_MAX] = {
    "const", "globalptr", "localptr", "add",  "sub",  "mul",  "sdiv", "smod",
    "udiv",  "umod",      "and",      "or",   "xor",  "sll",  "srl",  "sra",
    "neg",   "not",       "uext",     "sext", "ubfe", "sbfe", "bfi",  "eq",
    "ne",    "slt",       "sle",      "ult",  "ule",  "call", "ret",  "load",
    "store", "memcpy",    "jp",       "br",
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
    if (IROPC_HASRES(i->opc))
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
    case IR_LOAD:
    case IR_STORE:
    case IR_MEMCPY: I("%d", i->size); break;
    }

    I(" ; %d uses\n", i->hdr.numuses);
  } else if (v->vt == IRV_BLOCK) {
    IRBlock *b = (IRBlock *) v;
    if (b->is_entry)
      P("B%d: ; entry", b->hdr.id);
    else if (b->is_exit)
      P("B%d: ; exit, %d uses", b->hdr.id, b->hdr.numuses);
    else
      P("B%d: ; %d uses", b->hdr.id, b->hdr.numuses);
    IRB_ITER(i, b) {
      irprint_v((IRValue *) i, out);
    }
    if (b->rpo_next)
      irprint_v((IRValue *) b->rpo_next, out);
  }
}

void irprint(IRProgram *p, FILE *out) {
  for (IRFunction *f = p->funs; f; f = f->next) {
    P("\nfunction%s %s", f->obj->is_static ? " static" : "", f->obj->name);
    for (IRLocal *v = f->locals; v; v = v->next) {
      P("local %%%d (%s) %d %d ; %d uses", v->id, v->obj->name,
        v->obj->ty->size, v->obj->align, v->numuses);
    }
    ir_begin_pass((IRValue *) f->entry);
    irprint_v((IRValue *) f->entry, out);
  }
}

#undef P
#undef I
