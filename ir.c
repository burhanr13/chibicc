#include "ir.h"

void codegen(Obj *prog, FILE *out) {
  IRProgram *p = irgen(prog);
  // TODO: optimize
  ircodegen(p, out);
}

void ir_add_instr(IRBlock *b, IRInstr *i) {
  if (!b->first) {
    b->first = i;
    b->last = i;
  } else {
    b->last->next = i;
    i->prev = b->last;
    b->last = i;
  }
  ir_add_user((IRValue *) i, (IRValue *) b);
}

void ir_remove_instr(IRBlock *b, IRInstr *i) {
  if (i->prev) {
    i->prev->next = i->next;
    i->prev = NULL;
  } else {
    b->first = i->next;
  }
  if (i->next) {
    i->next->prev = i->prev;
    i->next = NULL;
  } else {
    b->last = i->prev;
  }
  ir_remove_user((IRValue *) i, (IRValue *) b);
}

void ir_add_user(IRValue *v, IRValue *user) {
  if (user->vt == IRV_INSTR)
    v->numuses++;
  IRUser *n = malloc(sizeof *n);
  n->next = v->uses;
  n->user = user;
  v->uses = n;
}

void ir_remove_user(IRValue *v, IRValue *user) {
  IRUser **u = &v->uses;
  while (*u && (*u)->user != user)
    u = &(*u)->next;
  if (*u) {
    if ((*u)->user->vt == IRV_INSTR)
      v->numuses--;
    IRUser *tmp = *u;
    *u = (*u)->next;
    free(tmp);
  }
  if (!v->uses) {
    if (v->vt == IRV_INSTR) {
      IRInstr *j = (IRInstr *) v;
      for (int i = 0; i < j->numops; i++) {
        ir_remove_user(j->ops[i], (IRValue *) j);
      }
      if (j->opc == IR_LOCALPTR)
        j->lvar->numuses--;
      free(j);
    } else if (v->vt == IRV_BLOCK) {
      IRBlock *b = (IRBlock *) v;
      while (b->first)
        ir_remove_user((IRValue *) b->first, (IRValue *) b);
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
    for (IRInstr *i = b->first; i; i = i->next) {
      ir_begin_pass((IRValue *) i);
    }
  }
}

void ir_setup_rpo2(IRBlock *b, IRBlock **res) {
  if (b->hdr.visited)
    return;
  b->hdr.visited = true;
  switch (b->last->opc) {
  case IR_JP: ir_setup_rpo2(b->last->bops[0], res); break;
  case IR_BR:
    ir_setup_rpo2(b->last->bops[1], res);
    ir_setup_rpo2(b->last->bops[2], res);
    break;
  }
  b->rpo_next = *res;
  *res = b;
}

void ir_setup_rpo(IRBlock *b) {
  IRBlock *res = NULL;
  ir_setup_rpo2(b, &res);
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

    I(" ; %d uses\n", i->hdr.numuses);
  } else if (v->vt == IRV_BLOCK) {
    IRBlock *b = (IRBlock *) v;
    P("B%d: ; %d uses", b->hdr.id, b->hdr.numuses);
    for (IRInstr *i = b->first; i; i = i->next) {
      irprint_v((IRValue *) i, out);
    }
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
    ir_setup_rpo(f->entry);
    irprint_v((IRValue *) f->entry, out);
  }
}

#undef P
#undef I
