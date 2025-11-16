#include "ir.h"

const char *R[] = {"zr", "sp", "a0", "a1",  "a2",  "a3",  "a4", "t0",
                   "t1", "t2", "t3", "t4",  "t5",  "t6",  "t7", "t8",
                   "t9", "s0", "s1", "s2",  "s3",  "s4",  "s5", "s6",
                   "s7", "s8", "s9", "s10", "s11", "s12", "fp", "lr"};

#define SPREG 1
#define ARGSSTART 2
#define RETREG ARGSSTART
#define NARGS 5
#define TEMPSTART 7
#define NTEMP 10
#define SAVEDSTART 17
#define NSAVED 13
#define FPREG 30

#define SIZE_SUFFIX(sz) (sz == 1 ? "b" : sz == 2 ? "h" : "w")

static FILE *output_file;

// later make this a growable vector where
// slots > 32 represent spill on stack
// <32 are registers
static IRInstr *usedloc[32] = {};

static int alloc_temp() {
  for (int i = TEMPSTART; i < TEMPSTART + NTEMP; i++) {
    if (usedloc[i])
      continue;
    return i;
  }
  fprintf(stderr, "spill\n"); // TODO
  return 1;
}

static void alloc_i(IRInstr *i) {
  if (i->curloc != 0 || i->hdr.numuses == 0)
    return;
  i->curloc = alloc_temp();
  usedloc[i->curloc] = i;
}

static void use_i(IRInstr *i) {
  if (++i->curuses == i->hdr.numuses)
    usedloc[i->curloc] = NULL;
}

static bool is_valid_limm16(uint32_t imm) {
  return (imm & 0xffff) == 0 || (imm & 0xffff0000) == 0 ||
         (~imm & 0xffff) == 0 || (~imm & 0xffff0000) == 0;
}

static bool is_valid_aimm16(uint32_t imm) {
  return (imm & 0xffff) == 0 || (imm & 0xffff0000) == 0 ||
         (-imm & 0xffff) == 0 || (-imm & 0xffff0000) == 0;
}

#define P(fmt, ...) fprintf(output_file, fmt "\n" __VA_OPT__(, ) __VA_ARGS__)
#define I(fmt, ...)                                                            \
  fprintf(output_file, "  " fmt "\n" __VA_OPT__(, ) __VA_ARGS__)

static void gen_mov(int dstloc, int srcloc) {
  if (dstloc == 0 || dstloc == srcloc)
    return;
  I("mov %s, %s", R[dstloc], R[srcloc]);
}

static void gen_i(IRInstr *i);

enum { COND_GT, COND_LE, COND_EQ, COND_NE, COND_LT, COND_GE };

static int gen_cmp(IRInstr *i) {
  int op1, op2;
  bool imm = false;
  bool swap = false;
  if (i->iops[0]->opc == IR_CONST && is_valid_aimm16(i->iops[0]->cval)) {
    imm = true;
    swap = true;
    gen_i(i->iops[1]);
    op1 = i->iops[1]->curloc;
    op2 = i->iops[0]->cval;
  } else if (i->iops[1]->opc == IR_CONST && is_valid_aimm16(i->iops[1]->cval)) {
    imm = true;
    gen_i(i->iops[0]);
    op1 = i->iops[0]->curloc;
    op2 = i->iops[1]->cval;
  } else {
    gen_i(i->iops[0]);
    gen_i(i->iops[1]);
    op1 = i->iops[0]->curloc;
    op2 = i->iops[1]->curloc;
  }
  use_i(i->iops[0]);
  use_i(i->iops[1]);
  const char *opcode = "";
  switch (i->opc) {
  case IR_EQ:
  case IR_NE:
  case IR_ULT:
  case IR_ULE: opcode = "ucmp"; break;
  case IR_SLT:
  case IR_SLE: opcode = "scmp"; break;
  }
  if (imm) {
    I("%si %s, %d", opcode, R[op1], op2);
  } else {
    I("%s %s, %s", opcode, R[op1], R[op2]);
  }
  switch (i->opc) {
  case IR_EQ: return COND_EQ;
  case IR_NE: return COND_NE;
  case IR_ULT:
  case IR_SLT: return swap ? COND_GT : COND_LT;
  case IR_ULE:
  case IR_SLE: return swap ? COND_GE : COND_LE;
  default: return 0;
  }
}

enum {
  ADDR_REG,
  ADDR_REG_IMM,
  ADDR_REG_REG,
  ADDR_REG_IDX,
};

typedef struct {
  int mode;
  int base;
  int offset;
  int scale;
} AddrMode;

static bool valid_scale(int i) {
  return i == 1 || i == 2 || i == 4;
}

static void gen_addr(IRInstr *i, AddrMode *res) {
  switch (i->opc) {
  case IR_ADD:
    if (i->iops[0]->opc == IR_LOCALPTR && i->iops[1]->opc == IR_CONST) {
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      use_i(i);
      res->mode = ADDR_REG_IMM;
      res->base = FPREG;
      res->offset = -i->iops[0]->lvar->offset + i->iops[1]->cval;
    } else if (i->iops[1]->opc == IR_CONST) {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      use_i(i);
      res->mode = ADDR_REG_IMM;
      res->base = i->iops[0]->curloc;
      res->offset = i->iops[1]->cval;
    } else if (i->iops[1]->opc == IR_MUL &&
               i->iops[1]->iops[1]->opc == IR_CONST &&
               valid_scale(i->iops[1]->iops[1]->cval)) {
      // TODO: change the check to shl after multiply is converted to shl
      gen_i(i->iops[0]);
      gen_i(i->iops[1]->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]->iops[0]);
      use_i(i->iops[1]->iops[1]);
      use_i(i->iops[1]);
      use_i(i);
      res->mode = ADDR_REG_IDX;
      res->base = i->iops[0]->curloc;
      res->offset = i->iops[1]->iops[0]->curloc;
      res->scale = i->iops[1]->iops[1]->cval;
    } else {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      use_i(i);
      res->mode = ADDR_REG_REG;
      res->base = i->iops[0]->curloc;
      res->offset = i->iops[1]->curloc;
    }
    break;
  case IR_LOCALPTR:
    use_i(i);
    res->mode = ADDR_REG_IMM;
    res->base = FPREG;
    res->offset = -i->lvar->offset;
    break;
  default:
    gen_i(i);
    use_i(i);
    res->mode = ADDR_REG;
    res->base = i->curloc;
  }
}

static void gen_i(IRInstr *i) {
  if (i->hdr.visited) // TODO: handle reloading after spill
    return;
  i->hdr.visited = true;
  switch (i->opc) {
  case IR_CONST:
    alloc_i(i);
    I("movi %s, %#x", R[i->curloc], (uint32_t) i->cval);
    break;
  case IR_GLOBALPTR:
    alloc_i(i);
    I("adr %s, %s", R[i->curloc], i->gvar->name);
    break;
  case IR_LOCALPTR:
    alloc_i(i);
    I("addi %s, fp, %d", R[i->curloc], -i->lvar->offset);
    break;
  case IR_ADD:
    if (i->iops[0]->opc == IR_CONST && is_valid_aimm16(i->iops[0]->cval)) {
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("addi %s, %s, %d", R[i->curloc], R[i->iops[1]->curloc],
        (uint32_t) i->iops[0]->cval);
    } else if (i->iops[1]->opc == IR_CONST &&
               is_valid_aimm16(i->iops[1]->cval)) {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("addi %s, %s, %d", R[i->curloc], R[i->iops[0]->curloc],
        (uint32_t) i->iops[1]->cval);
    } else {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("add %s, %s, %s", R[i->curloc], R[i->iops[0]->curloc],
        R[i->iops[1]->curloc]);
    }
    break;
  case IR_SUB:
    if (i->iops[0]->opc == IR_CONST && is_valid_aimm16(i->iops[0]->cval)) {
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("neg %s, %s", R[i->curloc], R[i->iops[1]->curloc]);
      I("addi %s, %s, %d", R[i->curloc], R[i->curloc],
        (uint32_t) i->iops[0]->cval);
    } else if (i->iops[1]->opc == IR_CONST &&
               is_valid_aimm16(i->iops[1]->cval)) {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("subi %s, %s, %d", R[i->curloc], R[i->iops[0]->curloc],
        (uint32_t) i->iops[1]->cval);
    } else {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("sub %s, %s, %s", R[i->curloc], R[i->iops[0]->curloc],
        R[i->iops[1]->curloc]);
    }
    break;
  case IR_AND:
  case IR_OR:
  case IR_XOR: {
    const char *opcode = (i->opc == IR_AND)  ? "and"
                         : (i->opc == IR_OR) ? "or"
                                             : "xor";
    if (i->iops[0]->opc == IR_CONST && is_valid_limm16(i->iops[0]->cval)) {
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%si %s, %s, %#x", opcode, R[i->curloc], R[i->iops[1]->curloc],
        (uint32_t) i->iops[0]->cval);
    } else if (i->iops[1]->opc == IR_CONST &&
               is_valid_limm16(i->iops[1]->cval)) {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%si %s, %s, %#x", opcode, R[i->curloc], R[i->iops[0]->curloc],
        (uint32_t) i->iops[1]->cval);
    } else {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%s %s, %s, %s", opcode, R[i->curloc], R[i->iops[0]->curloc],
        R[i->iops[1]->curloc]);
    }
    break;
  }
  case IR_SLL:
  case IR_SRL:
  case IR_SRA: {
    const char *opcode = (i->opc == IR_SLL)   ? "sll"
                         : (i->opc == IR_SRL) ? "srl"
                                              : "sra";
    if (i->iops[1]->opc == IR_CONST) {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%si %s, %s, %d", opcode, R[i->curloc], R[i->iops[0]->curloc],
        (uint32_t) i->iops[1]->cval % 32);
    } else {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%s %s, %s, %s", opcode, R[i->curloc], R[i->iops[0]->curloc],
        R[i->iops[1]->curloc]);
    }
    break;
  }
  case IR_NEG:
    gen_i(i->iops[0]);
    use_i(i->iops[0]);
    alloc_i(i);
    I("neg %s, %s", R[i->curloc], R[i->iops[0]->curloc]);
    break;
  case IR_NOT:
    gen_i(i->iops[0]);
    use_i(i->iops[0]);
    alloc_i(i);
    I("not %s, %s", R[i->curloc], R[i->iops[0]->curloc]);
    break;
  case IR_UEXT:
  case IR_SEXT: {
    gen_i(i->iops[0]);
    use_i(i->iops[0]);
    alloc_i(i);
    if (i->size >= 4)
      gen_mov(i->curloc, i->iops[0]->curloc);
    else
      I("%sx%s %s, %s", i->opc == IR_UEXT ? "u" : "s", SIZE_SUFFIX(i->size),
        R[i->curloc], R[i->iops[0]->curloc]);
    break;
  }
  case IR_EQ:
  case IR_NE:
  case IR_SLT:
  case IR_SLE:
  case IR_ULT:
  case IR_ULE: {
    const char *opcodes[] = {"setgt", "setle", "seteq",
                             "setne", "setlt", "setge"};
    int cond = gen_cmp(i);
    alloc_i(i);
    I("%s %s", opcodes[cond], R[i->curloc]);
    break;
  }
  case IR_CALL: {
    // TODO: stack args
    for (int a = 1; a < i->numops; a++) {
      i->iops[a]->curloc = ARGSSTART + a - 1;
      gen_i(i->iops[a]);
      use_i(i->iops[a]);
    }
    // TODO: save temp regs
    if (i->iops[0]->opc == IR_GLOBALPTR) {
      use_i(i->iops[0]);
      I("jl %s", i->iops[0]->gvar->name);
    } else {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      I("jlr %s", R[i->iops[0]->curloc]);
    }
    alloc_i(i);
    gen_mov(i->curloc, ARGSSTART);
    break;
  }
  case IR_ULOAD:
  case IR_SLOAD: {
    AddrMode amod;
    gen_addr(i->iops[0], &amod);
    alloc_i(i);
    switch (amod.mode) {
    case ADDR_REG:
      I("ld%s%s %s, (%s)", SIZE_SUFFIX(i->size),
        i->opc == IR_SLOAD && i->size != 4 ? "s" : "", R[i->curloc],
        R[amod.base]);
      break;
    case ADDR_REG_IMM:
      I("ld%s%s %s, %d(%s)", SIZE_SUFFIX(i->size),
        i->opc == IR_SLOAD && i->size != 4 ? "s" : "", R[i->curloc],
        amod.offset, R[amod.base]);
      break;
    case ADDR_REG_REG:
      I("ld%s%sx %s, (%s, %s)", SIZE_SUFFIX(i->size),
        i->opc == IR_SLOAD && i->size != 4 ? "s" : "", R[i->curloc],
        R[amod.base], R[amod.offset]);
      break;
    case ADDR_REG_IDX:
      I("ld%s%sx %s, (%s, %s, %d)", SIZE_SUFFIX(i->size),
        i->opc == IR_SLOAD && i->size != 4 ? "s" : "", R[i->curloc],
        R[amod.base], R[amod.offset], amod.scale);
      break;
    }
    break;
  }
  }
}

static void gen_b(IRBlock *b) {
  for (; b; b = b->rpo_next) {
    P(".B%d:", b->hdr.id);
    IRB_ITER(i, b) {
      switch (i->opc) {
      case IR_STORE: {
        AddrMode amod;
        gen_i(i->iops[1]);
        gen_addr(i->iops[0], &amod);
        use_i(i->iops[1]);
        switch (amod.mode) {
        case ADDR_REG:
          I("st%s %s, (%s)", SIZE_SUFFIX(i->size), R[i->iops[1]->curloc],
            R[amod.base]);
          break;
        case ADDR_REG_IMM:
          I("st%s %s, %d(%s)", SIZE_SUFFIX(i->size), R[i->iops[1]->curloc],
            amod.offset, R[amod.base]);
          break;
        case ADDR_REG_REG:
          I("st%sx %s, (%s, %s)", SIZE_SUFFIX(i->size), R[i->iops[1]->curloc],
            R[amod.base], R[amod.offset]);
          break;
        case ADDR_REG_IDX:
          I("st%sx %s, (%s, %s, %d)", SIZE_SUFFIX(i->size),
            R[i->iops[1]->curloc], R[amod.base], R[amod.offset], amod.scale);
          break;
        }
        break;
      }
      case IR_JP:
        if (b->rpo_next != i->bops[0])
          I("jp .B%d", i->ops[0]->id);
        break;
      case IR_BR: {
        const char *opcodes[] = {"bgt", "ble", "beq", "bne", "blt", "bge"};
        int cond = gen_cmp(i->iops[0]);
        use_i(i->iops[0]);
        if (b->rpo_next != i->bops[1])
          I("%s .B%d", opcodes[cond], i->ops[1]->id);
        if (b->rpo_next != i->bops[2])
          I("%s .B%d", opcodes[cond ^ 1], i->ops[2]->id);
        break;
      }
      case IR_RET:
        i->iops[0]->curloc = RETREG;
        gen_i(i->iops[0]);
        use_i(i->iops[0]);
        break;
      default: gen_i(i);
      }
    }
  }
}

static char hexdigit(int i) {
  if (i < 10)
    return '0' + i;
  else
    return 'a' + i - 10;
}

static char *escape_string_lit(const char *in) {
  char *res = malloc(strlen(in) * 2 + 1);
  char *outp = res;
  for (const char *inp = in; *inp; inp++) {
    if (isprint(*inp)) {
      *outp++ = *inp;
    } else {
      *outp++ = '\\';
      switch (*inp) {
      case '\n': *outp++ = 'n'; break;
      case '\r': *outp++ = 'r'; break;
      case '\t': *outp++ = 't'; break;
      default:
        *outp++ = 'x';
        *outp++ = hexdigit(*inp >> 4);
        *outp++ = hexdigit(*inp & 0xf);
        break;
      }
    }
  }
  *outp = '\0';
  return res;
}

void ircodegen(IRProgram *p, FILE *out) {
  output_file = out;
  for (IRFunction *f = p->funs; f; f = f->next) {
    memset(usedloc, 0, sizeof usedloc);
    // TODO: handle stack args/ struct args
    for (IRLocal *l = f->locals; l; l = l->next) {
      if (!l->numuses)
        continue;
      f->stacksize += l->obj->ty->size;
      f->stacksize = align_to(f->stacksize, l->obj->align);
      l->offset = f->stacksize;
    }
    f->stacksize = align_to(f->stacksize, 4);

    char *buf;
    size_t buflen;
    output_file = open_memstream(&buf, &buflen);

    ir_begin_pass((IRValue *) f->entry);
    gen_b(f->entry);

    fclose(output_file);
    output_file = out;

    I("#align 32");
    P("%s:", f->obj->name);
    I("stw lr, -4(sp)");
    I("stw fp, -8(sp)");
    I("subi fp, sp, 8");
    I("subi sp, sp, %d", 8 + f->stacksize);

    int pi = 0;
    for (IRLocal *p = f->params; p; p = p->next, pi++) {
      if (p->offset <= 0)
        continue;
      I("st%s %s, %d(fp)", SIZE_SUFFIX(p->obj->ty->size), R[ARGSSTART + pi],
        -p->offset);
    }

    fwrite(buf, 1, buflen, output_file);
    free(buf);

    I("addi sp, sp, %d", 8 + f->stacksize);
    I("ldw fp, -8(sp)");
    I("ldw lr, -4(sp)");
    I("ret");
    P();
  }
  for (Obj *g = p->obj; g; g = g->next) {
    if (g->is_function || !g->is_definition)
      continue;
    if (g->align != 1)
      I("#align %d", g->align * 8);
    P("%s:", g->name);
    if (g->init_data) {
      Relocation *rel = g->rel;
      if (g->is_string_literal) {
        char *esc = escape_string_lit(g->init_data);
        I("ds \"%s\"", esc);
        free(esc);
      } else {
        int pos = 0;
        while (pos < g->ty->size) {
          if (rel && rel->offset == pos) {
            I("dw %s%+ld", *rel->label, rel->addend);
            rel = rel->next;
            pos += 4;
          } else if (g->ty->size - pos >= 4) {
            I("dw %#x", *(uint32_t *) &g->init_data[pos]);
            pos += 4;
          } else {
            I("db %#x", g->init_data[pos++]);
          }
        }
      }
    } else {
      I("#res %#x", g->ty->size);
    }
  }
}
#undef P
#undef I
