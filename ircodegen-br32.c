#include "ir.h"

const char *R[] = {"zr", "sp", "a0", "a1",  "a2",  "a3",  "a4", "t0",
                   "t1", "t2", "t3", "t4",  "t5",  "t6",  "t7", "t8",
                   "t9", "s0", "s1", "s2",  "s3",  "s4",  "s5", "s6",
                   "s7", "s8", "s9", "s10", "s11", "s12", "fp", "lr"};

#define ZREG 0
#define SPREG 1
#define ARGSSTART 2
#define RETREG ARGSSTART
#define NARGS 5
#define TEMPSTART 7
#define NTEMP 10
#define SAVEDSTART 17
#define NSAVED 14
#define FPREG 30
#define LRREG 31

#define WORDSIZE 4

#define SIZE_SUFFIX(sz) (sz == 1 ? "b" : sz == 2 ? "h" : "w")

static FILE *output_file;

static IRFunction *cur_fun;
static IRVarLoc *usedreg[32];
static int maxsaved;
static int curstack;
#define LOCKED_REG ((IRVarLoc *) -1)

#define P(fmt, ...) fprintf(output_file, fmt "\n" __VA_OPT__(, ) __VA_ARGS__)
#define I(fmt, ...)                                                            \
  fprintf(output_file, "  " fmt "\n" __VA_OPT__(, ) __VA_ARGS__)

static int alloc_stack(int size, int align) {
  curstack += size;
  curstack = align_to(curstack, align);
  if (curstack > cur_fun->stacksize)
    cur_fun->stacksize = curstack;
  return curstack;
}

static void gen_mov(int dstreg, IRVarLoc *src) {
  if (dstreg == ZREG)
    return;
  if (src->sclass == LOC_REG) {
    if (dstreg != src->reg)
      I("mov %s, %s", R[dstreg], R[src->reg]);
  } else if (src->sclass == LOC_STACK) {
    I("ldw %s, %d(fp)", R[dstreg], src->stackoff);
  }
}

static void gen_movr(int dstreg, int srcreg) {
  if (dstreg != srcreg) {
    I("mov %s, %s", R[dstreg], R[srcreg]);
  }
}

static int alloc_saved() {
  for (int i = SAVEDSTART; i < SAVEDSTART + NSAVED; i++) {
    if (usedreg[i])
      continue;
    if (i > maxsaved)
      maxsaved = i;
    return i;
  }
  return -1;
}

static void gen_spill(int reg) {
  if (!usedreg[reg])
    return;
  assert(usedreg[reg] != LOCKED_REG);
  int s = alloc_saved();
  if (s >= 0) {
    I("mov %s, %s", R[s], R[reg]);
    usedreg[s] = usedreg[reg];
    usedreg[s]->reg = s;
  } else {
    int soff = alloc_stack(WORDSIZE, WORDSIZE);
    I("stw %s, %d(fp)", R[reg], -soff);
    usedreg[reg]->sclass = LOC_STACK;
    usedreg[reg]->stackoff = soff;
  }
  usedreg[reg] = NULL;
}

static void clobber_temp() {
  for (int i = ARGSSTART; i < ARGSSTART + NARGS; i++) {
    gen_spill(i);
  }
  for (int i = TEMPSTART; i < TEMPSTART + NTEMP; i++) {
    gen_spill(i);
  }
  if (usedreg[LRREG] != LOCKED_REG)
    gen_spill(LRREG);
}

static int alloc_temp() {
  if (cur_fun->is_leaf) {
    for (int i = ARGSSTART; i < ARGSSTART + NARGS; i++) {
      if (usedreg[i])
        continue;
      return i;
    }
  }
  for (int i = TEMPSTART; i < TEMPSTART + NTEMP; i++) {
    if (usedreg[i])
      continue;
    return i;
  }
  if (!usedreg[LRREG])
    return LRREG;
  int s = alloc_saved();
  if (s >= 0)
    return s;
  // TODO: figure out a better way to pick this
  gen_spill(TEMPSTART);
  return TEMPSTART;
}

static void gen_reload(IRVarLoc *loc) {
  assert(loc->sclass == LOC_STACK);
  int r = alloc_temp();
  I("ldw %s, %d(fp)", R[r], -loc->stackoff);
  loc->sclass = LOC_REG;
  loc->reg = r;
  usedreg[r] = loc;
}

static void alloc_i(IRInstr *i) {
  if (i->hdr.numuses == 0) {
    i->curloc.sclass = LOC_REG;
    i->curloc.reg = ZREG;
    return;
  }
  if (i->curloc.sclass == LOC_UNALLOC) {
    i->curloc.reg = alloc_temp();
    i->curloc.sclass = LOC_REG;
  }
  assert(i->curloc.sclass == LOC_REG);
  if (usedreg[i->curloc.reg] != LOCKED_REG)
    usedreg[i->curloc.reg] = &i->curloc;
}

static void alloc_i_fixed(IRInstr *i, int reg) {
  if (i->curloc.sclass == LOC_UNALLOC) {
    i->curloc.sclass = LOC_REG;
    i->curloc.reg = reg;
  }
  gen_movr(i->curloc.reg, reg);
  alloc_i(i);
}

static void use_i(IRInstr *i) {
  if (++i->curuses == i->hdr.numuses) {
    if (i->curloc.sclass == LOC_REG && usedreg[i->curloc.reg] == &i->curloc)
      usedreg[i->curloc.reg] = NULL;
    else if (i->curloc.sclass == LOC_STACK && i->curloc.stackoff == curstack)
      curstack -= WORDSIZE;
  }
}

static bool is_valid_limm16(uint32_t imm) {
  return (imm & 0xffff) == 0 || (imm & 0xffff0000) == 0 ||
         (~imm & 0xffff) == 0 || (~imm & 0xffff0000) == 0;
}

static bool is_valid_aimm16(uint32_t imm) {
  return (imm & 0xffff) == 0 || (imm & 0xffff0000) == 0 ||
         (-imm & 0xffff) == 0 || (-imm & 0xffff0000) == 0;
}

static void gen_i(IRInstr *i);

static void gen_i_fixed(IRInstr *i, int reg) {
  if (i->curloc.sclass == LOC_UNALLOC) {
    i->curloc.sclass = LOC_REG;
    i->curloc.reg = reg;
  }
  gen_i(i);
  gen_movr(reg, i->curloc.reg);
}

enum { COND_GT, COND_LE, COND_EQ, COND_NE, COND_LT, COND_GE };

static int gen_cmp(IRInstr *i) {
  int op1, op2;
  bool imm = false;
  bool swap = false;
  if (i->iops[0]->opc == IR_CONST && is_valid_aimm16(i->iops[0]->cval)) {
    imm = true;
    swap = true;
    gen_i(i->iops[1]);
    op1 = i->iops[1]->curloc.reg;
    op2 = i->iops[0]->cval;
  } else if (i->iops[1]->opc == IR_CONST && is_valid_aimm16(i->iops[1]->cval)) {
    imm = true;
    gen_i(i->iops[0]);
    op1 = i->iops[0]->curloc.reg;
    op2 = i->iops[1]->cval;
  } else {
    gen_i(i->iops[0]);
    gen_i(i->iops[1]);
    op1 = i->iops[0]->curloc.reg;
    op2 = i->iops[1]->curloc.reg;
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
} AddrMode;

static void gen_addr(IRInstr *i, AddrMode *res, int size) {
  switch (i->opc) {
  case IR_ADD:
    if (i->iops[0]->opc == IR_LOCALPTR && i->iops[1]->opc == IR_CONST) {
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      use_i(i);
      res->mode = ADDR_REG_IMM;
      res->base = FPREG;
      res->offset = -i->iops[0]->lvar->curloc.stackoff + i->iops[1]->cval;
    } else if (i->iops[1]->opc == IR_CONST) {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      use_i(i);
      res->mode = ADDR_REG_IMM;
      res->base = i->iops[0]->curloc.reg;
      res->offset = i->iops[1]->cval;
    } else if (i->iops[1]->opc == IR_SLL &&
               i->iops[1]->iops[1]->opc == IR_CONST &&
               1 << i->iops[1]->iops[1]->cval == size) {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]->iops[0]);
      use_i(i->iops[1]->iops[1]);
      use_i(i->iops[1]);
      use_i(i);
      res->mode = ADDR_REG_IDX;
      res->base = i->iops[0]->curloc.reg;
      res->offset = i->iops[1]->iops[0]->curloc.reg;
    } else {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      use_i(i);
      res->mode = ADDR_REG_REG;
      res->base = i->iops[0]->curloc.reg;
      res->offset = i->iops[1]->curloc.reg;
    }
    break;
  case IR_LOCALPTR:
    use_i(i);
    res->mode = ADDR_REG_IMM;
    res->base = FPREG;
    res->offset = -i->lvar->curloc.stackoff;
    break;
  default:
    gen_i(i);
    use_i(i);
    res->mode = ADDR_REG;
    res->base = i->curloc.reg;
  }
}

static void gen_load(int dstreg, AddrMode *amod, bool is_signed, int size) {
  switch (amod->mode) {
  case ADDR_REG:
    I("ld%s%s %s, (%s)", SIZE_SUFFIX(size), is_signed ? "s" : "", R[dstreg],
      R[amod->base]);
    break;
  case ADDR_REG_IMM:
    I("ld%s%s %s, %d(%s)", SIZE_SUFFIX(size), is_signed ? "s" : "", R[dstreg],
      amod->offset, R[amod->base]);
    break;
  case ADDR_REG_REG:
    I("ld%s%sx %s, (%s, %s)", SIZE_SUFFIX(size), is_signed ? "s" : "",
      R[dstreg], R[amod->base], R[amod->offset]);
    break;
  case ADDR_REG_IDX:
    I("ld%s%sx %s, (%s, %s, %d)", SIZE_SUFFIX(size), is_signed ? "s" : "",
      R[dstreg], R[amod->base], R[amod->offset], size);
    break;
  }
}

static void gen_i(IRInstr *i) {
  if (i->curloc.sclass == LOC_STACK)
    gen_reload(&i->curloc);
  if (i->hdr.visited) {
    assert(i->curloc.sclass == LOC_REG);
    return;
  }
  i->hdr.visited = true;
  switch (i->opc) {
  case IR_CONST:
    if (i->cval == 0) {
      alloc_i_fixed(i, ZREG);
    } else {
      alloc_i(i);
      I("movi %s, %d", R[i->curloc.reg], (uint32_t) i->cval);
    }
    break;
  case IR_GLOBALPTR:
    alloc_i(i);
    I("adr %s, %s", R[i->curloc.reg], i->gvar->name);
    break;
  case IR_LOCALPTR:
    if (i->lvar->curloc.sclass != LOC_STACK) {
      int soff = alloc_stack(WORDSIZE, WORDSIZE);
      I("stw %s, %d(fp)", R[i->lvar->curloc.reg], -soff);
      usedreg[i->lvar->curloc.reg] = NULL;
      i->lvar->curloc.sclass = LOC_STACK;
      i->lvar->curloc.stackoff = soff;
    }
    alloc_i(i);
    I("addi %s, fp, %d", R[i->curloc.reg], -i->lvar->curloc.stackoff);
    break;
  case IR_ADD:
  case IR_SUB: {
    const char *opcode = (i->opc == IR_ADD) ? "add" : "sub";
    if (i->iops[1]->opc == IR_CONST && is_valid_aimm16(i->iops[1]->cval)) {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%si %s, %s, %d", opcode, R[i->curloc.reg], R[i->iops[0]->curloc.reg],
        (uint32_t) i->iops[1]->cval);
    } else {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%s %s, %s, %s", opcode, R[i->curloc.reg], R[i->iops[0]->curloc.reg],
        R[i->iops[1]->curloc.reg]);
    }
    break;
  }
  case IR_AND:
  case IR_OR:
  case IR_XOR: {
    const char *opcode = (i->opc == IR_AND)  ? "and"
                         : (i->opc == IR_OR) ? "or"
                                             : "xor";
    if (i->iops[1]->opc == IR_CONST && is_valid_limm16(i->iops[1]->cval)) {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%si %s, %s, %#x", opcode, R[i->curloc.reg], R[i->iops[0]->curloc.reg],
        (uint32_t) i->iops[1]->cval);
    } else {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%s %s, %s, %s", opcode, R[i->curloc.reg], R[i->iops[0]->curloc.reg],
        R[i->iops[1]->curloc.reg]);
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
      I("%si %s, %s, %d", opcode, R[i->curloc.reg], R[i->iops[0]->curloc.reg],
        (uint32_t) i->iops[1]->cval % 32);
    } else {
      gen_i(i->iops[0]);
      gen_i(i->iops[1]);
      use_i(i->iops[0]);
      use_i(i->iops[1]);
      alloc_i(i);
      I("%s %s, %s, %s", opcode, R[i->curloc.reg], R[i->iops[0]->curloc.reg],
        R[i->iops[1]->curloc.reg]);
    }
    break;
  }
  case IR_NEG:
  case IR_NOT: {
    const char *opcode = i->opc == IR_NEG ? "neg" : "not";
    gen_i(i->iops[0]);
    use_i(i->iops[0]);
    alloc_i(i);
    I("%s %s, %s", opcode, R[i->curloc.reg], R[i->iops[0]->curloc.reg]);
    break;
  }
  case IR_UEXT:
  case IR_SEXT:
    if (i->iops[0]->opc == IR_LOAD &&
        !(i->iops[0]->iops[0]->opc == IR_LOCALPTR &&
          i->iops[0]->iops[0]->lvar->curloc.sclass == LOC_REG) &&
        i->iops[0]->size == i->size) {
      AddrMode amod;
      use_i(i->iops[0]);
      gen_addr(i->iops[0]->iops[0], &amod, i->size);
      alloc_i(i);
      gen_load(i->curloc.reg, &amod, i->opc == IR_SEXT, i->size);
    } else {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      alloc_i(i);
      if (i->size >= WORDSIZE) {
        gen_mov(i->curloc.reg, &i->iops[0]->curloc);
      } else {
        I("%sx%s %s, %s", i->opc == IR_UEXT ? "u" : "s", SIZE_SUFFIX(i->size),
          R[i->curloc.reg], R[i->iops[0]->curloc.reg]);
      }
    }
    break;
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
    I("%s %s", opcodes[cond], R[i->curloc.reg]);
    break;
  }
  case IR_MUL:
  case IR_SDIV:
  case IR_UDIV: // TODO: separate udiv
    gen_spill(ARGSSTART);
    gen_spill(ARGSSTART + 1);
    gen_i_fixed(i->iops[0], ARGSSTART);
    gen_i_fixed(i->iops[1], ARGSSTART + 1);
    gen_mov(ARGSSTART, &i->iops[0]->curloc);
    gen_mov(ARGSSTART + 1, &i->iops[1]->curloc);
    use_i(i->iops[0]);
    use_i(i->iops[1]);
    clobber_temp();
    I("jl %s", i->opc == IR_MUL ? "__mul" : "__div");
    alloc_i_fixed(i, RETREG);
    break;
  case IR_CALL:
    // TODO: stack args
    for (int a = 1; a < i->numops; a++) {
      if (i->iops[a]->curloc.sclass == LOC_UNALLOC) {
        gen_spill(ARGSSTART + a - 1);
        i->iops[a]->curloc.sclass = LOC_REG;
        i->iops[a]->curloc.reg = ARGSSTART + a - 1;
        gen_i(i->iops[a]);
      }
    }
    for (int a = 1; a < i->numops; a++) {
      gen_mov(ARGSSTART + a - 1, &i->iops[a]->curloc);
      use_i(i->iops[a]);
    }
    clobber_temp();
    if (i->iops[0]->opc == IR_GLOBALPTR) {
      use_i(i->iops[0]);
      I("jl %s", i->iops[0]->gvar->name);
    } else {
      gen_i(i->iops[0]);
      use_i(i->iops[0]);
      I("jlr %s", R[i->iops[0]->curloc.reg]);
    }
    alloc_i_fixed(i, RETREG);
    break;
  case IR_LOAD:
    if (i->iops[0]->opc == IR_LOCALPTR &&
        i->iops[0]->lvar->curloc.sclass == LOC_REG) {
      if (i->hdr.numuses > 1) {
        alloc_i(i);
        gen_movr(i->curloc.reg, i->iops[0]->lvar->curloc.reg);
      } else {
        alloc_i_fixed(i, i->iops[0]->lvar->curloc.reg);
      }
    } else {
      AddrMode amod;
      gen_addr(i->iops[0], &amod, i->size);
      alloc_i(i);
      gen_load(i->curloc.reg, &amod, false, i->size);
    }
    break;
  }
}

static void gen_b(IRBlock *b) {
  for (; b; b = b->rpo_next) {
    b->hdr.visited = true;
    P(".B%d:", b->hdr.id);
    IRB_ITER(i, b) {
      switch (i->opc) {
      case IR_STORE:
        if (i->iops[0]->opc == IR_LOCALPTR &&
            i->iops[0]->lvar->curloc.sclass == LOC_REG) {
          gen_i_fixed(i->iops[1], i->iops[0]->lvar->curloc.reg);
        } else {
          AddrMode amod;
          gen_i(i->iops[1]);
          gen_addr(i->iops[0], &amod, i->size);
          use_i(i->iops[1]);
          switch (amod.mode) {
          case ADDR_REG:
            I("st%s %s, (%s)", SIZE_SUFFIX(i->size), R[i->iops[1]->curloc.reg],
              R[amod.base]);
            break;
          case ADDR_REG_IMM:
            I("st%s %s, %d(%s)", SIZE_SUFFIX(i->size),
              R[i->iops[1]->curloc.reg], amod.offset, R[amod.base]);
            break;
          case ADDR_REG_REG:
            I("st%sx %s, (%s, %s)", SIZE_SUFFIX(i->size),
              R[i->iops[1]->curloc.reg], R[amod.base], R[amod.offset]);
            break;
          case ADDR_REG_IDX:
            I("st%sx %s, (%s, %s, %d)", SIZE_SUFFIX(i->size),
              R[i->iops[1]->curloc.reg], R[amod.base], R[amod.offset], i->size);
            break;
          }
        }
        break;
      case IR_JP:
        if (b->rpo_next != i->bops[0])
          I("jp .B%d", i->ops[0]->id);
        break;
      case IR_BR: {
        const char *opcodes[] = {"bgt", "ble", "beq", "bne", "blt", "bge"};
        if (i->iops[0]->opc == IR_CONST) {
          if (i->iops[0]->cval) {
            if (b->rpo_next != i->bops[1])
              I("jp .B%d", i->ops[1]->id);
          } else {
            if (b->rpo_next != i->bops[2])
              I("jp .B%d", i->ops[2]->id);
          }
        } else {
          assert(IROPC_ISCMP(i->iops[0]->opc));
          int cond = gen_cmp(i->iops[0]);
          use_i(i->iops[0]);
          if (b->rpo_next != i->bops[1])
            I("%s .B%d", opcodes[cond], i->ops[1]->id);
          if (b->rpo_next != i->bops[2])
            I("%s .B%d", opcodes[cond ^ 1], i->ops[2]->id);
        }
        break;
      }
      case IR_RET:
        if (i->iops[0]->curloc.sclass == LOC_UNALLOC) {
          i->iops[0]->curloc.sclass = LOC_REG;
          i->iops[0]->curloc.reg = RETREG;
          gen_i(i->iops[0]);
        } else {
          gen_mov(RETREG, &i->iops[0]->curloc);
        }
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
    memset(usedreg, 0, sizeof usedreg);
    usedreg[ZREG] = usedreg[SPREG] = usedreg[FPREG] = usedreg[LRREG] =
        LOCKED_REG;
    cur_fun = f;
    maxsaved = SAVEDSTART - 1;
    curstack = f->stacksize = 0;

    // TODO: handle stack args/ struct args
    int argidx = -1;
    for (IRLocal *l = f->locals; l; l = l->next) {
      if (l->is_param)
        argidx++;
      if (!l->numuses)
        continue;
      if (l->obj->ty->size <= WORDSIZE && !l->obj->ty->is_volatile) {
        int r;
        if (l->is_param && f->is_leaf) {
          r = ARGSSTART + argidx;
        } else {
          r = alloc_saved();
        }
        if (r >= 0) {
          l->curloc.sclass = LOC_REG;
          l->curloc.reg = r;
          usedreg[r] = LOCKED_REG;
          continue;
        }
      }
      l->curloc.sclass = LOC_STACK;
      l->curloc.stackoff = alloc_stack(l->obj->ty->size, l->obj->align);
    }
    f->stacksize = align_to(f->stacksize, WORDSIZE);

    char *buf;
    size_t buflen;
    output_file = open_memstream(&buf, &buflen);

    ir_begin_pass((IRValue *) f->entry);
    gen_b(f->entry);

    fclose(output_file);
    output_file = out;

    int save_lr = !f->is_leaf;
    int save_fp = f->stacksize != 0;
    int nsaved = save_lr + save_fp + maxsaved - SAVEDSTART + 1;
    int spdisp = WORDSIZE * nsaved + f->stacksize;

    I("#align 32");
    P("%s:", f->obj->name);
    if (save_lr)
      I("stw lr, %d(sp)", -WORDSIZE);
    if (save_fp)
      I("stw fp, %d(sp)", -(WORDSIZE * (save_lr + 1)));
    for (int i = SAVEDSTART; i <= maxsaved; i++) {
      I("stw %s, %d(sp)", R[i],
        -(WORDSIZE * (save_lr + save_fp + i - SAVEDSTART + 1)));
    }
    if (save_fp)
      I("subi fp, sp, %d", WORDSIZE * nsaved);
    if (spdisp != 0)
      I("subi sp, sp, %d", spdisp);

    int pi = 0;
    for (IRLocal *p = f->params; p; p = p->next, pi++) {
      if (p->curloc.sclass == LOC_REG) {
        gen_movr(p->curloc.reg, ARGSSTART + pi);
      } else if (p->curloc.sclass == LOC_STACK) {
        if (p->curloc.stackoff <= 0)
          continue;
        I("st%s %s, %d(fp)", SIZE_SUFFIX(p->obj->ty->size), R[ARGSSTART + pi],
          -p->curloc.stackoff);
      }
    }

    fwrite(buf, 1, buflen, output_file);
    free(buf);

    if (spdisp != 0)
      I("addi sp, sp, %d", spdisp);
    for (int i = maxsaved; i >= SAVEDSTART; i--) {
      I("ldw %s, %d(sp)", R[i],
        -(WORDSIZE * (save_lr + save_fp + i - SAVEDSTART + 1)));
    }
    if (save_fp)
      I("ldw fp, %d(sp)", -(WORDSIZE * (save_lr + 1)));
    if (save_lr)
      I("ldw lr, %d(sp)", -WORDSIZE);
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
            pos += WORDSIZE;
          } else if (g->ty->size - pos >= WORDSIZE) {
            I("dw %#x", *(uint32_t *) &g->init_data[pos]);
            pos += WORDSIZE;
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
