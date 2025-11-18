#include "ir.h"
#include "irpass.h"

bool irinstr_has_side_effect(IRInstr *i) {
  return IROPC_ISTERM(i->opc) || i->opc == IR_RET || i->opc == IR_STORE ||
         i->opc == IR_MEMCPY || i->opc == IR_CALL || i->opc == IR_RET ||
         (i->opc == IR_LOAD && i->is_volatile);
}

bool irinstr_isdead(IRInstr *i) {
  if (irinstr_has_side_effect(i))
    return false;
  return i->hdr.numuses == 0;
}

BPASS_BEGIN(opt_cfg)
  if (b->is_exit)
    return;
  bool found_term = false;
  IRB_ITER(i, b) {
    if (found_term || irinstr_isdead(i)) {
      ir_erase_instr(i);
    } else if (IROPC_ISTERM(i->opc))
      found_term = true;
  }
  assert(found_term && !IRB_ISEMPTY(b));

  if (IRB_LAST(b)->opc == IR_BR && IRB_LAST(b)->iops[0]->opc == IR_CONST) {
    if (IRB_LAST(b)->iops[0]->cval) {
      ir_replace((IRValue *) IRB_LAST(b),
                 (IRValue *) ir_jump(IRB_LAST(b)->bops[1]));
    } else {
      ir_replace((IRValue *) IRB_LAST(b),
                 (IRValue *) ir_jump(IRB_LAST(b)->bops[2]));
    }
  }
  if (IRB_LAST(b)->opc == IR_BR &&
      IRB_LAST(b)->bops[1] == IRB_LAST(b)->bops[2]) {
    ir_replace((IRValue *) IRB_LAST(b),
               (IRValue *) ir_jump(IRB_LAST(b)->bops[1]));
  }

  BPASS_REC_ALL(opt_cfg);

  if (IRB_LAST(b)->opc == IR_JP && IRB_LAST(b)->bops[0]->hdr.numuses == 1 &&
      !IRB_LAST(b)->bops[0]->is_exit) {
    ir_merge_block(b, IRB_LAST(b)->bops[0]);
  }

  if (!b->is_entry && IRB_FIRST(b) == IRB_LAST(b) &&
      IRB_LAST(b)->opc == IR_JP) {
    bool post_loop = b->is_post_loop;
    b = (IRBlock *) ir_replace((IRValue *) b, (IRValue *) IRB_LAST(b)->bops[0]);
    if (post_loop)
      b->is_post_loop = true;
  }

BPASS_END(opt_cfg)

#define LHS(i) ((i)->iops[0])
#define RHS(i) ((i)->iops[1])
#define OPT(i2) (i = (IRInstr *) ir_replace((IRValue *) i, (IRValue *) (i2)))

int ilog2(int n) {
  if (n == 0)
    return -1;
  extern int __builtin_ctz(unsigned int);
  int res = __builtin_ctz(n);
  return n == 1 << res ? res : -1;
}

IPASS_BEGIN(constant_fold)
  IPASS_REC_ALL(constant_fold);

  if (IROPC_ISUNARY(i->opc) && LHS(i)->opc == IR_CONST) {
    switch (i->opc) {
    case IR_NOT: OPT(ir_const(~LHS(i)->cval)); break;
    case IR_NEG: OPT(ir_const(-LHS(i)->cval)); break;
    case IR_UEXT:
      switch (i->size) {
      case 1: OPT(ir_const((uint8_t) LHS(i)->cval)); break;
      case 2: OPT(ir_const((uint16_t) LHS(i)->cval)); break;
      case 4: OPT(ir_const((uint32_t) LHS(i)->cval)); break;
      }
      break;
    case IR_SEXT:
      switch (i->size) {
      case 1: OPT(ir_const((int8_t) LHS(i)->cval)); break;
      case 2: OPT(ir_const((int16_t) LHS(i)->cval)); break;
      case 4: OPT(ir_const((int32_t) LHS(i)->cval)); break;
      }
      break;
    }
    return;
  }

  if (IROPC_ISBINARY(i->opc) && LHS(i)->opc == IR_CONST &&
      RHS(i)->opc == IR_CONST) {
    switch (i->opc) {
    case IR_ADD: OPT(ir_const(LHS(i)->cval + RHS(i)->cval)); break;
    case IR_SUB: OPT(ir_const(LHS(i)->cval - RHS(i)->cval)); break;
    case IR_MUL: OPT(ir_const(LHS(i)->cval * RHS(i)->cval)); break;
    case IR_SDIV:
      OPT(ir_const((int32_t) LHS(i)->cval / (int32_t) RHS(i)->cval));
      break;
    case IR_SMOD:
      OPT(ir_const((int32_t) LHS(i)->cval % (int32_t) RHS(i)->cval));
      break;
    case IR_UDIV:
      OPT(ir_const((uint32_t) LHS(i)->cval / (uint32_t) RHS(i)->cval));
      break;
    case IR_UMOD:
      OPT(ir_const((uint32_t) LHS(i)->cval % (uint32_t) RHS(i)->cval));
      break;
    case IR_AND: OPT(ir_const(LHS(i)->cval & RHS(i)->cval)); break;
    case IR_OR: OPT(ir_const(LHS(i)->cval | RHS(i)->cval)); break;
    case IR_XOR: OPT(ir_const(LHS(i)->cval ^ RHS(i)->cval)); break;
    case IR_SLL: OPT(ir_const(LHS(i)->cval << RHS(i)->cval)); break;
    case IR_SRL:
      OPT(ir_const((uint32_t) LHS(i)->cval >> (uint32_t) RHS(i)->cval));
      break;
    case IR_SRA:
      OPT(ir_const((int32_t) LHS(i)->cval >> (int32_t) RHS(i)->cval));
      break;
    case IR_EQ:
      OPT(ir_const((uint32_t) LHS(i)->cval == (uint32_t) RHS(i)->cval));
      break;
    case IR_NE:
      OPT(ir_const((uint32_t) LHS(i)->cval != (uint32_t) RHS(i)->cval));
      break;
    case IR_ULT:
      OPT(ir_const((uint32_t) LHS(i)->cval < (uint32_t) RHS(i)->cval));
      break;
    case IR_ULE:
      OPT(ir_const((uint32_t) LHS(i)->cval <= (uint32_t) RHS(i)->cval));
      break;
    case IR_SLT:
      OPT(ir_const((int32_t) LHS(i)->cval < (int32_t) RHS(i)->cval));
      break;
    case IR_SLE:
      OPT(ir_const((int32_t) LHS(i)->cval <= (int32_t) RHS(i)->cval));
      break;
    }
    return;
  }

  if (IROPC_ISCOMM(i->opc) && LHS(i)->opc == IR_CONST) {
    OPT(ir_binary(i->opc, RHS(i), LHS(i)));
  } else if (i->opc == IR_SUB && LHS(i)->opc == IR_CONST) {
    OPT(ir_binary(IR_ADD, ir_unary(IR_NEG, RHS(i)), LHS(i)));
  }

  if (IROPC_ISBINARY(i->opc) && RHS(i)->opc == IR_CONST) {
    switch (i->opc) {
    case IR_ADD:
    case IR_SUB:
      if ((LHS(i)->opc == IR_ADD || LHS(i)->opc == IR_SUB) &&
          RHS(LHS(i))->opc == IR_CONST) {
        if (LHS(i)->opc == i->opc) {
          OPT(ir_binary(i->opc, LHS(LHS(i)),
                        ir_const(RHS(LHS(i))->cval + RHS(i)->cval)));
        } else {
          OPT(ir_binary(i->opc, LHS(LHS(i)),
                        ir_const(RHS(i)->cval - RHS(LHS(i))->cval)));
        }
      }
      if (RHS(i)->cval == 0) {
        OPT(LHS(i));
      }
      break;
    case IR_AND:
    case IR_OR:
    case IR_XOR:
      if (LHS(i)->opc == i->opc && RHS(LHS(i))->opc == IR_CONST) {
        uint64_t res = i->opc == IR_AND  ? RHS(LHS(i))->cval & RHS(i)->cval
                       : i->opc == IR_OR ? RHS(LHS(i))->cval | RHS(i)->cval
                                         : RHS(LHS(i))->cval ^ RHS(i)->cval;
        OPT(ir_binary(i->opc, LHS(LHS(i)), ir_const(res)));
      }
      if (RHS(i)->cval == 0) {
        if (i->opc == IR_AND) {
          OPT(ir_const(0));
        } else {
          OPT(LHS(i));
        }
      }
      break;
    case IR_MUL:
    case IR_UDIV:
    case IR_SDIV: {
      if (LHS(i)->opc == i->opc && RHS(LHS(i))->opc == IR_CONST) {
        OPT(ir_binary(i->opc, LHS(LHS(i)),
                      ir_const(RHS(LHS(i))->cval * RHS(i)->cval)));
      }
      int sh = ilog2(RHS(i)->cval);
      if (sh != -1) {
        IROpc opc = i->opc == IR_MUL    ? IR_SLL
                    : i->opc == IR_UDIV ? IR_SRL
                                        : IR_SRA;
        if (sh == 0) {
          OPT(LHS(i));
        } else {
          OPT(ir_binary(opc, LHS(i), ir_const(sh)));
        }
      } else if (RHS(i)->cval == 0) {
        OPT(ir_const(0));
      }
      break;
    }
    case IR_UMOD:
    case IR_SMOD: {
      int sh = ilog2(RHS(i)->cval);
      if (sh != -1) {
        if (sh == 0) {
          OPT(ir_const(0));
        } else {
          OPT(ir_binary(IR_AND, LHS(i), ir_const((1 << sh) - 1)));
        }
      }
      break;
    }
    case IR_SLL:
    case IR_SRL:
    case IR_SRA: {
      if (LHS(i)->opc == i->opc && RHS(LHS(i))->opc == IR_CONST) {
        int combined = RHS(LHS(i))->cval + RHS(i)->cval;
        if (combined < 32)
          OPT(ir_binary(i->opc, LHS(LHS(i)), ir_const(combined)));
      }
      if (RHS(i)->cval == 0)
        OPT(LHS(i));
      break;
    }
    }
  }

  if (IROPC_ISBINARY(i->opc) && LHS(i)->opc == IR_CONST && LHS(i)->cval == 0) {
    switch (i->opc) {
    case IR_UDIV:
    case IR_SDIV:
    case IR_UMOD:
    case IR_SMOD:
    case IR_SLL:
    case IR_SRL:
    case IR_SRA: OPT(ir_const(0));
    }
  }

  IPASS_BBEGIN(constant_fold)
IPASS_END(constant_fold)
