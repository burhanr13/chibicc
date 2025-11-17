#pragma once

#define BPASS_BEGIN(name, ...)                                                 \
  void irpass_##name##2(IRBlock * b __VA_OPT__(, ) __VA_ARGS__) {              \
    if (b->hdr.visited)                                                        \
      return;                                                                  \
    b->hdr.visited = true;                                                     \
    b->rpo_next = NULL;

#define BPASS_END(name, ...)                                                   \
  }                                                                            \
  void irpass_##name(IRFunction *f) {                                          \
    ir_begin_pass((IRValue *) f->entry);                                       \
    PASS_REC(name, f->entry, __VA_ARGS__);                                     \
  }

#define IPASS_BEGIN(name, ...)                                                 \
  void irpass_##name##2(IRInstr * i __VA_OPT__(, ) __VA_ARGS__) {              \
    if (i->hdr.visited)                                                        \
      return;                                                                  \
    i->hdr.visited = true;

#define IPASS_BBEGIN(name, ...)                                                \
  }                                                                            \
  void irpass_##name(IRFunction *f) {                                          \
    ir_begin_pass((IRValue *) f->entry);                                       \
    __VA_ARGS__;                                                               \
    for (IRBlock *b = f->entry; b; b = b->rpo_next) {                          \
      b->hdr.visited = true;

#define IPASS_END(name, ...)                                                   \
  IRB_ITER(i, b) {                                                             \
    PASS_REC(name, i, __VA_ARGS__);                                            \
  }                                                                            \
  }                                                                            \
  }

#define PASS_REC(name, v, ...) irpass_##name##2(v __VA_OPT__(, ) __VA_ARGS__)

#define BPASS_REC_ALL(name, ...)                                               \
  switch (IRB_LAST(b)->opc) {                                                  \
  case IR_JP: PASS_REC(name, IRB_LAST(b)->bops[0], __VA_ARGS__); break;        \
  case IR_BR:                                                                  \
    PASS_REC(name, IRB_LAST(b)->bops[1], __VA_ARGS__);                         \
    PASS_REC(name, IRB_LAST(b)->bops[2], __VA_ARGS__);                         \
    break;                                                                     \
  }

#define IPASS_REC_ALL(name, ...)                                               \
  for (int o = 0; o < i->numops; o++) {                                        \
    if (i->ops[o]->vt == IRV_INSTR)                                            \
      PASS_REC(name, i->iops[o], __VA_ARGS__);                                 \
  }

#define IR_RUNPASS(pass, p, ...)                                               \
  for (IRFunction *f = (p)->funs; f; f = f->next) {                            \
    __VA_ARGS__;                                                               \
    pass(f);                                                                   \
  }
