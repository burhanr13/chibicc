#pragma once

#define BPASS_BEGIN(name, ...)                                                 \
  void irpass_##name##2(IRBlock * b __VA_OPT__(, ) __VA_ARGS__) {              \
    if (b->hdr.mark)                                                           \
      return;                                                                  \
    b->hdr.mark = true;                                                        \
    b->rpo_next = b->idom = NULL;

#define BPASS_END(name, ...)                                                   \
  }                                                                            \
  void irpass_##name(IRFunction *f) {                                          \
    PASS_REC(name, f->entry, __VA_ARGS__);                                     \
    ir_reset_marks((IRValue *) f->entry);                                      \
  }

#define IPASS_BEGIN(name, ...)                                                 \
  void irpass_##name##2(IRInstr * i __VA_OPT__(, ) __VA_ARGS__) {              \
    if (i->hdr.mark)                                                           \
      return;                                                                  \
    i->hdr.mark = true;                                                        \

#define IPASS_BBEGIN(name, ...)                                                \
  }                                                                            \
  void irpass_##name(IRFunction *f) {                                          \
    __VA_ARGS__;                                                               \
    for (IRBlock *b = f->entry; b; b = b->rpo_next) {                          \
      b->hdr.mark = true;

#define IPASS_END(name, ...)                                                   \
  IRB_ITER(i, b) {                                                             \
    PASS_REC(name, i, __VA_ARGS__);                                            \
  }                                                                            \
  }                                                                            \
  ir_reset_marks((IRValue *) f->entry);                                        \
  }

#define PASS_REC(name, v, ...) irpass_##name##2(v __VA_OPT__(, ) __VA_ARGS__)

#define BPASS_REC_ALL(name, ...)                                               \
  IRB_SUCCS(it, b) PASS_REC(name, it.b, __VA_ARGS__);

#define IPASS_REC_ALL(name, ...)                                               \
  IRI_ITERI(it, i) PASS_REC(name, it.i, __VA_ARGS__);

#define IR_RUNPASS(pass, p, ...)                                               \
  for (IRFunction *f = (p)->funs; f; f = f->next) {                            \
    __VA_ARGS__;                                                               \
    pass(f);                                                                   \
  }
