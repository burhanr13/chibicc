#pragma once

#define BPASS_BEGIN(name, ...)                                                 \
  void irpass_##name##2(IRBlock * b __VA_OPT__(, ) __VA_ARGS__) {              \
    if (b->hdr.visited)                                                        \
      return;                                                                  \
    b->hdr.visited = true;

#define BPASS_END(name, ...)                                                   \
  }                                                                            \
  void irpass_##name(IRFunction *f) {                                          \
    ir_begin_pass((IRValue *) f->entry);                                       \
    irpass_##name##2(f->entry __VA_OPT__(, ) __VA_ARGS__);                     \
  }

#define BPASS_REC(name, b, ...) irpass_##name##2(b __VA_OPT__(, ) __VA_ARGS__)
