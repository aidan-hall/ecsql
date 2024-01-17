#include "arena.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* Credit: https://nullprogram.com/blog/2023/09/27/ */

void *arena_allocate(Arena *a, ptrdiff_t size, ptrdiff_t align, ptrdiff_t count) {
  /* How far forward to move so allocation is aligned. */
  ptrdiff_t padding = -(uintptr_t)a->begin & (align - 1);
  ptrdiff_t available = a->end - a->begin - padding;
  if (available < 0 || count > available / size) {
    fputs("ARENA OVERFLOW!\n", stderr);
    abort(); // one possible out-of-memory policy
  }
  void *p = a->begin + padding;
  a->begin += padding + count * size;
  /* Zero-initialise all allocations. */
  return memset(p, 0, count * size);
}

Arena new_arena(ptrdiff_t capacity) {
  Arena a = {0};
  a.begin = malloc(capacity);
  a.end = a.begin ? a.begin + capacity : nullptr;
  return a;
}
