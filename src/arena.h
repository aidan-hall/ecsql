#ifndef ARENA_H
#define ARENA_H
/* Credit: https://nullprogram.com/blog/2023/09/27/ */

#include "common.h"
#include <stdlib.h>

typedef struct {
  char *begin;
  char *end;
} Arena;

#if defined(__GNUC__) || defined(__clang__)
__attribute((malloc, alloc_size(2, 4), alloc_align(3)))
#endif
void *
allocate(Arena *a, ptrdiff_t size, ptrdiff_t align, ptrdiff_t count);

Arena new_arena(ptrdiff_t capacity);

#define new(a, t, n) (t *)alloc(a, sizeof(t), _Alignof(t), n)

#endif
