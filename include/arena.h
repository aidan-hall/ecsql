#ifndef ARENA_H
#define ARENA_H
/* Credit: https://nullprogram.com/blog/2023/09/27/
 * A simple arena allocator.
 */

#include "common.h"

typedef struct {
  char *begin;
  char *end;
} Arena;

#if defined(__GNUC__) || defined(__clang__)
__attribute((malloc, alloc_size(2, 4), alloc_align(3)))
#endif
/* Allocate count elements of size bytes with align bytes of alignment from Arena a. */
void *
arena_allocate(Arena *a, ptrdiff_t size, ptrdiff_t align, ptrdiff_t count);

/* Create a new arena with malloc. */
Arena new_arena(ptrdiff_t capacity);

/* Allocate an array t[n] from Arena a. */
#define ALLOC(a, t, n) (t *)arena_allocate(a, sizeof(t), _Alignof(t), n)

#endif
