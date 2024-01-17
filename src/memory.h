#ifndef MEMORY_H
#define MEMORY_H

#include "arena.h"
#include "lisp.h"
#include <stdlib.h>

typedef struct {
  Arena space;
  Arena active;
  Arena inactive;
} Memory;

Memory new_lisp_memory(ptrdiff_t capacity);

/* Object allocate_cells(Memory *memory, ) */

#endif
