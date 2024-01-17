#include "memory.h"
#include "arena.h"
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

Memory new_lisp_memory(ptrdiff_t capacity) {
  /* Allocate enough space for the active and inactive stores. */
  Memory memory;
  memory.space = new_arena(capacity * 2);
  if (memory.space.end == nullptr) {
    fputs("Failed to allocate memory for Lisp.", stderr);
    exit(EXIT_FAILURE);
  }
  memory.active = (Arena){memory.space.begin, memory.space.begin + capacity};
  memory.inactive = (Arena){memory.active.begin, memory.space.end};
  
  return memory;
}
