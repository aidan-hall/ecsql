#include "arena.h"
#include "lisp.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

Memory new_lisp_memory(size capacity) {
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

size lisp_allocate_cells(LispEnv *lisp, size cells) {
  Object *start = ALLOC(&lisp->memory.active, Object, cells);

  if (start == nullptr) {
    wrong("allocation failure");
  }

  return start - (Object *)lisp->memory.space.begin;
}

size lisp_allocate_bytes(LispEnv *lisp, size count) {
  return lisp_allocate_cells(lisp, (count / sizeof(Object)) + 1);
  /* Object *start = */
  /*   arena_allocate(&lisp->memory.active, sizeof(u8), sizeof(Object), count); */

  /* if (start == nullptr) { */
  /*   wrong("allocation failure"); */
  /* } */
  
  /* return start - (Object *)lisp->memory.space.begin; */
}
