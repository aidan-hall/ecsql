#include "arena.h"
#include "lisp.h"
#include "object.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

Memory new_lisp_memory(size capacity) {
  /* Allocate enough space for the active and inactive stores. */
  Memory memory;
  memory.space = new_arena(capacity * 2);
  if (memory.space.end == NULL) {
    fputs("Failed to allocate memory for Lisp.", stderr);
    exit(EXIT_FAILURE);
  }
  memory.active = (Arena){memory.space.begin, memory.space.begin + capacity};
  memory.inactive = (Arena){memory.active.begin, memory.space.end};

  return memory;
}

size lisp_allocate_cells(struct LispEnv *lisp, size cells) {
  if (cells <= 0) {
    WRONG("Attempt to allocate a non-positive number of cells.", OBJ_BOX(cells, INT));
    return OBJ_UNDEFINED_TAG;
  }
  
  Object *start = ALLOC(&lisp->memory.active, Object, cells);

  if (start == NULL) {
    WRONG("allocation failure");
    return OBJ_UNDEFINED_TAG;
  }

  return start - (Object *)lisp->memory.space.begin;
}

size lisp_allocate_bytes(struct LispEnv *lisp, size count) {
  return lisp_allocate_cells(lisp, (count / sizeof(Object)) + 1);
}

Object lisp_cons(LispEnv *lisp, Object car, Object cdr) {
  size location = lisp_allocate_cells(lisp, 2);
  Object *data = lisp_cell_at(lisp, location);
  data[LISP_CAR_INDEX] = car;
  data[LISP_CDR_INDEX] = cdr;

  /* The 2 allows implicit conversion to an array type, if we implement one. */
  return OBJ_BOX_INDEX(location, 2, PAIR);
}
