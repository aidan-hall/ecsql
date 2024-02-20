#ifndef MEMORY_H
#define MEMORY_H

#include <arena.h>
#include <lisp/object.h>
#include <lisp/types.h>

typedef struct {
  Arena space;
  Arena active;
  Arena inactive;
} Memory;

Memory new_lisp_memory(size capacity);

/* Allocate the given number of cells in memory, return its index in memory or
 * -1 on failure. */
size lisp_allocate_cells(struct LispEnv *lisp, size cells);
size lisp_allocate_bytes(struct LispEnv *lisp, size count);
Object lisp_cons(struct LispEnv *lisp, Object car, Object cdr);
static inline Object lisp_make_vector(struct LispEnv *lisp, i32 length) {
  return OBJ_BOX_INDEX(length == 0 ? 0 : lisp_allocate_cells(lisp, length),
                       length, VECTOR);
}

#endif
