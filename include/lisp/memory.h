#ifndef MEMORY_H
#define MEMORY_H

#include <arena.h>
#include <lisp/object.h>
#include <lisp/types.h>

/* Lisp Memory */
typedef struct {
  /* The start of the Lisp memory. */
  char *base;
  /* The unused portion of Lisp memory. */
  Arena active;
} Memory;

Memory new_lisp_memory(size capacity);

/* Allocate the given number of cells in memory, return their index in memory or
 * NOT_PRESENT on failure. */
size lisp_allocate_cells(struct LispEnv *lisp, size cells);
/* Allocate 'count' bytes in Lisp memory, return their index in memory or
 * NOT_PRESENT upon failure */
size lisp_allocate_bytes(struct LispEnv *lisp, size count);
/* Cons: Allocate two contiguous cells, for the car and cdr. */
Object lisp_cons(struct LispEnv *lisp, Object car, Object cdr);
static inline Object lisp_make_vector(struct LispEnv *lisp, i32 length) {
  return OBJ_BOX_INDEX(length == 0 ? 0 : lisp_allocate_cells(lisp, length),
                       length, VECTOR);
}

#endif
