#ifndef LISP_H
#define LISP_H

#include "arena.h"
#include "common.h"
#include "khash.h"
#include <libgccjit.h>
#include <stddef.h>
#include <stdlib.h>

typedef u64 Object;

KHASH_MAP_INIT_STR(sym_name, Object)

typedef struct {
  Arena space;
  Arena active;
  Arena inactive;
} Memory;

/* This higher-order macro performs an operation for each of these "key"
 * symbols.  */
#define LISP_KEYSYMS(F)                                                        \
  F(nil)                                                                       \
  F(t)                                                                         \
  F(quote)

typedef struct {
  gcc_jit_context *jit;
  Memory memory;
  /* char* → Object of strings stored in 'memory'. */
  khash_t(sym_name) * symbols;
  /* Object(symbol) → gcc_jit_function* */
  /* hash_t *functions; */
  /* Object(symbol) → gcc_jit_lvalue* of global variables. */
  /* hash_t *globals; */
  struct {
#define DECL_KEYSYM(K) Object K;
    LISP_KEYSYMS(DECL_KEYSYM)
#undef DECL_KEYSYM
  } keysyms;
} LispEnv;

Memory new_lisp_memory(size capacity);

LispEnv new_lisp_environment();

Object lisp_store_string(LispEnv *lisp, s8 string);
/* Return the canonical symbol whose name is string 'name'. */
Object lisp_intern(LispEnv *lisp, s8 name);
/* Cons up a Lisp list. The last argument *must* have a NIL type tag. */
Object lisp_list(LispEnv *lisp, ...);

/* Allocate the given number of cells in memory, return its index in memory or
 * -1 on failure. */
size lisp_allocate_cells(LispEnv *lisp, size cells);
size lisp_allocate_bytes(LispEnv *lisp, size count);

/* Get the Lisp cell at the given index, with no error handling. */
static inline Object *lisp_cell_at(LispEnv *lisp, size index) {
  return &((Object *)lisp->memory.space.begin)[index];
}

#define OBJ_MASK (0xffffffffffffffe0)
#define OBJ_TAG_LENGTH (5)

#define LISP_INDEX_METADATA_LENGTH (16)
#define LISP_INDEX_OFFSET (LISP_INDEX_METADATA_LENGTH + OBJ_TAG_LENGTH)
#define LISP_INDEX_METADATA_MASK (0xffff << OBJ_TAG_LENGTH)
#define LISP_MAX_STRING_LENGTH ((2 << LISP_INDEX_METADATA_LENGTH) - 2)

/* 5-bit object type tag: allows up to 32 built-in types */
enum ObjectTag : Object {
  OBJ_NIL_TAG = 0,
  OBJ_PAIR_TAG,
  OBJ_UNDEFINED_TAG,
  OBJ_SYMBOL_TAG,
  OBJ_STRING_TAG,
  OBJ_INT_TAG,
  OBJ_FLOAT_TAG,
  OBJ_CLOSURE_TAG,
  OBJ_PRIMITIVE_TAG
};

/* Filter out just the type tag of an object */
#define OBJ_TAG_NAME(NAME) (OBJ_##NAME##_TAG)
#define OBJ_TYPE(OBJ) (OBJ & ~OBJ_MASK)
/* static inline Object OBJ_BOX(u64 value, u64 tag) { */
/*   return (Object)((value << OBJ_TAG_LENGTH) | OBJ_TAG_NAME(tag)); */
/* } */
#define OBJ_BOX(VALUE, TAG)                                                    \
  ((Object)(((VALUE) << (OBJ_TAG_LENGTH)) | OBJ_TAG_NAME(TAG)))
/* static inline Object OBJ_BOX_INDEX(u64 index, u16 metadata, u64 tag) { */
/*   return OBJ_BOX((index << LISP_INDEX_METADATA_LENGTH) | metadata, tag); */
/* } */
#define OBJ_BOX_INDEX(INDEX, METADATA, TAG)                                    \
  (OBJ_BOX(((INDEX) << LISP_INDEX_METADATA_LENGTH) | (METADATA), TAG))
#define OBJ_UNBOX(BOX) (BOX >> OBJ_TAG_LENGTH)
#define OBJ_UNBOX_INDEX(BOX) (BOX >> LISP_INDEX_OFFSET)
#define OBJ_UNBOX_METADATA(BOX)                                                \
  ((BOX & LISP_INDEX_METADATA_MASK) >> LISP_INDEX_METADATA_LENGTH)
#define OBJ_REINTERPRET(OBJ, NEWTAG) ((OBJ & OBJ_MASK) | OBJ_TAG_NAME(NEWTAG))

void wrong(const char *message);
#define LISP_ASSERT_TYPE(OBJ, TYPE)                                            \
  do {                                                                         \
    if (OBJ_TYPE(OBJ) != OBJ_##TYPE##_TAG) {                                   \
      wrong("FATAL: Wrong type of " #OBJ ": expected" #TYPE);                  \
    }                                                                          \
  } while (0)

#define EQ(X, Y) ((X) == (Y))

#define LISP_CAR_INDEX (0)
#define LISP_CDR_INDEX (1)
#define LISP_CAR(LISP, PAIR)                                                   \
  (*lisp_cell_at(LISP, OBJ_UNBOX_INDEX(PAIR) + LISP_CAR_INDEX))
#define LISP_CDR(LISP, PAIR)                                                   \
  (*lisp_cell_at(LISP, OBJ_UNBOX_INDEX(PAIR) + LISP_CDR_INDEX))

Object lisp_cons(LispEnv *lisp, Object car, Object cdr);
void lisp_print(LispEnv *lisp, Object object, FILE *stream, int depth);

#endif
