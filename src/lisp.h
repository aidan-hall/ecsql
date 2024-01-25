#ifndef LISP_H
#define LISP_H

#include "arena.h"
#include "common.h"
#include "khash.h"
#include <libgccjit.h>
#include <stddef.h>
#include <stdlib.h>
#include <setjmp.h>

typedef u64 Object;

KHASH_MAP_INIT_STR(sym_name, Object);
KHASH_MAP_INIT_INT64(var_syms, Object);
typedef khash_t(var_syms) SymbolTable;

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
  F(quote)                                                                     \
  F(quasiquote)                                                                \
  F(unquote)                                                                   \
  F(splice)                                                                    \
  F(function)                                                           \
  F(stdin)                                                                     \
  F(stdout)                                                                    \
  F(stderr)                                                                    \
  F(or)                                                                        \
  F(cond)                                                                      \
  F(and)                                                                       \
  F(progn)                                                                     \
  F(define)                                                                    \
  F(setq)                                                                      \
  F(lambda)                                                             \
  F(file)                                                               \
  F(string)                                                             \
  F(pair)                                                               \
  F(i32)                                                                \
  F(f32)                                                                \
  F(undefined)                                                          \
  F(symbol)                                                             \
  F(primitive)                                                          \
  F(closure)                                                            \
  F(character)                                                          \
  F(defun)

struct LispEnv;
typedef Object (*ReaderMacro)(struct LispEnv *lisp, FILE *stream);
typedef Object (*PrimitiveFunction)(struct LispEnv *lisp, Object arguments);
typedef struct {
  PrimitiveFunction fn;
  /* t if type-generic/variadic, list for fixed args */
  Object argument_types;
} InterpreterPrimitive;
KHASH_MAP_INIT_INT64(primitives, InterpreterPrimitive);

#define LISP_MAX_OPEN_STREAMS (16)

typedef struct LispEnv {
  struct {
    gcc_jit_context *ctxt;
    gcc_jit_type *object_type;
  } jit;
  Memory memory;
  /* char* → Object of strings stored in 'memory'. */
  khash_t(sym_name) * symbols;
  /* Object(symbol) → gcc_jit_function* */
  /* hash_t *functions; */
  khash_t(primitives) * primitive_functions;
  /* Object(symbol) → gcc_jit_lvalue* of global variables(?). */
  SymbolTable *globals;
  SymbolTable *functions;
  int quasiquote_level;
  FILE *open_streams[LISP_MAX_OPEN_STREAMS];
  /* Each reader macro is installed to an ASCII character index in here. */
  ReaderMacro reader_macros[128];
  struct {
#define DECL_KEYSYM(K) Object K;
    LISP_KEYSYMS(DECL_KEYSYM)
#undef DECL_KEYSYM
    /* The Lisp symbols we want for these are also C keywords so they need
     * special treatment. */
    Object if_k;    /* if */
    Object while_k; /* while */
  } keysyms;
  jmp_buf error_loc;
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
  /* Atomic objects (evaluate to themselves) */
  OBJ_NIL_TAG = 0,
  OBJ_STRING_TAG,
  OBJ_CHAR_TAG,
  OBJ_INT_TAG,
  OBJ_FLOAT_TAG,
  OBJ_FILE_PTR_TAG,
  /* Non-atomic objects */
  OBJ_UNDEFINED_TAG,
  OBJ_SYMBOL_TAG,
  OBJ_PAIR_TAG,
  OBJ_PRIMITIVE_TAG,
  OBJ_CLOSURE_TAG
};

/* Filter out just the type tag of an object */
#define OBJ_TAG_NAME(NAME) (OBJ_##NAME##_TAG)
#define OBJ_TYPE(OBJ) (OBJ & ~OBJ_MASK)
/* static inline Object OBJ_BOX(u64 value, u64 tag) { */
/*   return (Object)((value << OBJ_TAG_LENGTH) | OBJ_TAG_NAME(tag)); */
/* } */
#define OBJ_BOX_RAWTAG(VALUE, RAWTAG)                                          \
  ((Object)(((VALUE) << (OBJ_TAG_LENGTH)) | (RAWTAG)))
#define OBJ_BOX(VALUE, TAG) OBJ_BOX_RAWTAG(VALUE, OBJ_TAG_NAME(TAG))
/* static inline Object OBJ_BOX_INDEX(u64 index, u16 metadata, u64 tag) { */
/*   return OBJ_BOX((index << LISP_INDEX_METADATA_LENGTH) | metadata, tag); */
/* } */
#define OBJ_BOX_INDEX(INDEX, METADATA, TAG)                                    \
  (OBJ_BOX(((INDEX) << LISP_INDEX_METADATA_LENGTH) | (METADATA), TAG))
#define OBJ_UNBOX(BOX) (BOX >> OBJ_TAG_LENGTH)
#define OBJ_UNBOX_INDEX(BOX) (BOX >> LISP_INDEX_OFFSET)
#define OBJ_UNBOX_METADATA(BOX)                                                \
  ((BOX & LISP_INDEX_METADATA_MASK) >> OBJ_TAG_LENGTH)
#define OBJ_REINTERPRET_RAWTAG(OBJ, NEWTAG) ((OBJ & OBJ_MASK) | (NEWTAG))
#define OBJ_REINTERPRET(OBJ, NEWTAG)                                           \
  OBJ_REINTERPRET_RAWTAG(OBJ, OBJ_TAG_NAME(NEWTAG))

static inline float lisp_unbox_float(Object box) {
  if (OBJ_TYPE(box) == OBJ_FLOAT_TAG) {
    u32 val_bits = (u32)OBJ_UNBOX(box);
    return *(float *)&val_bits;
  } else if (OBJ_TYPE(box) == OBJ_INT_TAG) {
    return (float)(i32)OBJ_UNBOX(box);
  } else {
    return OBJ_UNDEFINED_TAG;
  }
}

void wrong(LispEnv *lisp, const char *message, Object arg);
#define WRONG2(MESSAGE, ARG)                                            \
  do {                                                                  \
    wrong(lisp, MESSAGE, ARG);                                          \
  } while (0)
#define WRONG1(MESSAGE) WRONG2(MESSAGE, OBJ_NIL_TAG)
#define WRONGX(a, b, c, ...) c
#define WRONG(...) WRONGX(__VA_ARGS__, WRONG2, WRONG1)(__VA_ARGS__)
#define LISP_ASSERT_TYPE(OBJ, TYPE)                                            \
  do {                                                                         \
    if (OBJ_TYPE(OBJ) != OBJ_##TYPE##_TAG) {                                   \
      WRONG("FATAL: Wrong type of " #OBJ ": expected " #TYPE ", got",   \
            lisp_type_of(lisp, OBJ));                                  \
      exit(1); /* Should be unreachable */                              \
    }                                                                          \
  } while (0)

#define EQ(X, Y) ((X) == (Y))

#define LISP_CAR_INDEX (0)
#define LISP_CDR_INDEX (1)
#define LISP_CAR_PLACE(LISP, PAIR) (lisp_cell_at(LISP, OBJ_UNBOX_INDEX(PAIR) + LISP_CAR_INDEX))
#define LISP_CDR_PLACE(LISP, PAIR) (lisp_cell_at(LISP, OBJ_UNBOX_INDEX(PAIR) + LISP_CDR_INDEX))
#define LISP_CAR(LISP, PAIR) (*LISP_CAR_PLACE(LISP, PAIR))
#define LISP_CDR(LISP, PAIR) (*LISP_CDR_PLACE(LISP, PAIR))

static inline bool lisp_true(Object value) {
  static_assert(!OBJ_NIL_TAG, "Lisp and C have the same truthy semantics.");
  return value;
}

static inline bool lisp_false(Object value) { return !lisp_true(value); }

static inline Object lisp_bool(LispEnv *lisp, bool value) {
  return value ? lisp->keysyms.t : OBJ_NIL_TAG;
}

Object lisp_cons(LispEnv *lisp, Object car, Object cdr);
void lisp_print(LispEnv *lisp, Object object, FILE *stream);
Object lisp_bind(LispEnv *lisp, Object parameters, Object arguments,
                 Object context);
Object lisp_evaluate(LispEnv *lisp, Object expression, Object context);
Object lisp_eval(LispEnv *lisp, Object expression);
Object lisp_apply(LispEnv *lisp, Object function, Object arguments);
Object lisp_type_of(LispEnv *lisp, Object obj);

#endif
