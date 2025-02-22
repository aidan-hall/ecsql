#ifndef LISP_H
#define LISP_H

#include <assert.h>
#include <common.h>
#include <ecs/ecs.h>
#include <klib/khash.h>
#include <lisp/memory.h>
#include <lisp/types.h>
#include <setjmp.h>
#include <stddef.h>
#include <stdlib.h>
#include <threads.h>

/* (Arbitrary constant) */
#define LISP_MAX_OPEN_STREAMS (16)

/* This higher-order macro applies 'F' to each of these "key" symbol names.  */
#define LISP_KEYSYMS(F)                                                        \
  F(nil)                                                                       \
  F(t)                                                                         \
  F(quote)                                                                     \
  F(quasiquote)                                                                \
  F(unquote)                                                                   \
  F(splice)                                                                    \
  F(stdin)                                                                     \
  F(stdout)                                                                    \
  F(stderr)                                                                    \
  F(or)                                                                        \
  F(opt)                                                                       \
  F(cond)                                                                      \
  F(and)                                                                       \
  F(progn)                                                                     \
  F(setq)                                                                      \
  F(lambda)                                                                    \
  F(file)                                                                      \
  F(string)                                                                    \
  F(pair)                                                                      \
  F(i32)                                                                       \
  F(f32)                                                                       \
  F(undefined)                                                                 \
  F(symbol)                                                                    \
  F(primitive)                                                                 \
  F(closure)                                                                   \
  F(character)                                                                 \
  F(defname)                                                                   \
  F(global)                                                                    \
  F(function)                                                                  \
  F(macro)                                                                     \
  F(eof)                                                                       \
  F(vector)                                                                    \
  F(entity)                                                                    \
  F(relation)                                                                  \
  F(left)                                                                      \
  F(right)

/* The Lisp Environment */
typedef struct LispEnv {
  Memory memory;
  /* char* → Object of strings stored in 'memory'. */
  khash_t(sym_name) * symbols;
  khash_t(primitives) * primitive_functions;
  /* id → name symbol */
  khash_t(struct_ids) * struct_ids;

  /* Namespaces */
  /* name symbol → metadata */
  khash_t(var_syms) * structs;
  khash_t(var_syms) * globals;
  khash_t(var_syms) * functions;
  khash_t(var_syms) * macros;
  FILE *open_streams[LISP_MAX_OPEN_STREAMS];
  u16 next_struct_id;

  /* This is indexed with the ASCII values of reader macro characters. */
  ReaderMacro reader_macros[128];
  struct {
    /* Generate declarations for all the LISP_KEYSYMS. */
#define DECL_KEYSYM(K) Object K;
    LISP_KEYSYMS(DECL_KEYSYM)
#undef DECL_KEYSYM
    /* The Lisp symbols we want for these are also C keywords so they need
     * special treatment. */
    Object if_k;         /* if */
    Object while_k;      /* while */
    Object struct_k;     /* struct */
    Object print_struct; /* prin1-struct-to */
    Object star_k;       /* * */
    Object not_k;        /* not */
  } keysyms;
  jmp_buf error_loc;

  /* ECS Stuff */
  struct World *world;
  struct {
    Object name;
    Object lisp_component_storage;
    Object lisp_system;
  } comp;
  mtx_t memory_lock;
} LispEnv;

LispEnv new_lisp_environment();

/* Metadata required for a Component's data to be accessible from Lisp. */
enum LispComponentStorageType { STORE_OBJECT, STORE_STRUCT, STORE_UNBOXED };

struct LispComponentStorage {
  enum LispComponentStorageType type;
  u16 struct_id;
  /* This is redundant with 'struct Storage', but having it here saves an
   * access in prim_ecs_set. */
  size size;
  enum ObjectTag object_type;
};

/* Returns the canonical symbol whose name is string 'name'. */
Object lisp_intern(LispEnv *lisp, s8 name);
/* Short-hand for lisp_intern. */
#define SYM(LISP, NAME) (lisp_intern(LISP, s8(NAME)))

/* Print message and arg, then unwind the stack back up to the top level of the
 * REPL. */
void wrong(struct LispEnv *lisp, const char *message, Object arg);
#define WRONG2(MESSAGE, ARG)                                                   \
  do {                                                                         \
    wrong(lisp, MESSAGE, ARG);                                                 \
  } while (0)
#define WRONG1(MESSAGE) WRONG2(MESSAGE, NIL)
#define WRONGX(a, b, c, ...) c

/* Like wrong, but arg is optional, and defaults to NIL. */
#define WRONG(...) WRONGX(__VA_ARGS__, WRONG2, WRONG1)(__VA_ARGS__)
#define LISP_ASSERT_RAW_TYPE(OBJ, TYPE)                                        \
  do {                                                                         \
    if (OBJ_TYPE(OBJ) != TYPE) {                                               \
      WRONG("FATAL: Wrong type of " #OBJ ": expected " #TYPE ", got",          \
            lisp_tag_name(lisp, OBJ_TYPE(OBJ)));                               \
      exit(1); /* Should be unreachable */                                     \
    }                                                                          \
  } while (0)
#define LISP_ASSERT_TYPE(OBJ, TYPE) LISP_ASSERT_RAW_TYPE(OBJ, OBJ_##TYPE##_TAG)

static_assert(sizeof(Object) == sizeof(Object *),
              "Pointers must fit in exactly 1 cell");

/**
 * Get the Lisp cell at the given index, with no error handling.
 * Negative indices indicate that the data at that cell is a *pointer* to the
 * actual data.
 * @param index Lisp memory index.
 */
static inline Object *lisp_cell_at(struct LispEnv *lisp, size index) {
  if (index >= 0) {
    return &((Object *)lisp->memory.base)[index];
  } else {
    return ((Object **)lisp->memory.base)[-index];
  }
}

/* Lisp's "truthy" Boolean logic. */

static inline bool lisp_true(Object value) { return value.bits != NIL.bits; }

static inline bool lisp_false(Object value) { return !lisp_true(value); }

static inline Object lisp_bool(LispEnv *lisp, bool value) {
  return value ? lisp->keysyms.t : NIL;
}

/* Cons up a Lisp list from the arguments. The last argument *must* have a NIL
 * type tag. */
Object lisp_list(LispEnv *lisp, ...);

/* Lisp strings */

/* Store 'string' as a Lisp Object. */
Object lisp_store_string(LispEnv *lisp, s8 string);
/* Concatenate a list of strings to produce a new one */
Object lisp_concat(LispEnv *lisp, Object strings);

static inline size lisp_string_length(Object string) {
  return OBJ_UNBOX_METADATA(string);
}

static inline s8 lisp_string_to_s8(LispEnv *lisp, Object string) {
  LISP_ASSERT_TYPE(string, STRING);
  return (s8){(u8 *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(string)),
              lisp_string_length(string)};
}

const u8 *lisp_string_to_null_terminated(LispEnv *lisp, Object string);
u8 *lisp_to_string(LispEnv *lisp, Object object);

static inline i32 lisp_length(LispEnv *lisp, Object list) {
  i32 length = 0;
  while (OBJ_TYPE(list) == OBJ_PAIR_TAG) {
    length += 1;
    list = LISP_CDR(lisp, list);
  }
  LISP_ASSERT_TYPE(list, NIL);
  return length;
}

/* Lisp vectors */

static inline Object *lisp_get_vector_item(LispEnv *lisp, Object vector,
                                           i32 index) {
  if (index < 0 || index >= (i16)OBJ_UNBOX_METADATA(vector)) {
    WRONG("Index out of bounds", lisp_cons(lisp, vector, OBJ_BOX(index, INT)));
    return NULL;
  }
  return &lisp_cell_at(lisp, OBJ_UNBOX_INDEX(vector))[index];
}

static inline Object lisp_vector_from_kvec(LispEnv *lisp,
                                           ObjectVector objects) {
  Object vector = lisp_make_vector(lisp, kv_size(objects));
  memcpy(lisp_get_vector_item(lisp, vector, 0), &kv_A(objects, 0),
         kv_size(objects) * sizeof(Object));
  return vector;
}

/* Get the value associated with key in alist. */
Object lisp_assoc(LispEnv *lisp, Object key, Object alist);
/* Store stream as a Lisp object. */
Object lisp_store_stream_handle(LispEnv *lisp, FILE *stream);

/* Create a context (alist) on top of 'context' with 'parameters' bound to
 * 'arguments'.
 * Display the name of 'function' in an error message if binding fails,
 * most likely due to too few or too many arguments. */
Object lisp_bind(struct LispEnv *lisp, Object parameters, Object arguments,
                 Object function, Object context);

/* Lisp run-time core functions
 */
Object lisp_evaluate(struct LispEnv *lisp, Object expression, Object context);
Object lisp_eval(struct LispEnv *lisp, Object expression);
Object lisp_apply(struct LispEnv *lisp, Object function, Object arguments);
Object lisp_evaluate_sequence(struct LispEnv *lisp, Object sequence,
                              Object context);
Object lisp_add_to_namespace(struct LispEnv *lisp, khash_t(var_syms) * env,
                             Object symbol, Object value);
Object *lisp_lookup_variable(LispEnv *lisp, Object symbol, Object context);
Object lisp_lookup_function(LispEnv *lisp, Object symbol);
Object lisp_defname(LispEnv *lisp, Object ns, Object symbol, Object value);

/* Macro expansion */

Object lisp_macroexpand_top(LispEnv *lisp, Object expression);
Object lisp_macroexpand_list(LispEnv *lisp, Object list);
Object lisp_macroexpand(LispEnv *lisp, Object expression);

/* Create a new Component Entity that stores data with the type named by symbol 'type'.
 * 'type' can be the name of a struct or Lisp primitive type. */
Object lisp_new_ecs_component(LispEnv *lisp, Object type);

/* Obtain a Lisp address to use as an alias for ptr within Lisp. */
static inline size lisp_store_pointer(struct LispEnv *lisp, void *ptr) {
  /* Use negative indices to indicate double indirection. */
  size idx = lisp_allocate_cells(lisp, 1);
  *(void **)lisp_cell_at(lisp, idx) = ptr;
  return -idx;
}

/* Read a Lisp form from STR, then macroexpand and evaluate it. */
#define LISP_EVAL_STR(LISP, STR)                                               \
  (lisp_eval(LISP, lisp_macroexpand(lisp, OBJS(lisp, STR))))

/* Create a new Lisp System with the given Query and System Function. */
Object lisp_make_system(LispEnv *lisp, Object query, Object func);
#endif
