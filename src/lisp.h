#ifndef LISP_H
#define LISP_H

#include "common.h"
#include "khash.h"
#include <assert.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include "memory.h"
#include "object.h"

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
  F(function)                                                                  \
  F(stdin)                                                                     \
  F(stdout)                                                                    \
  F(stderr)                                                                    \
  F(or)                                                                        \
  F(cond)                                                                      \
  F(and)                                                                       \
  F(progn)                                                                     \
  F(define)                                                                    \
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
  F(defun)                                                                     \
  F(defmacro)                                                                  \
  F(eof)

typedef struct LispEnv {
  Memory memory;
  /* char* → Object of strings stored in 'memory'. */
  khash_t(sym_name) * symbols;
  khash_t(primitives) * primitive_functions;
  khash_t(var_syms) *globals;
  khash_t(var_syms) *functions;
  khash_t(var_syms) *macros;
  FILE *open_streams[LISP_MAX_OPEN_STREAMS];
  /* This is indexed with the ASCII values of reader macro characters. */
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

LispEnv new_lisp_environment();

Object lisp_store_string(LispEnv *lisp, s8 string);
/* Return the canonical symbol whose name is string 'name'. */
Object lisp_intern(LispEnv *lisp, s8 name);

void wrong(struct LispEnv *lisp, const char *message, Object arg);
#define WRONG2(MESSAGE, ARG)                                                   \
  do {                                                                         \
    wrong(lisp, MESSAGE, ARG);                                                 \
  } while (0)
#define WRONG1(MESSAGE) WRONG2(MESSAGE, OBJ_NIL_TAG)
#define WRONGX(a, b, c, ...) c
#define WRONG(...) WRONGX(__VA_ARGS__, WRONG2, WRONG1)(__VA_ARGS__)
#define LISP_ASSERT_TYPE(OBJ, TYPE)                                            \
  do {                                                                         \
    if (OBJ_TYPE(OBJ) != OBJ_##TYPE##_TAG) {                                   \
      WRONG("FATAL: Wrong type of " #OBJ ": expected " #TYPE ", got",          \
            lisp_type_of(lisp, OBJ));                                          \
      exit(1); /* Should be unreachable */                                     \
    }                                                                          \
  } while (0)


/* Cons up a Lisp list. The last argument *must* have a NIL type tag. */
Object lisp_list(LispEnv *lisp, ...);
#endif
