#ifndef TYPES_H
#define TYPES_H

#include "khash.h"

#include "common.h"

typedef u64 Object;

struct LispEnv;

typedef Object (*ReaderMacro)(struct LispEnv *lisp, FILE *stream);
typedef Object (*PrimitiveFunction)(struct LispEnv *lisp, Object arguments);

typedef struct {
  PrimitiveFunction fn;
  Object id_symbol;
  /* Interpreted according to lisp_type_spec_matches(). */
  Object argument_types;
} InterpreterPrimitive;

KHASH_MAP_INIT_INT64(var_syms, Object);
KHASH_MAP_INIT_INT64(primitives, InterpreterPrimitive);
KHASH_MAP_INIT_STR(sym_name, Object);

Object lisp_type_of(struct LispEnv *lisp, Object obj);
bool lisp_type_spec_matches(struct LispEnv *lisp, Object value, Object spec);

#endif
