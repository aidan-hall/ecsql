#ifndef TYPES_H
#define TYPES_H

#include "object.h"
#include <klib/khash.h>

#include <common.h>

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
KHASH_MAP_INIT_INT(struct_ids, Object);
KHASH_MAP_INIT_INT(vtables, khash_t(sym_name));

Object lisp_tag_name(struct LispEnv *lisp, enum ObjectTag tag);
Object lisp_type_of(struct LispEnv *lisp, Object obj);
enum ObjectTag lisp_type_tag(struct LispEnv *lisp, Object obj);
bool lisp_type_spec_matches(struct LispEnv *lisp, Object value, Object spec);

#endif
