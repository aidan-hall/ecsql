#ifndef LISP_H
#define LISP_H

#include "common.h"
#include <stddef.h>
#include <gcc/jit/libgccjit.h>
typedef u64 Object;

#define OBJ_MASK (0xffffffffffffffe0)

/* 5-bit object type tag: allows up to 32 built-in types */
enum ObjectTag : Object {
  OBJ_NIL_TAG = OBJ_MASK,
  OBJ_PAIR_TAG,
  OBJ_UNDEFINED_TAG,
  OBJ_SYMBOL_TAG,
  OBJ_STRING_TAG,
  OBJ_INT_TAG,
  OBJ_FLOAT_TAG,
  OBJ_CLOSURE_TAG,
  OBJ_PRIMITIVE_TAG,
};
/* Filter out just the type tag of an object */
#define OBJ_TYPE(OBJ) (OBJ & OBJ_MASK)
#endif
