#include <klib/khash.h>
#include <lisp/lisp.h>
#include <lisp/object.h>
#include <lisp/types.h>

enum ObjectTag lisp_type_tag(LispEnv *lisp, Object obj) {
  if (EQ(obj, lisp->keysyms.nil))
    return OBJ_NIL_TAG;
  else if (EQ(obj, lisp->keysyms.i32))
    return OBJ_INT_TAG;
  else if (EQ(obj, lisp->keysyms.string))
    return OBJ_STRING_TAG;
  else if (EQ(obj, lisp->keysyms.f32))
    return OBJ_FLOAT_TAG;
  else if (EQ(obj, lisp->keysyms.character))
    return OBJ_CHAR_TAG;
  else if (EQ(obj, lisp->keysyms.undefined))
    return OBJ_UNDEFINED_TAG;
  else if (EQ(obj, lisp->keysyms.symbol))
    return OBJ_SYMBOL_TAG;
  else if (EQ(obj, lisp->keysyms.file))
    return OBJ_FILE_PTR_TAG;
  else if (EQ(obj, lisp->keysyms.primitive))
    return OBJ_PRIMITIVE_TAG;
  else if (EQ(obj, lisp->keysyms.closure))
    return OBJ_CLOSURE_TAG;
  else if (EQ(obj, lisp->keysyms.pair))
    return OBJ_PAIR_TAG;
  else if (EQ(obj, lisp->keysyms.vector))
    return OBJ_VECTOR_TAG;
  else if (EQ(obj, lisp->keysyms.entity))
    return OBJ_ENTITY_TAG;
  else if (EQ(obj, lisp->keysyms.relation))
    return OBJ_RELATION_TAG;
  else if (EQ(obj, lisp->keysyms.struct_k)) {
    return OBJ_STRUCT_TAG;
  }
  WRONG("Invalid object type name.", obj);
  return OBJ_UNDEFINED_TAG;
}

Object lisp_tag_name(LispEnv *lisp, enum ObjectTag tag) {
  switch (tag) {
  case OBJ_NIL_TAG:
    return lisp->keysyms.nil;
  case OBJ_INT_TAG:
    return lisp->keysyms.i32;
  case OBJ_STRING_TAG:
    return lisp->keysyms.string;
  case OBJ_FLOAT_TAG:
    return lisp->keysyms.f32;
  case OBJ_CHAR_TAG:
    return lisp->keysyms.character;
  case OBJ_UNDEFINED_TAG:
    return lisp->keysyms.undefined;
  case OBJ_SYMBOL_TAG:
    return lisp->keysyms.symbol;
  case OBJ_FILE_PTR_TAG:
    return lisp->keysyms.file;
  case OBJ_PRIMITIVE_TAG:
    return lisp->keysyms.primitive;
  case OBJ_CLOSURE_TAG:
    return lisp->keysyms.closure;
  case OBJ_PAIR_TAG:
    return lisp->keysyms.pair;
  case OBJ_STRUCT_TAG:
    return lisp->keysyms.struct_k;
  case OBJ_VECTOR_TAG:
    return lisp->keysyms.vector;
  case OBJ_ENTITY_TAG:
    return lisp->keysyms.entity;
  case OBJ_RELATION_TAG:
    return lisp->keysyms.relation;
  }
  WRONG("Invalid type tag of object.", OBJ_BOX(tag, INT));
  return UNDEFINED;
}

Object lisp_type_of(struct LispEnv *lisp, Object obj) {
  switch (OBJ_TYPE(obj)) {
  case OBJ_STRUCT_TAG: {
    khint_t iter =
        kh_get(struct_ids, lisp->struct_ids, OBJ_UNBOX_METADATA(obj));
    if (iter == kh_end(lisp->struct_ids)) {
      WRONG("Unknown struct type", OBJ_BOX(OBJ_UNBOX_METADATA(obj), INT));
      return UNDEFINED;
    }
    return kh_value(lisp->struct_ids, iter);
  }
  default:
    return lisp_tag_name(lisp, OBJ_TYPE(obj));
  }
}

bool lisp_type_spec_matches(LispEnv *lisp, Object value, Object spec) {
  if (OBJ_TYPE(spec) == OBJ_PAIR_TAG) {
    /* Handle specs of the form (or type type type...) */
    if (EQ(LISP_CAR(lisp, spec), lisp->keysyms.or)) {
      while (OBJ_TYPE(spec) == OBJ_PAIR_TAG) {
        if (lisp_type_spec_matches(lisp, value, LISP_CAR(lisp, spec))) {
          return true;
        }
        spec = LISP_CDR(lisp, spec);
      }
      return false;
    }
    /* (* type) */
    if (EQ(LISP_CAR(lisp, spec), lisp->keysyms.star_k)) {
      Object type = LISP_CAR(lisp, LISP_CDR(lisp, spec));
      while (OBJ_TYPE(value) == OBJ_PAIR_TAG) {
        if (!lisp_type_spec_matches(lisp, LISP_CAR(lisp, value), type)) {
          return false;
        }
        value = LISP_CDR(lisp, value);
      }
      return true;
    }
  }

  while (OBJ_TYPE(value) == OBJ_PAIR_TAG && OBJ_TYPE(spec) == OBJ_PAIR_TAG) {
    if (!lisp_type_spec_matches(lisp, LISP_CAR(lisp, value),
                                LISP_CAR(lisp, spec)))
      return false;
    value = LISP_CDR(lisp, value);
    spec = LISP_CDR(lisp, spec);
  }

  if (EQ(spec, lisp->keysyms.t)) {
    /* Variadic argument list accepts anything. */
    return true;
  }
  /* Handle end of list. */
  if (EQ(value, NIL) && EQ(spec, NIL))
    return true;

  return EQ(lisp_type_of(lisp, value), spec);
}
