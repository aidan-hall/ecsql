#include <klib/khash.h>
#include <lisp/types.h>
#include <lisp/lisp.h>
#include <lisp/object.h>

Object lisp_type_of(LispEnv *lisp, Object obj) {
  switch (OBJ_TYPE(obj)) {
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
  case OBJ_VECTOR_TAG:
    return lisp->keysyms.vector;
  case OBJ_STRUCT_TAG: {
    khint_t iter =
        kh_get(struct_ids, lisp->struct_ids, OBJ_UNBOX_METADATA(obj));
    if (iter == kh_end(lisp->struct_ids)) {
      WRONG("Unknown struct type", OBJ_BOX(OBJ_UNBOX_METADATA(obj), INT));
      return OBJ_UNDEFINED_TAG;
    }
    return kh_value(lisp->struct_ids, iter);
  }
  }
  WRONG("Invalid type tag of object.", OBJ_BOX(OBJ_TYPE(obj), INT));
  return OBJ_UNDEFINED_TAG;
}

bool lisp_type_spec_matches(LispEnv *lisp, Object value, Object spec) {

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
  if (OBJ_TYPE(value) == OBJ_NIL_TAG && OBJ_TYPE(spec) == OBJ_NIL_TAG)
    return true;

  return EQ(lisp_type_of(lisp, value), spec);
}
