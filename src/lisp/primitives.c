/* Primitive Lisp functions, macros and reader macros. */
#include "raylib.h"
#include <ecs/ecs.h>
#include <klib/khash.h>
#include <lisp/lisp.h>
#include <lisp/memory.h>
#include <lisp/object.h>
#include <lisp/primitives.h>
#include <lisp/print.h>
#include <lisp/reader.h>
#include <lisp/systems.h>
#include <lisp/types.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#define FIRST (LISP_CAR(lisp, args))
#define SECOND (LISP_CAR(lisp, LISP_CDR(lisp, args)))
#define THIRD (LISP_CAR(lisp, LISP_CDR(lisp, LISP_CDR(lisp, args))))
/* Shift forward through the arguments N times. */
#define SHIFT_ARGS(N)                                                          \
  do {                                                                         \
    for (int i = 0; i < (N); ++i) {                                            \
      args = LISP_CDR(lisp, args);                                             \
    }                                                                          \
  } while (0)

/* PRIMITIVE FUNCTIONS */

static Object prim_cons(LispEnv *lisp, Object args) {
  return lisp_cons(lisp, FIRST, SECOND);
}

static Object prim_read(LispEnv *lisp, Object args) {
  FILE *stream = lisp->open_streams[OBJ_UNBOX(FIRST)];
  return lisp_read(lisp, stream);
}
static Object prim_list(LispEnv *lisp, Object args) { return args; }

static Object prim_eq(LispEnv *lisp, Object args) {
  return lisp_bool(lisp, EQ(FIRST, SECOND));
}

static Object prim_eql(LispEnv *lisp, Object args) {
  Object a = FIRST;
  Object b = SECOND;
  if (OBJ_TYPE(a) == OBJ_FLOAT_TAG) {
    if (OBJ_TYPE(b) == OBJ_FLOAT_TAG) {
      return lisp_bool(lisp, lisp_unbox_float(a) == lisp_unbox_float(b));
    } else if (OBJ_TYPE(b) == OBJ_INT_TAG) {
      return lisp_bool(lisp, lisp_unbox_float(a) == (float)(i32)OBJ_UNBOX(b));
    }
  } else if (OBJ_TYPE(a) == OBJ_INT_TAG && OBJ_TYPE(b) == OBJ_FLOAT_TAG) {
    return lisp_bool(lisp, (float)(i32)OBJ_UNBOX(a) == lisp_unbox_float(b));
  } else if (OBJ_TYPE(a) == OBJ_STRING_TAG && OBJ_TYPE(b) == OBJ_STRING_TAG) {
    s8 as = lisp_string_to_s8(lisp, a);
    s8 bs = lisp_string_to_s8(lisp, b);
    size len = as.len < bs.len ? as.len : bs.len;
    return lisp_bool(lisp, memcmp(as.data, bs.data, len) == 0);
  }

  return lisp_bool(lisp, EQ(a, b));
}

static Object prim_assoc(LispEnv *lisp, Object args) {
  return lisp_assoc(lisp, FIRST, SECOND);
}

static Object prim_length(LispEnv *lisp, Object args) {
  Object obj = FIRST;
  switch (OBJ_TYPE(obj)) {
  case OBJ_NIL_TAG:
    return OBJ_IMM(0);
  case OBJ_PAIR_TAG:
    return OBJ_BOX(lisp_length(lisp, FIRST), INT);
  case OBJ_VECTOR_TAG:
  case OBJ_STRING_TAG:
    return OBJ_BOX(OBJ_UNBOX_METADATA(obj), INT);
  default:
    WRONG("Cannot take the length of object of type", lisp_type_of(lisp, obj));
    return UNDEFINED;
  }
}

static Object prim_quit(LispEnv *lisp, Object args) {
  exit(0);
  return UNDEFINED;
}

static Object prim_mul(LispEnv *lisp, Object args) {
  i32 product_int = 1;
  Object element = OBJ_BOX(0, INT);

  /* Integers */
  while (OBJ_TYPE(args) == OBJ_PAIR_TAG && OBJ_TYPE(element) == OBJ_INT_TAG) {
    element = FIRST;
    SHIFT_ARGS(1);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      product_int *= (i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      break;
    default:
      WRONG("Wrong type argument to *");
      return UNDEFINED;
    }
  }

  if (EQ(args, NIL) && OBJ_TYPE(element) != OBJ_FLOAT_TAG) {
    return OBJ_BOX(product_int, INT);
  }

  /* Floats */
  float product_float = (float)product_int;
  if (OBJ_TYPE(element) == OBJ_FLOAT_TAG) {
    product_float *= lisp_unbox_float(element);
  }

  while (OBJ_TYPE(args) == OBJ_PAIR_TAG) {
    element = FIRST;
    SHIFT_ARGS(1);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      product_float *= (float)(i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      product_float *= lisp_unbox_float(element);
      break;
    default:
      WRONG("Wrong type argument to *");
      return UNDEFINED;
    }
  }

  return OBJ_IMM(product_float);
}

static Object prim_sub(LispEnv *lisp, Object args) {
  i32 difference_int;
  float difference_float;
  Object element = OBJ_BOX(0, INT);

  /* No arguments: 0 */
  if (OBJ_TYPE(args) != OBJ_PAIR_TAG)
    return OBJ_BOX(0, INT);

  /* Single argument: compute '- arg' */
  if (EQ(LISP_CDR(lisp, args), NIL)) {
    element = FIRST;

    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      return OBJ_BOX(-(i32)OBJ_UNBOX(element), INT);
    case OBJ_FLOAT_TAG:
      difference_float = -lisp_unbox_float(element);
      return OBJ_IMM(difference_float);
    default:
      WRONG("Wrong type argument to -");
      return UNDEFINED;
    }
  }

  /* Multiple arguments: treat first as difference. */
  element = FIRST;
  SHIFT_ARGS(1);

  if (OBJ_TYPE(element) == OBJ_INT_TAG) {
    difference_int = (i32)OBJ_UNBOX(element);

    /* Integers */
    while (OBJ_TYPE(args) == OBJ_PAIR_TAG && OBJ_TYPE(element) == OBJ_INT_TAG) {
      element = FIRST;
      SHIFT_ARGS(1);
      switch (OBJ_TYPE(element)) {
      case OBJ_INT_TAG:
        difference_int -= (i32)OBJ_UNBOX(element);
        break;
      case OBJ_FLOAT_TAG:
        break;
      default:
        WRONG("Wrong type argument to -");
        return UNDEFINED;
      }
    }

    if (EQ(args, NIL) && OBJ_TYPE(element) != OBJ_FLOAT_TAG) {
      return OBJ_BOX(difference_int, INT);
    }

    difference_float = (float)difference_int;

    if (OBJ_TYPE(element) == OBJ_FLOAT_TAG) {
      difference_float -= lisp_unbox_float(element);
    }

  } else {
    difference_float = lisp_unbox_float(element);
  }

  while (OBJ_TYPE(args) == OBJ_PAIR_TAG) {
    element = FIRST;
    SHIFT_ARGS(1);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      difference_float -= (float)(i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      difference_float -= lisp_unbox_float(element);
      break;
    default:
      WRONG("Wrong type argument to *");
      return UNDEFINED;
    }
  }

  return OBJ_IMM(difference_float);
}

static Object prim_div(LispEnv *lisp, Object args) {
  i32 numerator_int = 1;
  float numerator_float;
  Object element = OBJ_BOX(0, INT);

  /* Single argument: compute 1 / arg */
  if (OBJ_TYPE(LISP_CDR(lisp, args)) == OBJ_NIL_TAG) {
    element = FIRST;

    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG: {
      i32 denominator = OBJ_UNBOX(element);
      if (denominator == 0) {
        WRONG("Division by 0.");
      }
      return OBJ_BOX(1 / denominator, INT);
    }
    case OBJ_FLOAT_TAG: {
      float denominator = lisp_unbox_float(element);
      if (denominator == 0) {
        WRONG("Division by 0.");
      }
      numerator_float = 1.0 / denominator;
      return OBJ_IMM(numerator_float);
    }
    default:
      WRONG("Wrong type argument to /");
      return UNDEFINED;
    }
  }

  /* Multiple arguments: treat first as numerator. */
  element = FIRST;
  SHIFT_ARGS(1);

  if (OBJ_TYPE(element) == OBJ_INT_TAG) {
    numerator_int = (i32)OBJ_UNBOX(element);

    /* Integers */
    while (OBJ_TYPE(args) == OBJ_PAIR_TAG && OBJ_TYPE(element) == OBJ_INT_TAG) {
      element = FIRST;
      SHIFT_ARGS(1);
      switch (OBJ_TYPE(element)) {
      case OBJ_INT_TAG: {
        i32 denominator = OBJ_UNBOX(element);
        if (denominator == 0) {
          WRONG("Division by 0.");
        }
        numerator_int /= denominator;
      } break;
      case OBJ_FLOAT_TAG:
        break;
      default:
        WRONG("Wrong type argument to /");
        return UNDEFINED;
      }
    }

    if (EQ(args, NIL) && OBJ_TYPE(element) != OBJ_FLOAT_TAG) {
      return OBJ_BOX(numerator_int, INT);
    }

    numerator_float = (float)numerator_int;

    if (OBJ_TYPE(element) == OBJ_FLOAT_TAG) {
      float denominator = lisp_unbox_float(element);
      if (denominator == 0) {
        WRONG("Division by 0.");
      }

      numerator_float /= denominator;
    }

  } else {
    numerator_float = lisp_unbox_float(element);
  }

  while (OBJ_TYPE(args) == OBJ_PAIR_TAG) {
    element = FIRST;
    SHIFT_ARGS(1);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG: {
      i32 denominator = OBJ_UNBOX(element);
      if (denominator == 0) {
        WRONG("Division by 0.");
      }
      numerator_float /= (float)denominator;
    } break;
    case OBJ_FLOAT_TAG: {
      float denominator = lisp_unbox_float(element);
      if (denominator == 0) {
        WRONG("Division by 0.");
      }

      numerator_float /= denominator;
    } break;
    default:
      WRONG("Wrong type argument to *");
      return UNDEFINED;
    }
  }

  return OBJ_IMM(numerator_float);
}

static Object prim_add(LispEnv *lisp, Object args) {
  i32 sum_int = 0;
  Object element = OBJ_BOX(0, INT);

  /* Integers */
  while (OBJ_TYPE(args) == OBJ_PAIR_TAG && OBJ_TYPE(element) == OBJ_INT_TAG) {
    element = FIRST;
    SHIFT_ARGS(1);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      sum_int += (i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      break;
    default:
      WRONG("Wrong type argument to +");
      return UNDEFINED;
    }
  }

  if (EQ(args, NIL) && OBJ_TYPE(element) != OBJ_FLOAT_TAG) {
    return OBJ_BOX(sum_int, INT);
  }

  /* Floats */
  float sum_float = (float)sum_int;
  if (OBJ_TYPE(element) == OBJ_FLOAT_TAG) {
    sum_float += lisp_unbox_float(element);
  }

  while (OBJ_TYPE(args) == OBJ_PAIR_TAG) {
    element = FIRST;
    SHIFT_ARGS(1);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      sum_float += (float)(i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      sum_float += lisp_unbox_float(element);
      break;
    default:
      WRONG("Wrong type argument to *");
      return UNDEFINED;
    }
  }

  return OBJ_IMM(sum_float);
}

static Object prim_add2f(LispEnv *lisp, Object args) {
  Object first = FIRST;
  LISP_ASSERT_TYPE(first, FLOAT);
  SHIFT_ARGS(1);
  LISP_ASSERT_TYPE(args, PAIR);
  Object second = FIRST;
  LISP_ASSERT_TYPE(second, FLOAT);
  float result = lisp_unbox_float(first) + lisp_unbox_float(second);
  return OBJ_IMM(result);
}

static Object prim_mod(LispEnv *lisp, Object args) {
  return OBJ_BOX(
      BIT_CAST(i32, OBJ_UNBOX(FIRST)) % BIT_CAST(i32, OBJ_UNBOX(SECOND)), INT);
}

static Object prim_floor(LispEnv *lisp, Object args) {
  return OBJ_IMM((i32)floorf(lisp_unbox_float(FIRST)));
}

static Object prim_ceiling(LispEnv *lisp, Object args) {
  return OBJ_IMM((i32)ceilf(lisp_unbox_float(FIRST)));
}

#define LISP_CMP(NAME, OP)                                                     \
  static Object NAME(LispEnv *lisp, Object args) {                             \
    Object a = FIRST;                                                          \
    SHIFT_ARGS(1);                                                             \
    Object b = FIRST;                                                          \
    if (OBJ_TYPE(a) == OBJ_INT_TAG && OBJ_TYPE(b) == OBJ_INT_TAG) {            \
      return lisp_bool(lisp, (i32)OBJ_UNBOX(a) OP(i32) OBJ_UNBOX(b));          \
    } else if (OBJ_TYPE(a) == OBJ_FLOAT_TAG && OBJ_TYPE(b) == OBJ_INT_TAG) {   \
      return lisp_bool(lisp, lisp_unbox_float(a) OP(float)(i32) OBJ_UNBOX(b)); \
    } else if (OBJ_TYPE(a) == OBJ_INT_TAG && OBJ_TYPE(b) == OBJ_FLOAT_TAG) {   \
      return lisp_bool(lisp, (float)(i32)OBJ_UNBOX(a) OP lisp_unbox_float(b)); \
    } else if (OBJ_TYPE(a) == OBJ_FLOAT_TAG && OBJ_TYPE(b) == OBJ_FLOAT_TAG) { \
      return lisp_bool(lisp, lisp_unbox_float(a) OP lisp_unbox_float(b));      \
    } else {                                                                   \
      WRONG("Invalidtypes of parameters to " #OP,                              \
            lisp_cons(lisp, lisp_type_of(lisp, a), lisp_type_of(lisp, b)));    \
      return UNDEFINED;                                                        \
    }                                                                          \
  }

LISP_CMP(prim_less, <);
LISP_CMP(prim_less_equal, <=);
LISP_CMP(prim_greater, >);
LISP_CMP(prim_greater_equal, >=);

static Object prim_print(LispEnv *lisp, Object args) {
  lisp_print(lisp, SECOND, lisp->open_streams[OBJ_UNBOX(FIRST)]);
  return NIL;
}

static Object prim_type_of(LispEnv *lisp, Object args) {
  return lisp_type_of(lisp, FIRST);
}

static Object prim_type_tag(LispEnv *lisp, Object args) {
  return OBJ_BOX(lisp_type_tag(lisp, FIRST), INT);
}

static Object prim_funcall(LispEnv *lisp, Object args) {
  Object function = FIRST;
  SHIFT_ARGS(1);

  if (OBJ_TYPE(function) == OBJ_SYMBOL_TAG) {
    function = lisp_lookup_function(lisp, function);
  }
  return lisp_apply(lisp, function, args);
}

static Object build_apply_arglist(LispEnv *lisp, Object args) {
  if (OBJ_TYPE(args) != OBJ_PAIR_TAG) {
    WRONG("Invalid argument list to apply.");
  }
  if (OBJ_TYPE(LISP_CDR(lisp, args)) != OBJ_NIL_TAG) {
    return lisp_cons(lisp, LISP_CAR(lisp, args),
                     build_apply_arglist(lisp, LISP_CDR(lisp, args)));
  }
  if (OBJ_TYPE(LISP_CAR(lisp, args)) != OBJ_PAIR_TAG) {
    WRONG("Last argument to apply was not a list.");
  }
  return LISP_CAR(lisp, args);
}

static Object prim_apply(LispEnv *lisp, Object args) {
  Object function = FIRST;
  SHIFT_ARGS(1);

  if (OBJ_TYPE(function) == OBJ_SYMBOL_TAG) {
    function = lisp_lookup_function(lisp, function);
  }
  return lisp_apply(lisp, function, build_apply_arglist(lisp, args));
}

static Object prim_eval(LispEnv *lisp, Object args) {
  return lisp_eval(lisp, lisp_macroexpand(lisp, FIRST));
}

static Object prim_macroexpand(LispEnv *lisp, Object args) {
  return lisp_macroexpand(lisp, FIRST);
}

static Object prim_macroexpand_1(LispEnv *lisp, Object args) {
  return lisp_macroexpand_top(lisp, FIRST);
}

static Object prim_car(LispEnv *lisp, Object args) {
  Object pair = FIRST;
  if (EQ(pair, NIL))
    return NIL;

  LISP_ASSERT_TYPE(pair, PAIR);
  return LISP_CAR(lisp, pair);
}

static Object prim_cdr(LispEnv *lisp, Object args) {
  Object pair = FIRST;
  if (EQ(pair, NIL))
    return NIL;

  LISP_ASSERT_TYPE(pair, PAIR);
  return LISP_CDR(lisp, pair);
}

static Object prim_setcar(LispEnv *lisp, Object args) {
  Object value = SECOND;
  *LISP_CAR_PLACE(lisp, FIRST) = value;
  return value;
}

static Object prim_setcdr(LispEnv *lisp, Object args) {
  Object value = SECOND;
  *LISP_CDR_PLACE(lisp, FIRST) = value;
  return value;
}

static Object prim_make_vector(LispEnv *lisp, Object args) {
  i32 length = (i32)OBJ_UNBOX(FIRST);
  if (length < 0) {
    WRONG("Negative length of vector", FIRST);
    return UNDEFINED;
  }
  Object vector = lisp_make_vector(lisp, length);
  Object *cells = lisp_cell_at(lisp, OBJ_UNBOX_INDEX(vector));

  Object init_value = SECOND;

  for (i32 i = 0; i < length; ++i) {
    cells[i] = init_value;
  }

  return vector;
}

static Object prim_vector(LispEnv *lisp, Object args) {
  i32 length = lisp_length(lisp, args);
  Object vector = lisp_make_vector(lisp, length);
  Object *cells = lisp_cell_at(lisp, OBJ_UNBOX_INDEX(vector));

  for (i32 i = 0; i < length; i++) {
    cells[i] = FIRST;
    SHIFT_ARGS(1);
  }

  return vector;
}

static Object prim_aset(LispEnv *lisp, Object args) {
  Object vector = FIRST;
  SHIFT_ARGS(1);
  i32 index = (i32)OBJ_UNBOX(FIRST);
  SHIFT_ARGS(1);
  Object value = FIRST;
  return *lisp_get_vector_item(lisp, vector, index) = value;
}

static Object prim_aref(LispEnv *lisp, Object args) {
  Object vector = FIRST;
  SHIFT_ARGS(1);
  i32 index = (i32)OBJ_UNBOX(FIRST);
  return *lisp_get_vector_item(lisp, vector, index);
}

static Object lisp_open_file(LispEnv *lisp, Object args) {
  Object filename = FIRST;
  SHIFT_ARGS(1);
  Object mode = FIRST;
  char *filename_s = (char *)lisp_string_to_null_terminated(lisp, filename);
  char *mode_s = (char *)lisp_string_to_null_terminated(lisp, mode);
  FILE *stream = fopen(filename_s, mode_s);
  if (stream == NULL) {
    return NIL;
  } else {
    return lisp_store_stream_handle(lisp, stream);
  }
}

static Object lisp_close_stream(LispEnv *lisp, Object args) {
  args = FIRST;
  size index = OBJ_UNBOX(args);
  fclose(lisp->open_streams[index]);
  /* Release the slot so we can store another open stream there. */
  lisp->open_streams[index] = NULL;
  return NIL;
}

static Object prim_feof(LispEnv *lisp, Object args) {
  FILE *file = lisp->open_streams[OBJ_UNBOX(FIRST)];
  return lisp_bool(lisp, feof(file) != 0);
}

static Object prim_getc_stream(LispEnv *lisp, Object args) {
  args = FIRST;
  char c = fgetc(lisp->open_streams[OBJ_UNBOX(args)]);
  return OBJ_BOX(c, CHAR);
}

static Object prim_fputs_stream(LispEnv *lisp, Object args) {
  const char *s = (const char *)lisp_string_to_null_terminated(lisp, FIRST);
  fputs(s, lisp->open_streams[OBJ_UNBOX(SECOND)]);
  return NIL;
}

static Object prim_fputc_stream(LispEnv *lisp, Object args) {
  const u8 c = OBJ_UNBOX(FIRST);
  fputc(c, lisp->open_streams[OBJ_UNBOX(SECOND)]);
  return NIL;
}

static Object prim_wrong(LispEnv *lisp, Object args) {
  const char *message =
      (const char *)lisp_string_to_null_terminated(lisp, FIRST);
  WRONG(message, SECOND);
  return UNDEFINED;
}

/* Create a new string of the specified length, filled with the supplied
 * character. */
static Object prim_make_string(LispEnv *lisp, Object args) {
  i32 length = (i32)OBJ_UNBOX(FIRST);
  if (length < 0) {
    WRONG("Negative length of string", FIRST);
    return UNDEFINED;
  }
  size data_index = lisp_allocate_bytes(lisp, length + 1);
  char *chars = (char *)lisp_cell_at(lisp, data_index);

  memset(chars, OBJ_UNBOX(SECOND), length);
  chars[length] = '\0';

  return OBJ_BOX_INDEX(data_index, length, STRING);
}

static Object prim_to_string(LispEnv *lisp, Object args) {
  u8 *buf = lisp_to_string(lisp, FIRST);
  return lisp_store_string(
      lisp, (s8){buf, strnlen((const char *)buf, LISP_MAX_STRING_LENGTH)});
}

static Object prim_symbol_name(LispEnv *lisp, Object args) {
  return OBJ_REINTERPRET(FIRST, STRING);
}

static Object prim_intern(LispEnv *lisp, Object args) {
  return lisp_intern(lisp, lisp_string_to_s8(lisp, FIRST));
}

/* Create a new, uninterned symbol with its name being the supplied argument. */
static Object prim_make_symbol(LispEnv *lisp, Object args) {
  s8 string = lisp_string_to_s8(lisp, FIRST);
  return OBJ_REINTERPRET(lisp_store_string(lisp, string), SYMBOL);
}

static Object prim_defname(LispEnv *lisp, Object args) {
  Object ns = FIRST;
  SHIFT_ARGS(1);
  Object name = FIRST;
  SHIFT_ARGS(1);
  Object value = FIRST;

  return lisp_defname(lisp, ns, name, value);
}

/* Get the size, in Bytes, of objects with the supplied type. */
static Object prim_size_of(LispEnv *lisp, Object args) {
  Object obj = FIRST;
  /* Get the size of a struct, stored in the first element of the struct
   * metadata vector. */
  khint_t iter = kh_get(var_syms, lisp->structs, obj.bits);
  if (iter != kh_end(lisp->structs))
    return *lisp_get_vector_item(lisp, kh_value(lisp->structs, iter), 0);

#define SIZE_CASE(SYMBOL, TYPE)                                                \
  if (EQ(obj, lisp->keysyms.SYMBOL))                                           \
  return OBJ_BOX(sizeof(TYPE), INT)
  SIZE_CASE(i32, int);
  SIZE_CASE(f32, float);
  SIZE_CASE(character, u8);
  SIZE_CASE(symbol, Object);
  SIZE_CASE(vector, Object);
  SIZE_CASE(file, Object);
  SIZE_CASE(pair, Object);
  SIZE_CASE(closure, Object);
  SIZE_CASE(string, Object);

#undef SIZE_CASE

  WRONG("Called size-of with a non-type argument", obj);
  return UNDEFINED;
}

static Object prim_type_spec_matches(LispEnv *lisp, Object args) {
  return lisp_bool(lisp, lisp_type_spec_matches(lisp, FIRST, SECOND));
}

/* STRUCT API */

static Object prim_is_struct(LispEnv *lisp, Object args) {
  /* We can't write this test in Lisp because type-of returns a struct's struct
   * type name, not the symbol 'struct. */
  return lisp_bool(lisp, OBJ_TYPE(FIRST) == OBJ_STRUCT_TAG);
}

/* (symbol) Generate a new struct id, pointing to the given symbol. */
static Object prim_struct_register(LispEnv *lisp, Object args) {
  Object name = FIRST;
  if (lisp->next_struct_id == UINT16_MAX) {
    WRONG("Ran out of 16-bit IDs for structs, somehow.");
    return UNDEFINED;
  }
  u16 id = lisp->next_struct_id++;
  const Object boxed_id = OBJ_BOX(id, INT);

  khint_t iter = kh_get(struct_ids, lisp->struct_ids, id);
  if (iter != kh_end(lisp->struct_ids)) {
    WRONG("Somehow generated a duplicate struct ID", boxed_id);
    exit(1);
  }

  int absent;
  iter = kh_put(struct_ids, lisp->struct_ids, id, &absent);
  if (absent < 1) {
    WRONG("Failed to add struct id to lisp->struct_ids", boxed_id);
  }

  kh_value(lisp->struct_ids, iter) = name;

  return boxed_id;
}

static Object prim_struct_metadata(LispEnv *lisp, Object args) {
  khint_t iter = kh_get(var_syms, lisp->structs, FIRST.bits);
  if (iter == kh_end(lisp->structs)) {
    /* Indicates that the provided symbol is not a struct name. */
    return NIL;
  }
  return kh_value(lisp->structs, iter);
}

/* These functions allow somewhat arbitrary memory manipulation,
 * and are primarily intended for use in implementing structs. */

/* (struct offset/B) */
static Object prim_mem_get_object(LispEnv *lisp, Object args) {
  char *loc = &((char *)lisp_cell_at(lisp, FIRST.index))[SECOND.val];

  Object value;
  memcpy(&value.bits, loc, sizeof(Object));
  return value;
}

/* (struct offset/B value) */
static Object prim_mem_set_object(LispEnv *lisp, Object args) {
  char *loc = &((char *)lisp_cell_at(lisp, FIRST.index))[SECOND.val];
  SHIFT_ARGS(2);

  Object value = FIRST;
  memcpy(loc, &value.bits, sizeof(Object));
  return value;
}

/* (struct offset/B type len/B) */
static Object prim_mem_get_val(LispEnv *lisp, Object args) {
  char *loc = &((char *)lisp_cell_at(lisp, FIRST.index))[SECOND.val];
  SHIFT_ARGS(2);

  size len = SECOND.val;
  /* If something takes up > 7 Bytes, it doesn't fit in an Object box. */
  assert(len < sizeof(Object));
  enum ObjectTag tag = FIRST.val;
  u64 value = 0;
  /* Store the data "at the back" of the value, where it gets stored in Lisp. */
  memcpy(&value, loc, len);
  return OBJ_BOX_RAWTAG(value, tag);
}

/* (struct offset/B value len/B) */
static Object prim_mem_set_val(LispEnv *lisp, Object args) {
  char *dest_loc = &((char *)lisp_cell_at(lisp, FIRST.index))[SECOND.val];
  SHIFT_ARGS(2);
  size len = SECOND.val;
  u64 val = FIRST.val;
  /* Copy from the start of the data, which is stored "at the back" of val. */
  memcpy(dest_loc, &val, len);
  return FIRST;
}

/* (struct offset/B struct-type len/B)
 * Produces a copy. */
static Object prim_mem_get_vec(LispEnv *lisp, Object args) {
  /* TODO: Handle potentially misaligned pointers here? */
  char *src = &((char *)lisp_cell_at(lisp, FIRST.index))[SECOND.val];
  SHIFT_ARGS(2);
  size len = SECOND.val;
  size dest_idx = lisp_allocate_bytes(lisp, len);
  char *dest = (char *)lisp_cell_at(lisp, dest_idx);
  memcpy(dest, src, len);
  return OBJ_BOX_INDEX(dest_idx, FIRST.val, STRUCT);
}

/* (struct offset/B value-reference len/B) */
static Object prim_mem_set_vec(LispEnv *lisp, Object args) {
  char *dest_loc = &((char *)lisp_cell_at(lisp, FIRST.index))[SECOND.val];
  SHIFT_ARGS(2);
  size len = SECOND.val;
  char *src_loc = (char *)lisp_cell_at(lisp, FIRST.index);
  memcpy(dest_loc, src_loc, len);
  return FIRST;
}

/* (i32 i32): (struct type ID, # cells) */
static Object prim_struct_allocate(LispEnv *lisp, Object args) {
  i32 bytes = (i32)OBJ_UNBOX(FIRST);
  size data_index = lisp_allocate_bytes(lisp, bytes);
  return OBJ_BOX_INDEX(data_index, (u16)OBJ_UNBOX(SECOND), STRUCT);
}

/* ECS API */

static Object prim_make_entity(LispEnv *lisp, Object args) {
  return ENT_BOX((EntityID){BIT_CAST(u32, OBJ_UNBOX(FIRST))},
                 BIT_CAST(u32, OBJ_UNBOX(SECOND)));
}
/* Obtain the live Entity with the supplied ID, otherwise NIL. */
static Object prim_entity_with_id(LispEnv *lisp, Object args) {
  EntityID id = (EntityID){OBJ_UNBOX(FIRST)};
  u16 *gen = ecs_generation(lisp->world, id);
  Object entity = ENT_BOX(id, *gen);
  return ecs_alive(lisp->world, entity) ? entity : NIL;
}

static Object prim_ecs_new(LispEnv *lisp, Object args) {
  return ecs_new(lisp->world);
}

static Object prim_ecs_destroy(LispEnv *lisp, Object args) {
  Object obj = FIRST;
  if (!ecs_alive(lisp->world, obj)) {
    WRONG("Attempt to destroy a dead object.", obj);
  }
  ecs_destroy(lisp->world, FIRST);
  return NIL;
}

static Object get_ecs_data(LispEnv *lisp, struct LispComponentStorage *storage,
                           void *obj) {
  switch (storage->type) {
  case STORE_OBJECT:
    return *(Object *)obj;
  case STORE_STRUCT:
    /* We want to be able to modify a struct's data in-place, so return a
     * pointer to that. */
    return OBJ_BOX_INDEX(lisp_store_pointer(lisp, obj), storage->struct_id,
                         STRUCT);
  case STORE_UNBOXED: {
    u64 val = 0;
    memcpy(&val, obj, storage->size);
    return OBJ_BOX_RAWTAG(val, storage->object_type);
  }
  }

  WRONG("Invalid Lisp storage type", OBJ_BOX(storage->type, INT));
  return UNDEFINED;
}

static Object prim_ecs_get(LispEnv *lisp, Object args) {
  Object component = SECOND;
  Object entity = FIRST;
  struct LispComponentStorage *storage =
      ecs_get(lisp->world, component, lisp->comp.lisp_component_storage);
  if (storage == NULL) {
    WRONG("No Lisp type storage metadata for the given component type",
          component);
    return UNDEFINED;
  }
  void *obj = ecs_get(lisp->world, entity, component);
  if (obj == NULL) {
    WRONG("Attempted to get a Component with no storage, or that the Entity "
          "didn't have",
          args);
    return UNDEFINED;
  }

  return get_ecs_data(lisp, storage, obj);
}

static Object prim_ecs_set(LispEnv *lisp, Object args) {
  Object component = SECOND;
  Object entity = FIRST;
  Object value = THIRD;
  struct LispComponentStorage *lisp_storage =
      ecs_get(lisp->world, component, lisp->comp.lisp_component_storage);
  if (lisp_storage == NULL) {
    WRONG("No Lisp type storage metadata for the given component type",
          component);
    return UNDEFINED;
  }
  void *loc = ecs_get(lisp->world, entity, component);
  if (loc == NULL) {
    WRONG("Attempted to set a Component with no storage, or that the Entity "
          "didn't have",
          args);
    return UNDEFINED;
  }

  switch (lisp_storage->type) {
  case STORE_OBJECT:
    /* Allow undefined type to let users store any Object in a Component */
    if (lisp_storage->object_type != OBJ_TYPE(value) &&
        lisp_storage->object_type != OBJ_UNDEFINED_TAG) {
      WRONG("Attempt to store incorrect type of object in a Component "
            "(expected . actual)",
            lisp_cons(lisp, lisp_tag_name(lisp, lisp_storage->object_type),
                      lisp_type_of(lisp, component)));
      return UNDEFINED;
    }
    *(Object *)loc = value;
    break;
  case STORE_STRUCT:
    if (OBJ_TYPE(value) != OBJ_STRUCT_TAG) {
      WRONG("Attempted to store non-struct in a struct Component", value);
      return UNDEFINED;
    }
    if (lisp_storage->struct_id != OBJ_UNBOX_METADATA(value)) {
      WRONG("Attempted to store incorrect type of struct in a Component "
            "(expected . actual)",
            lisp_cons(lisp, OBJ_BOX(lisp_storage->struct_id, INT),
                      OBJ_BOX(OBJ_UNBOX_METADATA(value), INT)));
      return UNDEFINED;
    }
    memcpy(loc, lisp_cell_at(lisp, OBJ_UNBOX_INDEX(value)), lisp_storage->size);
    break;
  case STORE_UNBOXED: {
    u64 data = value.val;
    memcpy(loc, &data, lisp_storage->size);
  } break;
  }
  return value;
}

/* Returns nil for Entities with no Storage or LispComponentStorage. */
static Object prim_ecs_storage_type(LispEnv *lisp, Object args) {
  Object entity = FIRST;
  struct World *world = lisp->world;
  WorldComponents *world_components = ecs_world_components(world);

  if (OBJ_TYPE(entity) == OBJ_RELATION_TAG) {
    Object original = entity;
    entity = ecs_object_with_id(world, (EntityID){entity.relation});
    if (EQ(entity, NIL)) {
      WRONG("Failed to get storage type of a relation", original);
      return UNDEFINED;
    }
  }

  if (!ecs_has(world, entity, world_components->storage) ||
      !ecs_has(world, entity, lisp->comp.lisp_component_storage)) {
    return NIL;
  }
  struct LispComponentStorage storage = *(struct LispComponentStorage *)ecs_get(
      world, entity, lisp->comp.lisp_component_storage);
  if (storage.type != STORE_STRUCT) {
    return lisp_tag_name(lisp, storage.object_type);
  }
  u16 id = storage.struct_id;
  khiter_t iter = kh_get(struct_ids, lisp->struct_ids, id);
  if (iter == kh_end(lisp->struct_ids)) {
    WRONG("Couldn't find name of struct with ID", OBJ_IMM((i32)id));
    return UNDEFINED;
  }
  return kh_value(lisp->struct_ids, iter);
}

static Object prim_ecs_set_name(LispEnv *lisp, Object args) {
  return lisp_bool(lisp, ecs_set_name(lisp->world, FIRST, SECOND));
}

static Object prim_ecs_lookup_by_name(LispEnv *lisp, Object args) {
  return ecs_lookup_by_name(lisp->world, FIRST);
}

static Object prim_ecs_alive(LispEnv *lisp, Object args) {
  Object obj = FIRST;
  return lisp_bool(lisp, ecs_alive(lisp->world, obj));
}

static Object prim_ecs_has(LispEnv *lisp, Object args) {
  return lisp_bool(lisp, ecs_has(lisp->world, FIRST, SECOND));
}

static Object prim_ecs_pair(LispEnv *lisp, Object args) {
  IGNORE(lisp);
  return ecs_pair(FIRST, SECOND);
}

static Object prim_ecs_pair_target(LispEnv *lisp, Object args) {
  Object obj = FIRST;

  return ecs_object_with_id(lisp->world, obj.entity);
}

static Object prim_ecs_pair_relation(LispEnv *lisp, Object args) {
  Object obj = FIRST;

  return ecs_object_with_id(lisp->world, (EntityID){obj.relation});
}

static Object prim_ecs_add(LispEnv *lisp, Object args) {
  ecs_add(lisp->world, FIRST, SECOND);
  return NIL;
}

static Object prim_ecs_remove(LispEnv *lisp, Object args) {
  ecs_remove(lisp->world, FIRST, SECOND);
  return NIL;
}

static Object prim_ecs_id(LispEnv *lisp, Object args) {
  return OBJ_BOX(FIRST.id.val, INT);
}

static Object prim_ecs_gen(LispEnv *lisp, Object args) {
  Object obj = FIRST;
  return OBJ_BOX(obj.gen, INT);
}

/* (type-name) */
static Object prim_ecs_new_component(LispEnv *lisp, Object args) {
  return lisp_new_ecs_component(lisp, FIRST);
}

static Object prim_ecs_do_query(LispEnv *lisp, Object args) {
  Object query = FIRST;
  Object function = SECOND;
  ecs_do_query(lisp, query, lisp_run_system, BIT_CAST(void *, function));
  return NIL;
}

static Object prim_get_mouse_y(LispEnv *lisp, Object args) {
  IGNORE(args);
  return OBJ_IMM(GetMouseY());
}

static Object prim_get_mouse_x(LispEnv *lisp, Object args) {
  IGNORE(args);
  return OBJ_IMM(GetMouseX());
}

static Object prim_get_screen_width(LispEnv *lisp, Object args) {
  IGNORE(args);
  return OBJ_IMM(GetScreenWidth());
}

static Object prim_get_screen_height(LispEnv *lisp, Object args) {
  IGNORE(args);
  return OBJ_IMM(GetScreenHeight());
}

static Object prim_get_delta(LispEnv *lisp, Object args) {
  IGNORE(args);
  return OBJ_IMM(GetFrameTime());
}

static Object prim_draw_text(LispEnv *lisp, Object args) {
  const char *text = (char *)lisp_string_to_null_terminated(lisp, FIRST);
  SHIFT_ARGS(1);
  float x = lisp_unbox_float(FIRST);
  SHIFT_ARGS(1);
  float y = lisp_unbox_float(FIRST);
  SHIFT_ARGS(1);
  float font_size = lisp_unbox_float(FIRST);

  DrawText(text, x, y, font_size, WHITE);
  return NIL;
}

static MouseButton symbol_to_mouse_button(LispEnv *lisp, Object sym) {
  if (EQ(sym, lisp->keysyms.left)) {
    return MOUSE_BUTTON_LEFT;
  } else if (EQ(sym, lisp->keysyms.right)) {
    return MOUSE_BUTTON_RIGHT;
  } else {
    WRONG("Invalid mouse button name", sym);
    exit(1);
  }
}

static Object prim_mouse_pressed(LispEnv *lisp, Object args) {
  return lisp_bool(lisp,
                   IsMouseButtonPressed(symbol_to_mouse_button(lisp, FIRST)));
}

static Object prim_mouse_down(LispEnv *lisp, Object args) {
  return lisp_bool(lisp,
                   IsMouseButtonDown(symbol_to_mouse_button(lisp, FIRST)));
}

static Object prim_make_system(LispEnv *lisp, Object args) {
  return lisp_make_system(lisp, FIRST, SECOND);
}

/* READER MACROS */

Object lisp_reader_hash(LispEnv *lisp, FILE *stream) {
  u8 next = fgetc(stream);
  switch (next) {
  case '\\':
    return OBJ_BOX(fgetc(stream), CHAR);
  case '\'':
    return lisp_cons(lisp, lisp->keysyms.function,
                     lisp_cons(lisp, lisp_read(lisp, stream), NIL));
  case 's': {
    Object stream_id = lisp_read(lisp, stream);
    LISP_ASSERT_TYPE(stream_id, INT);
    size stream_index = OBJ_UNBOX(stream_id);
    if (stream_index < 0 || stream_index > LISP_MAX_OPEN_STREAMS) {
      WRONG("Invalid stream id", stream_id);
    } else if (lisp->open_streams[stream_index] == NULL) {
      WRONG("Stream is not open", stream_id);
      return UNDEFINED;
    } else {
      return OBJ_BOX(stream_index, FILE_PTR);
    }
  }
  case '/':
    return lisp_cons(lisp, lisp->keysyms.macro,
                     lisp_cons(lisp, lisp_read(lisp, stream), NIL));
  case '*': {
    Object type = lisp_read(lisp, stream);
    LISP_ASSERT_TYPE(type, SYMBOL);
    return lisp_cons(
        lisp,
        lisp_intern(
            lisp,
            lisp_string_to_s8(
                lisp,
                lisp_concat(
                    lisp, lisp_list(lisp, lisp_store_string(lisp, s8("make-")),
                                    OBJ_REINTERPRET(type, STRING), NIL)))),
        lisp_read(lisp, stream));
  }
  case '!':
    WRONG("Attempt to read an unreadable expression", lisp_read(lisp, stream));
    return UNDEFINED;
  case 'g':
    WRONG("Attempt to re-read gensym numbered", lisp_read(lisp, stream));
    return UNDEFINED;
  default:
    WRONG("Unknown # reader macro", OBJ_BOX(next, CHAR));
    return NIL;
  }
}

/* Build a list of the form (quoter object) */
static inline Object lisp_quotify(LispEnv *lisp, Object quoter, Object object) {
  return lisp_cons(lisp, quoter, lisp_cons(lisp, object, NIL));
}

/* Some built-in reader macros. */
#define QUOTER_READER(NAME)                                                    \
  static Object lisp_reader_##NAME(LispEnv *lisp, FILE *stream) {              \
    return lisp_quotify(lisp, lisp->keysyms.NAME, lisp_read(lisp, stream));    \
  }
QUOTER_READER(quote);
QUOTER_READER(unquote);
QUOTER_READER(quasiquote);

static Object lisp_add_primitive(LispEnv *lisp, Object name_sym, Object params,
                                 PrimitiveFunction fn,
                                 khash_t(primitives) * primitives) {
  Object prim_obj = OBJ_REINTERPRET_RAWTAG(name_sym, OBJ_PRIMITIVE_TAG);

  InterpreterPrimitive prim;
  prim.fn = fn;
  /* Just used for error messages. */
  prim.id_symbol = name_sym;
  /* The input argument type list may be stack-allocated. */
  prim.argument_types = params;

  u32 fn_iter = kh_get(primitives, primitives, prim_obj.bits);
  if (fn_iter != kh_end(primitives)) {
    WRONG("Attempt to redefine primitive function.");
    return UNDEFINED;
  }
  int absent;
  fn_iter = kh_put(primitives, primitives, prim_obj.bits, &absent);
  if (absent < 1) {
    WRONG("Failed to define primitive function.");
    return UNDEFINED;
  }

  kh_value(primitives, fn_iter) = prim;

  Object sym_iter =
      lisp_add_to_namespace(lisp, lisp->functions, name_sym, prim_obj);
  if (EQ(sym_iter, UNDEFINED)) {
    WRONG("Failed to add a primitive function name.");
    return sym_iter;
  }

  return prim_obj;
}

void lisp_install_primitives(LispEnv *lisp) {

  lisp->reader_macros['\''] = lisp_reader_quote;
  lisp->reader_macros['`'] = lisp_reader_quasiquote;
  lisp->reader_macros[','] = lisp_reader_unquote;
  lisp->reader_macros['#'] = lisp_reader_hash;

#define OBJSX(S) OBJS(lisp, S)
#define DEFPRIMFUN(NAME, SPEC, FUN)                                            \
  lisp_add_primitive(lisp, SYM(lisp, NAME), OBJSX(SPEC), FUN,                  \
                     lisp->primitive_functions)
  DEFPRIMFUN("+2f", "(f32 f32)", prim_add2f);
  DEFPRIMFUN("*", "(* (or f32 i32))", prim_mul);
  DEFPRIMFUN("%", "(i32 i32)", prim_mod);
  DEFPRIMFUN("/", "(t . t)", prim_div);
  DEFPRIMFUN("-", "(* (or f32 i32))", prim_sub);
  DEFPRIMFUN("+", "(* (or f32 i32))", prim_add);
  DEFPRIMFUN("floor", "(f32)", prim_floor);
  DEFPRIMFUN("ceiling", "(f32)", prim_ceiling);
  DEFPRIMFUN("quit", "()", prim_quit);
  DEFPRIMFUN("fopen", "(string string)", lisp_open_file);
  DEFPRIMFUN("fclose", "(file)", lisp_close_stream);
  DEFPRIMFUN("fputs", "(string file) ", prim_fputs_stream);
  DEFPRIMFUN("fputc", "(character file) ", prim_fputc_stream);
  DEFPRIMFUN("concat", "(* string)", lisp_concat);
  DEFPRIMFUN("make-string", "(i32 character)", prim_make_string);
  DEFPRIMFUN("symbol-name", "(symbol)", prim_symbol_name);
  DEFPRIMFUN("intern", "(string)", prim_intern);
  DEFPRIMFUN("make-symbol", "(string)", prim_make_symbol);
  DEFPRIMFUN("getc", "(file)", prim_getc_stream);
  DEFPRIMFUN("feof", "(file) ", prim_feof);
  DEFPRIMFUN("read-stream", "(file)", prim_read);
  DEFPRIMFUN("car", "((or pair nil))", prim_car);
  DEFPRIMFUN("cdr", "((or pair nil))", prim_cdr);
  DEFPRIMFUN("setcar", "(pair t)", prim_setcar);
  DEFPRIMFUN("setcdr", "(pair t)", prim_setcdr);
  DEFPRIMFUN("cons", "(t t)", prim_cons);
  DEFPRIMFUN("list", "t", prim_list);
  DEFPRIMFUN("length", "((or pair vector string nil))", prim_length);
  DEFPRIMFUN("make-vector", "(i32 t)", prim_make_vector);
  DEFPRIMFUN("vector", "t", prim_vector);
  DEFPRIMFUN("aref", "(vector i32)", prim_aref);
  DEFPRIMFUN("aset", "(vector i32 t)", prim_aset);
  DEFPRIMFUN("eq", "(t t)", prim_eq);
  DEFPRIMFUN("eql", "(t t)", prim_eql);
  DEFPRIMFUN("assoc", "(t (or pair nil))", prim_assoc);
  DEFPRIMFUN("<", "(t t)", prim_less);
  DEFPRIMFUN("<=", "(t t)", prim_less_equal);
  DEFPRIMFUN(">", "(t t)", prim_greater);
  DEFPRIMFUN(">=", "(t t)", prim_greater_equal);
  DEFPRIMFUN("prin1-to*", "(file t)", prim_print);
  DEFPRIMFUN("to-string", "(t)", prim_to_string);
  DEFPRIMFUN("type-of", "(t)", prim_type_of);
  DEFPRIMFUN("type-tag", "(symbol)", prim_type_tag);
  DEFPRIMFUN("funcall", "(t . t)", prim_funcall);
  DEFPRIMFUN("apply", "(t . t)", prim_apply);
  DEFPRIMFUN("eval", "(t)", prim_eval);
  DEFPRIMFUN("macroexpand-1", "(t)", prim_macroexpand_1);
  DEFPRIMFUN("macroexpand", "(t)", prim_macroexpand);
  DEFPRIMFUN("wrong", "(string t)", prim_wrong);
  DEFPRIMFUN("defname", "(symbol symbol t)", prim_defname);
  DEFPRIMFUN("size-of", "(symbol)", prim_size_of);
  DEFPRIMFUN("type-spec-matches", "(t t)", prim_type_spec_matches);
  /* TODO: These are private, and only called from generated code, so don't
   * waste time checking the type. */
  DEFPRIMFUN("--struct-set-vec", "(t i32 t i32)", prim_mem_set_vec);
  DEFPRIMFUN("--struct-set-val", "(t i32 t i32)", prim_mem_set_val);
  DEFPRIMFUN("--struct-get-vec", "(t i32 i32 i32)", prim_mem_get_vec);
  DEFPRIMFUN("--struct-get-val", "(t i32 i32 i32)", prim_mem_get_val);
  DEFPRIMFUN("--struct-get-object", "(t i32)", prim_mem_get_object);
  DEFPRIMFUN("--struct-set-object", "(t i32 t)", prim_mem_set_object);
  DEFPRIMFUN("--struct-allocate", "(i32 i32)", prim_struct_allocate);
  DEFPRIMFUN("--struct-register", "(symbol)", prim_struct_register);
  DEFPRIMFUN("structp", "(t)", prim_is_struct);
  DEFPRIMFUN("struct-metadata", "(symbol)", prim_struct_metadata);

  DEFPRIMFUN("ecs-new", "()", prim_ecs_new);
  DEFPRIMFUN("make-entity", "(i32 i32)", prim_make_entity);
  DEFPRIMFUN("ecs-entity", "(i32)", prim_entity_with_id);
  DEFPRIMFUN("ecs-destroy", "(entity)", prim_ecs_destroy);
  DEFPRIMFUN("ecs-get", "(entity (or entity relation))", prim_ecs_get);
  DEFPRIMFUN("ecs-set", "(entity (or entity relation) t)", prim_ecs_set);
  DEFPRIMFUN("ecs-set-name", "(entity symbol)", prim_ecs_set_name);
  DEFPRIMFUN("ecs-lookup", "(symbol)", prim_ecs_lookup_by_name);
  DEFPRIMFUN("ecs-has", "(entity (or entity relation))", prim_ecs_has);
  DEFPRIMFUN("ecs-alive", "(entity)", prim_ecs_alive);
  DEFPRIMFUN("ecs-add", "(entity (or entity relation))", prim_ecs_add);
  DEFPRIMFUN("ecs-remove", "(entity (or entity relation))", prim_ecs_remove);
  DEFPRIMFUN("ecs-id", "(entity)", prim_ecs_id);
  DEFPRIMFUN("ecs-gen", "(entity)", prim_ecs_gen);
  DEFPRIMFUN("make-relation", "(entity entity)", prim_ecs_pair);
  DEFPRIMFUN("ecs-pair", "(entity entity)", prim_ecs_pair);
  DEFPRIMFUN("ecs-relation", "(relation)", prim_ecs_pair_relation);
  DEFPRIMFUN("ecs-target", "(relation)", prim_ecs_pair_target);
  DEFPRIMFUN("ecs-new-component", "(symbol)", prim_ecs_new_component);
  DEFPRIMFUN("ecs-do-query", "(t t)", prim_ecs_do_query);
  DEFPRIMFUN("ecs-register-system", "(pair (or closure primitive))",
             prim_make_system);
  DEFPRIMFUN("ecs-storage-type", "((or entity relation))",
             prim_ecs_storage_type);

  DEFPRIMFUN("get-mouse-y", "()", prim_get_mouse_y);
  DEFPRIMFUN("get-mouse-x", "()", prim_get_mouse_x);
  DEFPRIMFUN("get-screen-width", "()", prim_get_screen_width);
  DEFPRIMFUN("get-screen-height", "()", prim_get_screen_height);
  DEFPRIMFUN("get-delta", "()", prim_get_delta);
  DEFPRIMFUN("draw-text", "(string f32 f32 i32)", prim_draw_text);
  DEFPRIMFUN("is-mouse-down", "(symbol)", prim_mouse_down);
  DEFPRIMFUN("is-mouse-pressed", "(symbol)", prim_mouse_pressed);
#undef DEFPRIMFUN
#undef OBJSX
}
