/* Primitive Lisp functions, macros and reader macros. */
#include "primitives.h"
#include "lisp.h"
#include "memory.h"
#include "object.h"
#include "print.h"
#include "reader.h"

#define FIRST (LISP_CAR(lisp, args))
#define SECOND (LISP_CAR(lisp, LISP_CDR(lisp, args)))

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

static Object prim_length(LispEnv *lisp, Object args) {
  return OBJ_BOX(lisp_length(lisp, FIRST), INT);
}

static Object prim_quit(LispEnv *lisp, Object args) {
  exit(0);
  return OBJ_UNDEFINED_TAG;
}

static Object prim_mul(LispEnv *lisp, Object args) {
  i32 product_int = 1;
  Object element = OBJ_INT_TAG;

  /* Integers */
  while (OBJ_TYPE(args) == OBJ_PAIR_TAG && OBJ_TYPE(element) == OBJ_INT_TAG) {
    element = FIRST;
    args = LISP_CDR(lisp, args);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      product_int *= (i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      break;
    default:
      WRONG("Wrong type argument to *");
      return OBJ_UNDEFINED_TAG;
    }
  }

  if (OBJ_TYPE(args) == OBJ_NIL_TAG && OBJ_TYPE(element) != OBJ_FLOAT_TAG) {
    return OBJ_BOX(product_int, INT);
  }

  /* Floats */
  float product_float = (float)product_int;
  if (OBJ_TYPE(element) == OBJ_FLOAT_TAG) {
    product_float *= lisp_unbox_float(element);
  }

  while (OBJ_TYPE(args) == OBJ_PAIR_TAG) {
    element = FIRST;
    args = LISP_CDR(lisp, args);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      product_float *= (float)(i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      product_float *= lisp_unbox_float(element);
      break;
    default:
      WRONG("Wrong type argument to *");
      return OBJ_UNDEFINED_TAG;
    }
  }

  return OBJ_BOX(*(u64 *)&product_float, FLOAT);
}

static Object prim_sub(LispEnv *lisp, Object args) {
  i32 difference_int;
  float difference_float;
  Object element = OBJ_INT_TAG;

  /* No arguments: 0 */
  if (OBJ_TYPE(args) != OBJ_PAIR_TAG)
    return OBJ_BOX(0, INT);

  /* Single argument: compute '- arg' */
  if (OBJ_TYPE(LISP_CDR(lisp, args)) == OBJ_NIL_TAG) {
    element = FIRST;

    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      return OBJ_BOX(-(i32)OBJ_UNBOX(element), INT);
    case OBJ_FLOAT_TAG:
      difference_float = -lisp_unbox_float(element);
      return OBJ_BOX(*(u64 *)&difference_float, FLOAT);
    default:
      WRONG("Wrong type argument to -");
      return OBJ_UNDEFINED_TAG;
    }
  }

  /* Multiple arguments: treat first as difference. */
  element = FIRST;
  args = LISP_CDR(lisp, args);

  if (OBJ_TYPE(element) == OBJ_INT_TAG) {
    difference_int = (i32)OBJ_UNBOX(element);

    /* Integers */
    while (OBJ_TYPE(args) == OBJ_PAIR_TAG && OBJ_TYPE(element) == OBJ_INT_TAG) {
      element = FIRST;
      args = LISP_CDR(lisp, args);
      switch (OBJ_TYPE(element)) {
      case OBJ_INT_TAG:
        difference_int -= (i32)OBJ_UNBOX(element);
        break;
      case OBJ_FLOAT_TAG:
        break;
      default:
        WRONG("Wrong type argument to -");
        return OBJ_UNDEFINED_TAG;
      }
    }

    if (OBJ_TYPE(args) == OBJ_NIL_TAG && OBJ_TYPE(element) != OBJ_FLOAT_TAG) {
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
    args = LISP_CDR(lisp, args);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      difference_float -= (float)(i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      difference_float -= lisp_unbox_float(element);
      break;
    default:
      WRONG("Wrong type argument to *");
      return OBJ_UNDEFINED_TAG;
    }
  }

  return OBJ_BOX(*(u64 *)&difference_float, FLOAT);
}

static Object prim_div(LispEnv *lisp, Object args) {
  i32 numerator_int = 1;
  float numerator_float;
  Object element = OBJ_INT_TAG;

  /* Single argument: compute 1 / arg */
  if (OBJ_TYPE(LISP_CDR(lisp, args)) == OBJ_NIL_TAG) {
    element = FIRST;

    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      return OBJ_BOX(1 / (i32)OBJ_UNBOX(element), INT);
    case OBJ_FLOAT_TAG:
      numerator_float = 1.0 / lisp_unbox_float(element);
      return OBJ_BOX(*(u64 *)&numerator_float, FLOAT);
    default:
      WRONG("Wrong type argument to /");
      return OBJ_UNDEFINED_TAG;
    }
  }

  /* Multiple arguments: treat first as numerator. */
  element = FIRST;
  args = LISP_CDR(lisp, args);

  if (OBJ_TYPE(element) == OBJ_INT_TAG) {
    numerator_int = (i32)OBJ_UNBOX(element);

    /* Integers */
    while (OBJ_TYPE(args) == OBJ_PAIR_TAG && OBJ_TYPE(element) == OBJ_INT_TAG) {
      element = FIRST;
      args = LISP_CDR(lisp, args);
      switch (OBJ_TYPE(element)) {
      case OBJ_INT_TAG:
        numerator_int /= (i32)OBJ_UNBOX(element);
        break;
      case OBJ_FLOAT_TAG:
        break;
      default:
        WRONG("Wrong type argument to /");
        return OBJ_UNDEFINED_TAG;
      }
    }

    if (OBJ_TYPE(args) == OBJ_NIL_TAG && OBJ_TYPE(element) != OBJ_FLOAT_TAG) {
      return OBJ_BOX(numerator_int, INT);
    }

    numerator_float = (float)numerator_int;

    if (OBJ_TYPE(element) == OBJ_FLOAT_TAG) {
      numerator_float /= lisp_unbox_float(element);
    }

  } else {
    numerator_float = lisp_unbox_float(element);
  }

  while (OBJ_TYPE(args) == OBJ_PAIR_TAG) {
    element = FIRST;
    args = LISP_CDR(lisp, args);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      numerator_float /= (float)(i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      numerator_float /= lisp_unbox_float(element);
      break;
    default:
      WRONG("Wrong type argument to *");
      return OBJ_UNDEFINED_TAG;
    }
  }

  return OBJ_BOX(*(u64 *)&numerator_float, FLOAT);
}

static Object prim_add(LispEnv *lisp, Object args) {
  i32 sum_int = 0;
  Object element = OBJ_INT_TAG;

  /* Integers */
  while (OBJ_TYPE(args) == OBJ_PAIR_TAG && OBJ_TYPE(element) == OBJ_INT_TAG) {
    element = FIRST;
    args = LISP_CDR(lisp, args);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      sum_int += (i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      break;
    default:
      WRONG("Wrong type argument to +");
      return OBJ_UNDEFINED_TAG;
    }
  }

  if (OBJ_TYPE(args) == OBJ_NIL_TAG && OBJ_TYPE(element) != OBJ_FLOAT_TAG) {
    return OBJ_BOX(sum_int, INT);
  }

  /* Floats */
  float sum_float = (float)sum_int;
  if (OBJ_TYPE(element) == OBJ_FLOAT_TAG) {
    sum_float += lisp_unbox_float(element);
  }

  while (OBJ_TYPE(args) == OBJ_PAIR_TAG) {
    element = FIRST;
    args = LISP_CDR(lisp, args);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      sum_float += (float)(i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      sum_float += lisp_unbox_float(element);
      break;
    default:
      WRONG("Wrong type argument to *");
      return OBJ_UNDEFINED_TAG;
    }
  }

  return OBJ_BOX(*(u64 *)&sum_float, FLOAT);
}

static Object prim_add2f(LispEnv *lisp, Object args) {
  Object first = FIRST;
  LISP_ASSERT_TYPE(first, FLOAT);
  args = LISP_CDR(lisp, args);
  LISP_ASSERT_TYPE(args, PAIR);
  Object second = FIRST;
  LISP_ASSERT_TYPE(second, FLOAT);
  float result = lisp_unbox_float(first) + lisp_unbox_float(second);
  return OBJ_BOX(*(u64 *)&result, FLOAT);
}
#define LISP_CMP(NAME, OP)                                                     \
  static Object NAME(LispEnv *lisp, Object args) {                             \
    Object a = FIRST;                                                          \
    args = LISP_CDR(lisp, args);                                               \
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
      return OBJ_UNDEFINED_TAG;                                                \
    }                                                                          \
  }

LISP_CMP(prim_less, <);
LISP_CMP(prim_less_equal, <=);
LISP_CMP(prim_greater, >);
LISP_CMP(prim_greater_equal, >=);

static Object prim_print(LispEnv *lisp, Object args) {
  lisp_print(lisp, FIRST, stdout);
  fputc('\n', stdout);
  return OBJ_NIL_TAG;
}

static Object prim_type_of(LispEnv *lisp, Object args) {
  return lisp_type_of(lisp, FIRST);
}

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

  u32 fn_iter = kh_get(primitives, primitives, prim_obj);
  if (fn_iter != kh_end(primitives)) {
    WRONG("Attempt to redefine primitive function.");
    return OBJ_UNDEFINED_TAG;
  }
  int absent;
  fn_iter = kh_put(primitives, primitives, prim_obj, &absent);
  if (absent < 1) {
    WRONG("Failed to define primitive function.");
    return OBJ_UNDEFINED_TAG;
  }

  kh_value(primitives, fn_iter) = prim;

  Object sym_iter =
      lisp_add_to_namespace(lisp, lisp->functions, name_sym, prim_obj);
  if (sym_iter == OBJ_UNDEFINED_TAG) {
    WRONG("Failed to add a primitive function name.");
    return sym_iter;
  }

  return prim_obj;
}

static Object prim_funcall(LispEnv *lisp, Object args) {
  Object function = FIRST;
  args = LISP_CDR(lisp, args);

  if (OBJ_TYPE(function) == OBJ_SYMBOL_TAG) {
    function = lisp_lookup_function(lisp, function);
  }
  return lisp_apply(lisp, function, args);
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
  Object pair = LISP_CAR(lisp, args);
  if (OBJ_TYPE(pair) == OBJ_NIL_TAG)
    return OBJ_NIL_TAG;

  LISP_ASSERT_TYPE(pair, PAIR);
  return LISP_CAR(lisp, pair);
}

static Object prim_cdr(LispEnv *lisp, Object args) {
  Object pair = LISP_CAR(lisp, args);
  if (OBJ_TYPE(pair) == OBJ_NIL_TAG)
    return OBJ_NIL_TAG;

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
    return OBJ_UNDEFINED_TAG;
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

  for (i32 i = 0; i < length; i++, args = LISP_CDR(lisp, args)) {
    cells[i] = LISP_CAR(lisp, args);
  }

  return vector;
}

static Object *lisp_get_vector_item(LispEnv *lisp, Object vector, i32 index) {
  if (index < 0 || index >= (i16)OBJ_UNBOX_METADATA(vector)) {
    WRONG("Index out of bounds", lisp_cons(lisp, vector, OBJ_BOX(index, INT)));
    return NULL;
  }
  return lisp_cell_at(lisp, OBJ_UNBOX_INDEX(vector) + index);
}

static Object prim_aset(LispEnv *lisp, Object args) {
  Object vector = LISP_CAR(lisp, args);
  args = LISP_CDR(lisp, args);
  i32 index = (i32)OBJ_UNBOX(LISP_CAR(lisp, args));
  args = LISP_CDR(lisp, args);
  Object value = LISP_CAR(lisp, args);
  return *lisp_get_vector_item(lisp, vector, index) = value;
}

static Object prim_aref(LispEnv *lisp, Object args) {
  Object vector = LISP_CAR(lisp, args);
  args = LISP_CDR(lisp, args);
  i32 index = (i32)OBJ_UNBOX(LISP_CAR(lisp, args));
  return *lisp_get_vector_item(lisp, vector, index);
}

static Object lisp_open_file(LispEnv *lisp, Object args) {
  Object filename = LISP_CAR(lisp, args);
  args = LISP_CDR(lisp, args);
  Object mode = LISP_CAR(lisp, args);
  char *filename_s = (char *)lisp_string_to_null_terminated(lisp, filename);
  char *mode_s = (char *)lisp_string_to_null_terminated(lisp, mode);
  FILE *stream = fopen(filename_s, mode_s);
  if (stream == NULL) {
    return OBJ_NIL_TAG;
  } else {
    return lisp_store_stream_handle(lisp, stream);
  }
}

static Object lisp_close_stream(LispEnv *lisp, Object args) {
  args = LISP_CAR(lisp, args);
  size index = OBJ_UNBOX(args);
  fclose(lisp->open_streams[index]);
  /* Release the slot so we can store another open stream there. */
  lisp->open_streams[index] = NULL;
  return OBJ_NIL_TAG;
}

static Object prim_feof(LispEnv *lisp, Object args) {
  FILE *file = lisp->open_streams[OBJ_UNBOX(FIRST)];
  return lisp_bool(lisp, feof(file) != 0);
}

static Object prim_getc_stream(LispEnv *lisp, Object args) {
  args = LISP_CAR(lisp, args);
  char c = fgetc(lisp->open_streams[OBJ_UNBOX(args)]);
  return OBJ_BOX(c, CHAR);
}

static Object prim_fputs_stream(LispEnv *lisp, Object args) {
  const char *s = (const char *)lisp_string_to_null_terminated(lisp, FIRST);
  fputs(s, lisp->open_streams[OBJ_UNBOX(SECOND)]);
  return OBJ_NIL_TAG;
}

static Object prim_wrong(LispEnv *lisp, Object args) {
  const char *message =
      (const char *)lisp_string_to_null_terminated(lisp, FIRST);
  WRONG(message, SECOND);
  return OBJ_UNDEFINED_TAG;
}

static Object prim_defname(LispEnv *lisp, Object args) {
  Object ns = LISP_CAR(lisp, args);
  args = LISP_CDR(lisp, args);
  Object name = LISP_CAR(lisp, args);
  args = LISP_CDR(lisp, args);
  Object value = LISP_CAR(lisp, args);

  return lisp_defname(lisp, ns, name, value);
}

/* READER MACROS */

Object lisp_reader_question_mark(LispEnv *lisp, FILE *stream) {
  return OBJ_BOX(fgetc(stream), CHAR);
}

/* Build a list of the form (quoter object) */
static inline Object lisp_quotify(LispEnv *lisp, Object quoter, Object object) {
  return lisp_cons(lisp, quoter, lisp_cons(lisp, object, OBJ_NIL_TAG));
}

/* Some built-in reader macros. */
#define QUOTER_READER(NAME)                                                    \
  static Object lisp_reader_##NAME(LispEnv *lisp, FILE *stream) {              \
    return lisp_quotify(lisp, lisp->keysyms.NAME, lisp_read(lisp, stream));    \
  }
QUOTER_READER(quote);
QUOTER_READER(unquote);
QUOTER_READER(quasiquote);

void lisp_install_primitives(LispEnv *lisp) {

  lisp->reader_macros['\''] = lisp_reader_quote;
  lisp->reader_macros['`'] = lisp_reader_quasiquote;
  lisp->reader_macros[','] = lisp_reader_unquote;
  lisp->reader_macros['?'] = lisp_reader_question_mark;

#define OBJSX(S) OBJS(lisp, S)
#define DEFPRIMFUN(NAME, SPEC, FUN)                                            \
  lisp_add_primitive(lisp, OBJSX(NAME), OBJSX(SPEC), FUN,                      \
                     lisp->primitive_functions)
  DEFPRIMFUN("+2f", "(f32 f32)", prim_add2f);
  DEFPRIMFUN("*", "t", prim_mul);
  DEFPRIMFUN("/", "(t . t)", prim_div);
  DEFPRIMFUN("-", "t", prim_sub);
  DEFPRIMFUN("+", "t", prim_add);
  DEFPRIMFUN("quit", "()", prim_quit);
  DEFPRIMFUN("fopen", "(string string)", lisp_open_file);
  DEFPRIMFUN("fclose", "(file)", lisp_close_stream);
  DEFPRIMFUN("fputs", "(string file) ", prim_fputs_stream);
  DEFPRIMFUN("getc", "(file)", prim_getc_stream);
  DEFPRIMFUN("feof", "(file) ", prim_feof);
  DEFPRIMFUN("read-stream", "(file)", prim_read);
  DEFPRIMFUN("car", "(pair)", prim_car);
  DEFPRIMFUN("cdr", "(pair)", prim_cdr);
  DEFPRIMFUN("setcar", "(pair t)", prim_setcar);
  DEFPRIMFUN("setcdr", "(pair t)", prim_setcdr);
  DEFPRIMFUN("cons", "(t t)", prim_cons);
  DEFPRIMFUN("list", "t", prim_list);
  DEFPRIMFUN("length", "(pair)", prim_length);
  DEFPRIMFUN("make-vector", "(i32 t)", prim_make_vector);
  DEFPRIMFUN("vector", "t", prim_vector);
  DEFPRIMFUN("aref", "(vector i32)", prim_aref);
  DEFPRIMFUN("aset", "(vector i32 t)", prim_aset);
  DEFPRIMFUN("eq", "(t t)", prim_eq);
  DEFPRIMFUN("eql", "(t t)", prim_eql);
  DEFPRIMFUN("<", "(t t)", prim_less);
  DEFPRIMFUN("<=", "(t t)", prim_less_equal);
  DEFPRIMFUN(">", "(t t)", prim_greater);
  DEFPRIMFUN(">=", "(t t)", prim_greater_equal);
  DEFPRIMFUN("print", "(t)", prim_print);
  DEFPRIMFUN("type-of", "(t)", prim_type_of);
  DEFPRIMFUN("funcall", "(t . t)", prim_funcall);
  DEFPRIMFUN("eval", "(t)", prim_eval);
  DEFPRIMFUN("macroexpand-1", "(t)", prim_macroexpand_1);
  DEFPRIMFUN("macroexpand", "(t)", prim_macroexpand);
  DEFPRIMFUN("wrong", "(string t)", prim_wrong);
  DEFPRIMFUN("defname", "(symbol symbol t)", prim_defname);
#undef DEFPRIMFUN
#undef OBJSX
}
