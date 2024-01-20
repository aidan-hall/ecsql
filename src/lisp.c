#include "lisp.h"
#include "reader.h"
#include <libgccjit.h>

void wrong(const char *message) {
  fputs(message, stderr);
  fputc('\n', stderr);
  exit(1);
}

static inline size lisp_string_length(Object string) {
  /* LISP_ASSERT_TYPE(string, STRING); */
  return OBJ_UNBOX_METADATA(string);
}

static inline s8 lisp_string_to_s8(LispEnv *lisp, Object string) {
  LISP_ASSERT_TYPE(string, STRING);
  return (s8){(u8 *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(string)),
              lisp_string_length(string)};
}

static inline u8 *lisp_string_to_null_terminated(LispEnv *lisp, Object string) {
  LISP_ASSERT_TYPE(string, STRING);
  s8 string_s8 = lisp_string_to_s8(lisp, string);
  if (string_s8.data[string_s8.len] != '\0') {
    string = lisp_store_string(lisp, string_s8);
    return (u8 *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(string));
  } else {
    return string_s8.data;
  }
}

Object lisp_question_mark(LispEnv *lisp, FILE *stream) {
  return OBJ_BOX(fgetc(stream), CHAR);
}

/* Build a list of the form (quoter object) */
static inline Object lisp_quotify(LispEnv *lisp, Object quoter, Object object) {
  return lisp_cons(lisp, quoter, lisp_cons(lisp, object, OBJ_NIL_TAG));
}

/* Some built-in reader macros. */
static Object lisp_quote(LispEnv *lisp, FILE *stream) {
  return lisp_quotify(lisp, lisp->keysyms.quote, lisp_read(lisp, stream));
}

static Object lisp_quasiquote(LispEnv *lisp, FILE *stream) {
  lisp->quasiquote_level++;
  Object res =
      lisp_quotify(lisp, lisp->keysyms.quasiquote, lisp_read(lisp, stream));
  lisp->quasiquote_level--;
  return res;
}

static Object lisp_unquote(LispEnv *lisp, FILE *stream) {
  if (lisp->quasiquote_level <= 0) {
    wrong("Attempted to unquote outside a quasiquote.");
    return OBJ_UNDEFINED_TAG;
  }
  lisp->quasiquote_level--;
  Object res =
      lisp_quotify(lisp, lisp->keysyms.unquote, lisp_read(lisp, stream));
  lisp->quasiquote_level++;
  return res;
}

static Object lisp_define_global(LispEnv *lisp, Object symbol, Object value) {
  /* TODO: In what context do globals exist? Create a new one per global? Sounds
   * icky... */
  LISP_ASSERT_TYPE(symbol, SYMBOL);

  khint_t global_key = kh_get(var_syms, lisp->globals, symbol);
  if (global_key != kh_end(lisp->globals)) {
    wrong("Attempt to re-define global variable.");
    return OBJ_UNDEFINED_TAG;
  }

  /* char *name = (char *)lisp_string_to_null_terminated( */
  /*     lisp, OBJ_REINTERPRET(symbol, STRING)); */
  /* gcc_jit_lvalue *global_loc = gcc_jit_context_new_global( */
  /*     lisp->jit.ctxt, nullptr, GCC_JIT_GLOBAL_EXPORTED,
   * lisp->jit.object_type, */
  /*     name); */
  int absent;
  global_key = kh_put(var_syms, lisp->globals, symbol, &absent);
  if (absent < 0) {
    wrong("Failed to create global variable: couldn't add it to the variable "
          "namespace.");
    return OBJ_UNDEFINED_TAG;
  }
  kh_value(lisp->globals, global_key) = value;
  return value;
}

/* static Object lisp_define_function(LispEnv *lisp, Object ) */

static Object lisp_store_stream_handle(LispEnv *lisp, FILE *stream) {
  size i;
  for (i = 0; i < LISP_MAX_OPEN_STREAMS; i++) {
    if (lisp->open_streams[i] == nullptr) {
      lisp->open_streams[i] = stream;
      return OBJ_BOX(i, FILE_PTR);
    }
  }
  wrong("The maximum number of streams is already open.");
  return OBJ_UNDEFINED_TAG;
}

static Object lisp_open_file(LispEnv *lisp, Object args) {
  Object filename = LISP_CAR(lisp, args);
  LISP_ASSERT_TYPE(filename, STRING);
  args = LISP_CDR(lisp, args);
  LISP_ASSERT_TYPE(args, PAIR);
  Object mode = LISP_CAR(lisp, args);
  LISP_ASSERT_TYPE(mode, STRING);
  char *filename_s = lisp_string_to_null_terminated(lisp, filename);
  char *mode_s = lisp_string_to_null_terminated(lisp, mode);
  FILE *stream = fopen(filename_s, mode_s);
  if (stream == nullptr) {
    return OBJ_NIL_TAG;
  } else {
    return lisp_store_stream_handle(lisp, stream);
  }
}

static Object lisp_close_stream(LispEnv *lisp, Object args) {
  args = LISP_CAR(lisp, args);
  LISP_ASSERT_TYPE(args, FILE_PTR);
  size index = OBJ_UNBOX(args);
  fclose(lisp->open_streams[index]);
  /* Release the slot so we can store another open stream there. */
  lisp->open_streams[index] = nullptr;
  return OBJ_NIL_TAG;
}

static Object lisp_getc_stream(LispEnv *lisp, Object args) {
  args = LISP_CAR(lisp, args);
  LISP_ASSERT_TYPE(args, FILE_PTR);
  char c = fgetc(lisp->open_streams[OBJ_UNBOX(args)]);
  return OBJ_BOX(c, CHAR);
}

static Object prim_mul(LispEnv *lisp, Object args) {
  i32 product_int = 1;
  Object element = OBJ_INT_TAG;

  /* Integers */
  while (OBJ_TYPE(args) == OBJ_PAIR_TAG && OBJ_TYPE(element) == OBJ_INT_TAG) {
    element = LISP_CAR(lisp, args);
    args = LISP_CDR(lisp, args);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      product_int *= (i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      break;
    default:
      wrong("Wrong type argument to *");
      return OBJ_UNDEFINED_TAG;
    }
  }

  if (OBJ_TYPE(args) == OBJ_NIL_TAG) {
    return OBJ_BOX(product_int, INT);
  }

  /* Floats */
  float product_float = (float)product_int;
  if (OBJ_TYPE(element) == OBJ_FLOAT_TAG) {
    product_float *= lisp_unbox_float(element);
  }

  while (OBJ_TYPE(args) == OBJ_PAIR_TAG) {
    element = LISP_CAR(lisp, args);
    args = LISP_CDR(lisp, args);
    switch (OBJ_TYPE(element)) {
    case OBJ_INT_TAG:
      product_float *= (float)(i32)OBJ_UNBOX(element);
      break;
    case OBJ_FLOAT_TAG:
      /* Repurpose product_int to smuggle out the bits of the 32-bit float. */
      product_float *= lisp_unbox_float(element);
      break;
    default:
      wrong("Wrong type argument to *");
      return OBJ_UNDEFINED_TAG;
    }
  }

  return OBJ_BOX(*(u64 *)&product_float, FLOAT);
}

static Object prim_add2f(LispEnv *lisp, Object args) {
  Object first = LISP_CAR(lisp, args);
  LISP_ASSERT_TYPE(first, FLOAT);
  args = LISP_CDR(lisp, args);
  LISP_ASSERT_TYPE(args, PAIR);
  Object second = LISP_CAR(lisp, args);
  LISP_ASSERT_TYPE(second, FLOAT);
  float result = lisp_unbox_float(first) + lisp_unbox_float(second);
  return OBJ_BOX(*(u64 *)&result, FLOAT);
}

static Object lisp_add_primitive(LispEnv *lisp, s8 name,
                                 InterpreterPrimitive fn) {
  Object name_sym = lisp_intern(lisp, name);
  u32 prim_loc = kh_get(var_syms, lisp->functions, name_sym);
  if (prim_loc != kh_end(lisp->functions)) {
    wrong("Attempt to redefine a function.");
    return OBJ_UNDEFINED_TAG;
  }

  /* Install the primitive function. */
  int absent;
  prim_loc = kh_put(var_syms, lisp->functions, name_sym, &absent);
  if (absent < 0) {
    wrong("Failed to add a primitive: couldn't add it to the function "
          "namespace.");
    return OBJ_UNDEFINED_TAG;
  }
  Object prim_obj = OBJ_BOX(lisp->n_primitives, PRIMITIVE);
  lisp->primitives[lisp->n_primitives++] = fn;
  kh_value(lisp->functions, prim_loc) = prim_obj;

  return prim_obj;
}

LispEnv new_lisp_environment() {
  LispEnv lisp = {0};
  /* A Gigabyte of RAM should do the trick. */
  lisp.memory = new_lisp_memory(1024 * 1024 * 1024);
  lisp.jit.ctxt = gcc_jit_context_acquire();
  lisp.jit.object_type =
      gcc_jit_context_get_type(lisp.jit.ctxt, GCC_JIT_TYPE_UINT64_T);

  static_assert(sizeof(void *) == sizeof(Object), "Can't use clibs/hash.");
  lisp.symbols = kh_init(sym_name);
  lisp.globals = kh_init(var_syms);
  lisp.functions = kh_init(var_syms);

  lisp.reader_macros['\''] = lisp_quote;
  lisp.reader_macros['`'] = lisp_quasiquote;
  lisp.reader_macros[','] = lisp_unquote;
  lisp.reader_macros['?'] = lisp_question_mark;

#define REGISTER_KEYSYM(K) lisp.keysyms.K = lisp_intern(&lisp, s8(#K));
  LISP_KEYSYMS(REGISTER_KEYSYM);
#undef REGISTER_KEYSYM

  lisp.keysyms.if_k = lisp_intern(&lisp, s8("if"));

  lisp_define_global(&lisp, lisp.keysyms.nil, OBJ_NIL_TAG);
  lisp_define_global(&lisp, lisp.keysyms.t, lisp.keysyms.t);
  lisp_define_global(&lisp, lisp.keysyms.stdin,
                     lisp_store_stream_handle(&lisp, stdin));
  lisp_define_global(&lisp, lisp.keysyms.stdout,
                     lisp_store_stream_handle(&lisp, stdout));
  lisp_define_global(&lisp, lisp.keysyms.stderr,
                     lisp_store_stream_handle(&lisp, stderr));

  lisp_add_primitive(&lisp, s8("+2f"), prim_add2f);
  lisp_add_primitive(&lisp, s8("*"), prim_mul);
  lisp_add_primitive(&lisp, s8("fopen"), lisp_open_file);
  lisp_add_primitive(&lisp, s8("fclose"), lisp_close_stream);
  lisp_add_primitive(&lisp, s8("getc"), lisp_getc_stream);

  return lisp;
}

Object lisp_store_string(LispEnv *lisp, s8 string) {
  if (string.len > LISP_MAX_STRING_LENGTH) {
    wrong("Symbol name too long.");
  }

  /* We will include a NULL terminator byte for security. */
  size data_index = lisp_allocate_bytes(lisp, string.len + 1);
  u8 *data = (u8 *)lisp_cell_at(lisp, data_index);
  strncpy((char *)data, (char *)string.data, string.len);

  return OBJ_BOX_INDEX(data_index, string.len, STRING);
}

void lisp_store_object(LispEnv *lisp, Object value) {
  *lisp_cell_at(lisp, lisp_allocate_cells(lisp, 1)) = value;
}

Object lisp_intern(LispEnv *lisp, s8 string) {
  /* s8 string = lisp_string_to_s8(lisp, name); */

  Object symbol = OBJ_UNDEFINED_TAG;
  /* Strings created as slices of others may not be NULL-terminated. */
  if (string.data[string.len] != '\0') {
    /* Convert to NULL-terminated string before passing to hash_get. */
    /* Symbols are encoded as a string reference, with the SYMBOL tag. */
    symbol = lisp_store_string(lisp, string);
    /* We only need to update string.data, since the length remains the same. */
    string.data = lisp_string_to_null_terminated(lisp, symbol);
    symbol = OBJ_REINTERPRET(symbol, SYMBOL);
  }

  khint_t interned_key = kh_get(sym_name, lisp->symbols, (char *)string.data);

  if (interned_key == kh_end(lisp->symbols)) {
    /* Create a symbol object pointing to the symbol name in memory. */
    if (symbol == OBJ_UNDEFINED_TAG) {
      symbol = lisp_store_string(lisp, string);
      string = lisp_string_to_s8(lisp, symbol);
      symbol = OBJ_REINTERPRET(symbol, SYMBOL);
    }

    /* Add 'name' to the symbol table, "interning" it. */
    int absent;
    interned_key =
        kh_put(sym_name, lisp->symbols, (char *)string.data, &absent);
    if (absent < 0) {
      wrong("Failed to intern a symbol: couldn't add it to the symbol table.");
      return OBJ_UNDEFINED_TAG;
    }
    kh_value(lisp->symbols, interned_key) = symbol;
    return symbol;
  } else {
    return kh_value(lisp->symbols, interned_key);
  }
}

Object lisp_cons(LispEnv *lisp, Object car, Object cdr) {
  size location = lisp_allocate_cells(lisp, 2);
  Object *data = lisp_cell_at(lisp, location);
  data[LISP_CAR_INDEX] = car;
  data[LISP_CDR_INDEX] = cdr;

  /* The 2 allows implicit conversion to an array type, if we implement one. */
  return OBJ_BOX_INDEX(location, 2, PAIR);
}

/* list *must* be a pair or nil.  Any sub-structure should be handled correctly.
 */
static void lisp_print_list(LispEnv *lisp, Object list, FILE *stream) {
  fputc('(', stream);
  while (OBJ_TYPE(list) == OBJ_PAIR_TAG) {
    lisp_print(lisp, LISP_CAR(lisp, list), stream);
    list = LISP_CDR(lisp, list);
    if (OBJ_TYPE(list) == OBJ_PAIR_TAG)
      fputc(' ', stream);
  }

  if (OBJ_TYPE(list) != OBJ_NIL_TAG) {
    fputs(" . ", stream);
    lisp_print(lisp, list, stream);
  }
  fputc(')', stream);
}

void lisp_print(LispEnv *lisp, Object object, FILE *stream) {
  /* for (int i = 0; i < depth; i++) { */
  /*   fputc(' ', stream); */
  /* } */
  switch (OBJ_TYPE(object)) {
  case OBJ_FLOAT_TAG: {
    u32 val_bits = (u32)OBJ_UNBOX(object);
    float val = *(float *)&val_bits;
    fprintf(stream, "%f", val);
  } break;
  case OBJ_INT_TAG: {
    i32 val_bits = (i32)OBJ_UNBOX(object);
    fprintf(stream, "%d", val_bits);
  } break;
  case OBJ_STRING_TAG: {
    s8 s = lisp_string_to_s8(lisp, object);
    fputc('"', stream);
    print_s8(stream, s);
    fputc('"', stream);
    /* fprintf(stream, "\"%s\"", object, */
    /*         (char *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object))); */
  } break;
  case OBJ_SYMBOL_TAG:
    fprintf(stream, "%s", (char *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object)));
    break;
  case OBJ_CHAR_TAG:
    fputc('?', stream);
    fputc(OBJ_UNBOX(object), stream);
    break;
  case OBJ_PAIR_TAG:
  case OBJ_NIL_TAG:
    lisp_print_list(lisp, object, stream);
    break;
  case OBJ_FILE_PTR_TAG:
    fprintf(stream, "#<stream:%lx>", OBJ_UNBOX(object));
    break;
  default:
    fprintf(stream, "other type: %lx\n", OBJ_TYPE(object));
    wrong("unprintable string");
    break;
  }
}

static Object lisp_eval_argument_list(LispEnv *lisp, Object arguments,
                                      Object context) {
  if (OBJ_TYPE(arguments) == OBJ_PAIR_TAG) {
    Object tmp = lisp_evaluate(lisp, LISP_CAR(lisp, arguments), context);
    return lisp_cons(
        lisp, tmp,
        lisp_eval_argument_list(lisp, LISP_CDR(lisp, arguments), context));
  } else {
    return lisp_evaluate(lisp, arguments, context);
  }
}

static Object lisp_lookup_variable(LispEnv *lisp, Object symbol,
                                   Object context) {
  LISP_ASSERT_TYPE(symbol, SYMBOL);
  /* TODO: Search in the lexical context. */
  auto global_key = kh_get(var_syms, lisp->globals, symbol);
  if (global_key == kh_end(lisp->globals)) {
    return OBJ_UNDEFINED_TAG;
  }
  return kh_value(lisp->globals, global_key);
}

static Object lisp_lookup_function(LispEnv *lisp, Object symbol,
                                   Object context) {
  LISP_ASSERT_TYPE(symbol, SYMBOL);
  /* TODO: Search in the lexical context. */
  auto function_key = kh_get(var_syms, lisp->functions, symbol);
  if (function_key == kh_end(lisp->functions)) {
    return OBJ_UNDEFINED_TAG;
  }
  return kh_value(lisp->functions, function_key);
}

Object lisp_bind_recur(LispEnv *lisp, Object parameters, Object arguments) {
  Object tmp;
  if (OBJ_TYPE(parameters) == OBJ_NIL_TAG ||
      OBJ_TYPE(arguments) == OBJ_NIL_TAG) {
    if (OBJ_TYPE(parameters) == OBJ_NIL_TAG &&
        OBJ_TYPE(arguments) == OBJ_NIL_TAG) {
      return OBJ_NIL_TAG;
    } else {
      wrong("Wrong number of arguments.");
      return OBJ_UNDEFINED_TAG;
    }
  }
  if (OBJ_TYPE(parameters) == OBJ_PAIR_TAG) {
    if (OBJ_TYPE(arguments) == OBJ_PAIR_TAG) {
      tmp = lisp_cons(lisp, LISP_CAR(lisp, parameters),
                      LISP_CAR(lisp, arguments));
      return lisp_cons(lisp, tmp,
                       lisp_bind_recur(lisp, LISP_CDR(lisp, parameters),
                                       LISP_CDR(lisp, arguments)));
    } else {
      wrong("Too few arguments.");
    }
  }

  /* This allows rest parameters: ((lambda (x . xs)) 1 2 3) binds ((x . 1) (xs .
   * (2 3))) */
  return lisp_cons(lisp, parameters, arguments);
}

/* Create a context (alist) on top of 'context' with 'parameters' bound to
 * 'arguments'. */
Object lisp_bind(LispEnv *lisp, Object parameters, Object arguments,
                 Object context) {
  return lisp_cons(lisp, lisp_bind_recur(lisp, parameters, arguments), context);
}

Object lisp_apply(LispEnv *lisp, Object function, Object arguments) {
  switch (OBJ_TYPE(function)) {
  case OBJ_PRIMITIVE_TAG:
    return lisp->primitives[OBJ_UNBOX(function)](lisp, arguments);
  default:
    wrong("Unsupported type of function.");
    break;
  }
  return OBJ_UNDEFINED_TAG;
}

Object lisp_evaluate(LispEnv *lisp, Object expression, Object context) {
  Object tmp;
  switch (OBJ_TYPE(expression)) {
  case OBJ_NIL_TAG:
  case OBJ_STRING_TAG:
  case OBJ_CHAR_TAG:
  case OBJ_INT_TAG:
  case OBJ_FLOAT_TAG:
  case OBJ_FILE_PTR_TAG:
  case OBJ_PRIMITIVE_TAG:
    return expression;
  case OBJ_SYMBOL_TAG:
    /* Variable name */
    tmp = lisp_lookup_variable(lisp, expression, context);
    if (OBJ_TYPE(tmp) == OBJ_UNDEFINED_TAG) {
      wrong("Undefined variable.");
      break;
    }
    return tmp;
  case OBJ_PAIR_TAG:
    /* Function call: look up name, then apply it to the argument list. */
    /* TODO: Add macros and special forms. */
    tmp = lisp_lookup_function(lisp, LISP_CAR(lisp, expression), context);
    return lisp_apply(
        lisp, tmp,
        lisp_eval_argument_list(lisp, LISP_CDR(lisp, expression), context));
  default:
    wrong("Cannot evaluate object: unhandled type.");
    break;
  }

  return OBJ_UNDEFINED_TAG;
}

Object lisp_eval(LispEnv *lisp, Object expression) {
  return lisp_evaluate(lisp, expression, OBJ_NIL_TAG);
}
