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

/* Add 'symbol' to 'env' with 'value'. */
static Object lisp_add_to_namespace(LispEnv *lisp, khash_t(var_syms) * env,
                                    Object symbol, Object value) {
  LISP_ASSERT_TYPE(symbol, SYMBOL);

  u32 iter = kh_get(var_syms, env, symbol);
  if (iter != kh_end(env)) {
    return OBJ_UNDEFINED_TAG;
  }

  int absent;
  iter = kh_put(var_syms, env, symbol, &absent);
  if (absent < 1) {
    return OBJ_UNDEFINED_TAG;
  }
  kh_value(env, iter) = value;
  return value;
}

static Object lisp_define_global(LispEnv *lisp, Object symbol, Object value) {
  /* We don't need to do anything with the returned (boxed) key, so the tag is
   * irrelevant. */
  /* TODO: Do something better here? We've created an over-general API. */
  symbol = lisp_add_to_namespace(lisp, lisp->globals, symbol, value);
  if (symbol == OBJ_UNDEFINED_TAG) {
    wrong("Failed to define global variable.");
  }
  return value;
}

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

static Object lisp_car(LispEnv *lisp, Object args) {
  Object pair = LISP_CAR(lisp, args);
  if (OBJ_TYPE(pair) == OBJ_NIL_TAG)
    return OBJ_NIL_TAG;

  LISP_ASSERT_TYPE(pair, PAIR);
  return LISP_CAR(lisp, pair);
}

static Object lisp_cdr(LispEnv *lisp, Object args) {
  Object pair = LISP_CAR(lisp, args);
  if (OBJ_TYPE(pair) == OBJ_NIL_TAG)
    return OBJ_NIL_TAG;

  LISP_ASSERT_TYPE(pair, PAIR);
  return LISP_CDR(lisp, pair);
}

static Object prim_cons(LispEnv *lisp, Object args) {
  return lisp_cons(lisp, LISP_CAR(lisp, args),
                   LISP_CAR(lisp, LISP_CDR(lisp, args)));
}

static Object prim_read(LispEnv *lisp, Object args) {
  FILE *stream = lisp->open_streams[OBJ_UNBOX(LISP_CAR(lisp, args))];
  return lisp_read(lisp, stream);
}
static Object prim_list(LispEnv *lisp, Object args) { return args; }

static Object prim_eq(LispEnv *lisp, Object args) {
  return lisp_bool(
      lisp, EQ(LISP_CAR(lisp, args), LISP_CAR(lisp, LISP_CDR(lisp, args))));
}

static Object prim_eql(LispEnv *lisp, Object args) {
  Object a = LISP_CAR(lisp, args);
  Object b = LISP_CAR(lisp, LISP_CDR(lisp, args));
  if (OBJ_TYPE(a) == OBJ_FLOAT_TAG) {
    if (OBJ_TYPE(b) == OBJ_FLOAT_TAG) {
      return lisp_bool(lisp, lisp_unbox_float(a) == lisp_unbox_float(b));
    } else if (OBJ_TYPE(b) == OBJ_INT_TAG) {
      return lisp_bool(lisp, lisp_unbox_float(a) == (float)(i32)OBJ_UNBOX(b));
    }
  } else if (OBJ_TYPE(a) == OBJ_INT_TAG && OBJ_TYPE(b) == OBJ_FLOAT_TAG) {
    return lisp_bool(lisp, (float)(i32)OBJ_UNBOX(a) == lisp_unbox_float(b));
  }

  return lisp_bool(lisp, EQ(a, b));
}

static Object prim_assert(LispEnv *lisp, Object args) {
  if (lisp_false(LISP_CAR(lisp, args))) {
    wrong("Assertion failure in lisp!");
    return OBJ_NIL_TAG;
  }

  return lisp->keysyms.t;
}

static Object lisp_open_file(LispEnv *lisp, Object args) {
  Object filename = LISP_CAR(lisp, args);
  args = LISP_CDR(lisp, args);
  Object mode = LISP_CAR(lisp, args);
  char *filename_s = (char *)lisp_string_to_null_terminated(lisp, filename);
  char *mode_s = (char *)lisp_string_to_null_terminated(lisp, mode);
  FILE *stream = fopen(filename_s, mode_s);
  if (stream == nullptr) {
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
  lisp->open_streams[index] = nullptr;
  return OBJ_NIL_TAG;
}

static Object lisp_getc_stream(LispEnv *lisp, Object args) {
  args = LISP_CAR(lisp, args);
  char c = fgetc(lisp->open_streams[OBJ_UNBOX(args)]);
  return OBJ_BOX(c, CHAR);
}

static size lisp_length(LispEnv *lisp, Object list) {
  size length = 0;
  while (OBJ_TYPE(list) == OBJ_PAIR_TAG) {
    length += 1;
    list = LISP_CDR(lisp, list);
  }
  LISP_ASSERT_TYPE(list, NIL);
  return length;
}

static Object lisp_quit(LispEnv *lisp, Object args) {
  exit(0);
  return OBJ_UNDEFINED_TAG;
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

  if (OBJ_TYPE(args) == OBJ_NIL_TAG && OBJ_TYPE(element) != OBJ_FLOAT_TAG) {
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

static Object prim_print(LispEnv *lisp, Object args) {
  lisp_print(lisp, LISP_CAR(lisp, args), stdout);
  fputc('\n', stdout);
  return OBJ_NIL_TAG;
}

static Object prim_type_of(LispEnv *lisp, Object args) {
  return lisp_type_of(lisp, LISP_CAR(lisp, args));
}

static Object lisp_add_primitive(LispEnv *lisp, Object name_sym, Object params, PrimitiveFunction fn,
                                 khash_t(primitives) * primitives) {
  Object prim_obj = OBJ_REINTERPRET_RAWTAG(name_sym, OBJ_PRIMITIVE_TAG);

  InterpreterPrimitive prim;
  prim.fn = fn;

  /* The input argument type list may be stack-allocated. */
  prim.argument_types = params;

  u32 fn_iter = kh_get(primitives, primitives, prim_obj);
  if (fn_iter != kh_end(primitives)) {
    wrong("Attempt to redefine primitive function.");
    return OBJ_UNDEFINED_TAG;
  }
  int absent;
  fn_iter = kh_put(primitives, primitives, prim_obj, &absent);
  if (absent < 1) {
    wrong("Failed to define primitive function.");
    return OBJ_UNDEFINED_TAG;
  }

  kh_value(primitives, fn_iter) = prim;

  Object sym_iter =
      lisp_add_to_namespace(lisp, lisp->functions, name_sym, prim_obj);
  if (sym_iter == OBJ_UNDEFINED_TAG) {
    wrong("Failed to add a primitive function name.");
    return sym_iter;
  }

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
  lisp.primitive_functions = kh_init(primitives);

  lisp.reader_macros['\''] = lisp_quote;
  lisp.reader_macros['`'] = lisp_quasiquote;
  lisp.reader_macros[','] = lisp_unquote;
  lisp.reader_macros['?'] = lisp_question_mark;

#define REGISTER_KEYSYM(K) lisp.keysyms.K = lisp_intern(&lisp, s8(#K));
  LISP_KEYSYMS(REGISTER_KEYSYM);
#undef REGISTER_KEYSYM

  lisp.keysyms.if_k = lisp_intern(&lisp, s8("if"));
  lisp.keysyms.while_k = lisp_intern(&lisp, s8("while"));

  lisp_define_global(&lisp, lisp.keysyms.nil, OBJ_NIL_TAG);
  lisp_define_global(&lisp, lisp.keysyms.t, lisp.keysyms.t);
  lisp_define_global(&lisp, lisp.keysyms.stdin,
                     lisp_store_stream_handle(&lisp, stdin));
  lisp_define_global(&lisp, lisp.keysyms.stdout,
                     lisp_store_stream_handle(&lisp, stdout));
  lisp_define_global(&lisp, lisp.keysyms.stderr,
                     lisp_store_stream_handle(&lisp, stderr));
#define OBJSX(S) OBJS(&lisp, S)
#define DEFPRIMFUN(NAME, SPEC, FUN) lisp_add_primitive(&lisp, OBJSX(NAME), OBJSX(SPEC), FUN, lisp.primitive_functions)
  DEFPRIMFUN("+2f", "(f32 f32)", prim_add2f);
  DEFPRIMFUN("*", "t", prim_mul);
  DEFPRIMFUN("quit", "()", lisp_quit);
  DEFPRIMFUN("fopen", "(string string)", lisp_open_file);
  DEFPRIMFUN("fclose", "(file)", lisp_close_stream);
  DEFPRIMFUN("getc", "(file)", lisp_getc_stream);
  DEFPRIMFUN("read-stream", "(file)", prim_read);
  DEFPRIMFUN("car", "(pair)", lisp_car);
  DEFPRIMFUN("cdr", "(pair)", lisp_cdr);
  DEFPRIMFUN("cons", "(t t)", prim_cons);
  DEFPRIMFUN("list", "t", prim_list);
  DEFPRIMFUN("eq", "(t t)", prim_eq);
  DEFPRIMFUN("eql", "(t t)", prim_eql);
  DEFPRIMFUN("assert", "(t)", prim_assert);
  DEFPRIMFUN("print", "(t)", prim_print);
  DEFPRIMFUN("type-of", "(t)", prim_type_of);
#undef DEFPRIMFUN
#undef OBJSX
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
  switch ((enum ObjectTag) OBJ_TYPE(object)) {
  case OBJ_FLOAT_TAG:
    fprintf(stream, "%f", lisp_unbox_float(object));
    break;
  case OBJ_INT_TAG:
    fprintf(stream, "%d", (i32)OBJ_UNBOX(object));
    break;
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
    fprintf(stream, "#(stream %ld)", OBJ_UNBOX(object));
    break;
  case OBJ_UNDEFINED_TAG:
    fprintf(stream, "#undefined");
    break;
  case OBJ_PRIMITIVE_TAG:
    lisp_print(lisp, lisp_list(lisp, lisp->keysyms.function, OBJ_REINTERPRET(object, SYMBOL), OBJ_NIL_TAG), stream);
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

static Object *lisp_lookup_variable(LispEnv *lisp, Object symbol,
                                    Object context) {
  LISP_ASSERT_TYPE(symbol, SYMBOL);
  /* TODO: Search in the lexical context. */
  khint_t global_key = kh_get(var_syms, lisp->globals, symbol);
  if (global_key == kh_end(lisp->globals)) {
    return nullptr;
  }
  return &kh_value(lisp->globals, global_key);
}

static Object lisp_lookup_function(LispEnv *lisp, Object symbol,
                                   Object context) {
  LISP_ASSERT_TYPE(symbol, SYMBOL);
  /* TODO: Search in the lexical context. */
  khint_t function_key = kh_get(var_syms, lisp->functions, symbol);
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

Object lisp_type_of(LispEnv *lisp, Object obj) {
  /* This explicit cast triggers a warning if we miss a case here. */
  switch ((enum ObjectTag)OBJ_TYPE(obj)) {
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
  }
  wrong("Invalid type tag of object.");
  return OBJ_UNDEFINED_TAG;
}

static inline bool lisp_type_spec_matches(LispEnv *lisp, Object value,
                                          Object spec) {

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

static bool lisp_check_argument_types(LispEnv *lisp, InterpreterPrimitive prim,
                                      Object arguments) {
  bool res = lisp_type_spec_matches(lisp, arguments, prim.argument_types);
  if (!res) {
    wrong("Invalid arguments to primitive function.");
  }
  return res;
}

Object lisp_apply(LispEnv *lisp, Object function, Object arguments) {
  switch (OBJ_TYPE(function)) {
  case OBJ_PRIMITIVE_TAG: {

    InterpreterPrimitive prim =
        kh_value(lisp->primitive_functions,
                 kh_get(primitives, lisp->primitive_functions, function));
    if (!lisp_check_argument_types(lisp, prim, arguments)) {
      return OBJ_UNDEFINED_TAG;
    }
    return prim.fn(lisp, arguments);
  }
  default:
    wrong("Unsupported type of function.");
    break;
  }
  return OBJ_UNDEFINED_TAG;
}

Object lisp_or(LispEnv *lisp, Object sequence, Object context) {
  Object statement;
  if (OBJ_TYPE(sequence) == OBJ_NIL_TAG) {
    return OBJ_NIL_TAG;
  }
  do {
    statement = LISP_CAR(lisp, sequence);
    sequence = LISP_CDR(lisp, sequence);
    statement = lisp_evaluate(lisp, statement, context);
  } while (OBJ_TYPE(sequence) == OBJ_PAIR_TAG && lisp_false(statement));
  return statement;
}

Object lisp_and(LispEnv *lisp, Object sequence, Object context) {
  Object statement;
  if (OBJ_TYPE(sequence) == OBJ_NIL_TAG) {
    return lisp->keysyms.t;
  }
  do {
    statement = LISP_CAR(lisp, sequence);
    sequence = LISP_CDR(lisp, sequence);
    statement = lisp_evaluate(lisp, statement, context);
  } while (OBJ_TYPE(sequence) == OBJ_PAIR_TAG && lisp_true(statement));
  return statement;
}

/* Evaluate the forms of 'sequence' in order, and return the result of the last.
 */
Object lisp_evaluate_sequence(LispEnv *lisp, Object sequence, Object context) {
  Object statement;
  if (OBJ_TYPE(sequence) == OBJ_NIL_TAG) {
    return OBJ_NIL_TAG;
  }
  do {
    statement = LISP_CAR(lisp, sequence);
    sequence = LISP_CDR(lisp, sequence);
    statement = lisp_evaluate(lisp, statement, context);
  } while (OBJ_TYPE(sequence) == OBJ_PAIR_TAG);
  return statement;
}

Object lisp_evaluate_quasiquoted(LispEnv *lisp, Object expression,
                                 Object context, int level) {
  if (level == 0) {
    return lisp_evaluate(lisp, expression, context);
  }

  Object tmp;

  if (OBJ_TYPE(expression) == OBJ_PAIR_TAG) {
    tmp = LISP_CAR(lisp, expression);
    /* TODO: Error handling here. */
    /* static_assert(false, "This is completely fucked."); */
    if (EQ(tmp, lisp->keysyms.unquote)) {
      tmp = lisp_evaluate_quasiquoted(
          lisp, LISP_CAR(lisp, LISP_CDR(lisp, expression)), context, level - 1);
      if (level > 1) {
        tmp = lisp_cons(lisp, lisp->keysyms.unquote,
                        lisp_cons(lisp, tmp, OBJ_NIL_TAG));
      }
      return tmp;
    } else if (EQ(tmp, lisp->keysyms.quasiquote)) {
      tmp = lisp_evaluate_quasiquoted(
          lisp, LISP_CAR(lisp, LISP_CDR(lisp, expression)), context, level + 1);
      return lisp_cons(lisp, lisp->keysyms.quasiquote,
                       lisp_cons(lisp, tmp, OBJ_NIL_TAG));
    } else {
      return lisp_cons(lisp,
                       lisp_evaluate_quasiquoted(lisp, tmp, context, level),
                       lisp_evaluate_quasiquoted(
                           lisp, LISP_CDR(lisp, expression), context, level));
    }
  } else {
    return expression;
  }
}

Object lisp_evaluate(LispEnv *lisp, Object expression, Object context) {
  Object tmp;
  Object *tmp_ptr;
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
    tmp_ptr = lisp_lookup_variable(lisp, expression, context);
    if (tmp_ptr == nullptr) {
      wrong("Undefined variable.");
      break;
    }
    return *tmp_ptr;
  case OBJ_PAIR_TAG:
    /* Function call: look up name, then apply it to the argument list. */
    /* TODO: Add macros. */
    tmp = LISP_CAR(lisp, expression);

    if (EQ(tmp, lisp->keysyms.quote)) {
      tmp = LISP_CDR(lisp, expression);
      LISP_ASSERT_TYPE(tmp, PAIR);
      return LISP_CAR(lisp, tmp);
    
    } else if (EQ(tmp, lisp->keysyms.function)) {
      tmp = LISP_CDR(lisp, expression);
      LISP_ASSERT_TYPE(tmp, PAIR);
      return lisp_lookup_function(lisp, LISP_CAR(lisp, tmp), context);

    } else if (EQ(tmp, lisp->keysyms.quasiquote)) {
      return lisp_evaluate_quasiquoted(
          lisp, LISP_CAR(lisp, LISP_CDR(lisp, expression)), context, 1);

    } else if (EQ(tmp, lisp->keysyms.progn)) {
      return lisp_evaluate_sequence(lisp, LISP_CDR(lisp, expression), context);

    } else if (EQ(tmp, lisp->keysyms.and)) {
      return lisp_and(lisp, LISP_CDR(lisp, expression), context);

    } else if (EQ(tmp, lisp->keysyms.or)) {
      return lisp_or(lisp, LISP_CDR(lisp, expression), context);

    } else if (EQ(tmp, lisp->keysyms.if_k)) {
      tmp = LISP_CDR(lisp, expression);
      if (lisp_length(lisp, tmp) < 2) {
        wrong("Need at least the CONDITION and THEN clauses for an if form.");
        return OBJ_UNDEFINED_TAG;
      }

      if (lisp_true(lisp_evaluate(lisp, LISP_CAR(lisp, tmp), context))) {
        return lisp_evaluate(lisp, LISP_CAR(lisp, LISP_CDR(lisp, tmp)),
                             context);
      } else {
        return lisp_evaluate_sequence(lisp, LISP_CDR(lisp, LISP_CDR(lisp, tmp)),
                                      context);
      }

    } else if (EQ(tmp, lisp->keysyms.while_k)) {
      tmp = LISP_CDR(lisp, expression);

      if (lisp_length(lisp, tmp) < 1) {
        wrong("Need at least the CONDITION clause for a while form.");
        return OBJ_UNDEFINED_TAG;
      }

      Object test = LISP_CAR(lisp, tmp);
      tmp = LISP_CDR(lisp, tmp);
      while (lisp_true(lisp_evaluate(lisp, test, context))) {
        lisp_evaluate_sequence(lisp, tmp, context);
      }
      /* Follow Emacs Lisp's behaviour. */
      return OBJ_NIL_TAG;

    } else if (EQ(tmp, lisp->keysyms.define)) {
      tmp = LISP_CDR(lisp, expression);
      if (lisp_length(lisp, tmp) != 2) {
        wrong("Must have exactly 2 arguments to definition form: NAME and "
              "VALUE.");
        return OBJ_UNDEFINED_TAG;
      }
      return lisp_define_global(
          lisp, LISP_CAR(lisp, tmp),
          lisp_evaluate(lisp, LISP_CAR(lisp, LISP_CDR(lisp, tmp)), context));

    } else if (EQ(tmp, lisp->keysyms.setq)) {
      tmp = LISP_CDR(lisp, expression);
      if (lisp_length(lisp, tmp) != 2) {
        wrong("Currently we only accept 2 arguments for setq: VARIABLE and "
              "VALUE.");
        return OBJ_UNDEFINED_TAG;
      }
      tmp_ptr = lisp_lookup_variable(lisp, LISP_CAR(lisp, tmp), context);
      if (tmp_ptr == nullptr) {
        wrong("Undefined variable.");
        return OBJ_UNDEFINED_TAG;
      }
      return *tmp_ptr = lisp_evaluate(lisp, LISP_CAR(lisp, LISP_CDR(lisp, tmp)),
                                      context);
    } else {
      tmp = lisp_lookup_function(lisp, LISP_CAR(lisp, expression), context);
      return lisp_apply(
          lisp, tmp,
          lisp_eval_argument_list(lisp, LISP_CDR(lisp, expression), context));
    }
  default:
    printf("%lx\n", expression);
    wrong("Cannot evaluate object: unhandled type.");
    break;
  }

  return OBJ_UNDEFINED_TAG;
}

Object lisp_eval(LispEnv *lisp, Object expression) {
  return lisp_evaluate(lisp, expression, OBJ_NIL_TAG);
}
