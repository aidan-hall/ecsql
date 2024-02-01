#include "lisp.h"
#include "object.h"
#include "primitives.h"
#include "print.h"
#include "reader.h"
#include <stdarg.h>

void wrong(LispEnv *lisp, const char *message, Object arg) {
  fputs(message, stderr);
  if (OBJ_TYPE(arg) != OBJ_NIL_TAG) {
    fputs(": ", stderr);
    lisp_print(lisp, arg, stderr);
  }
  fputc('\n', stderr);
  longjmp(lisp->error_loc, 1);
}


static const u8 empty_string[] = u8"";

const u8 *lisp_string_to_null_terminated(LispEnv *lisp, Object string) {
  LISP_ASSERT_TYPE(string, STRING);
  s8 string_s8 = lisp_string_to_s8(lisp, string);
  if (string_s8.len == 0) {
    return empty_string;
  }
  if (string_s8.data[string_s8.len] != '\0') {
    string = lisp_store_string(lisp, string_s8);
    return (u8 *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(string));
  } else {
    return string_s8.data;
  }
}

/* Add 'symbol' to 'env' with 'value'. */
Object lisp_add_to_namespace(LispEnv *lisp, khash_t(var_syms) * env,
                             Object symbol, Object value) {
  LISP_ASSERT_TYPE(symbol, SYMBOL);

  u32 iter = kh_get(var_syms, env, symbol);
  if (iter != kh_end(env)) {
    fprintf(stderr, "WARNING: Re-defining a name in the given namespace: ");
    lisp_print(lisp, symbol, stderr);
    fputc('\n', stderr);
  } else {
    int absent;
    iter = kh_put(var_syms, env, symbol, &absent);
    if (absent < 1) {
      WRONG("Failed to allocate a place in the namespace for a new name",
            symbol);
      return OBJ_UNDEFINED_TAG;
    }
  }
  kh_value(env, iter) = value;
  return value;
}

Object lisp_defname(LispEnv *lisp, Object ns, Object symbol, Object value) {
  khash_t(var_syms) * env;
  if (EQ(ns, lisp->keysyms.global)) {
    env = lisp->globals;
  } else if (EQ(ns, lisp->keysyms.function)) {
    env = lisp->functions;
  } else if (EQ(ns, lisp->keysyms.macro)) {
    env = lisp->macros;
  } else {
    WRONG("Invalid namespace", ns);
    return OBJ_UNDEFINED_TAG;
  }

  return lisp_add_to_namespace(lisp, env, symbol, value);
}

static Object lisp_define_global(LispEnv *lisp, Object symbol, Object value) {
  /* We don't need to do anything with the returned (boxed) key, so the tag is
   * irrelevant. */
  /* TODO: Do something better here? We've created an over-general API. */
  symbol = lisp_add_to_namespace(lisp, lisp->globals, symbol, value);
  if (symbol == OBJ_UNDEFINED_TAG) {
    WRONG("Failed to define global variable");
  }
  return value;
}

Object lisp_store_stream_handle(LispEnv *lisp, FILE *stream) {
  size i;
  for (i = 0; i < LISP_MAX_OPEN_STREAMS; i++) {
    if (lisp->open_streams[i] == NULL) {
      lisp->open_streams[i] = stream;
      return OBJ_BOX(i, FILE_PTR);
    }
  }
  WRONG("The maximum number of streams is already open.");
  return OBJ_UNDEFINED_TAG;
}
LispEnv new_lisp_environment() {
  LispEnv lisp = {0};

  if (setjmp(lisp.error_loc) != 0) {
    fprintf(stderr, "VERY BAD: longjmp called on error_loc before a proper "
                    "jump location was set.\n");
    exit(1);
  }

  /* A Gigabyte of RAM should do the trick. */
  lisp.memory = new_lisp_memory(1024 * 1024 * 1024);

  static_assert(sizeof(void *) == sizeof(Object), "Can't use clibs/hash.");
  lisp.symbols = kh_init(sym_name);
  lisp.globals = kh_init(var_syms);
  lisp.functions = kh_init(var_syms);
  lisp.primitive_functions = kh_init(primitives);
  lisp.macros = kh_init(var_syms);

#define REGISTER_KEYSYM(K) lisp.keysyms.K = lisp_intern(&lisp, s8(#K));
  LISP_KEYSYMS(REGISTER_KEYSYM);
#undef REGISTER_KEYSYM

  lisp.keysyms.if_k = lisp_intern(&lisp, s8("if"));
  lisp.keysyms.while_k = lisp_intern(&lisp, s8("while"));
  lisp.keysyms.struct_k = lisp_intern(&lisp, s8("struct"));

  lisp_define_global(&lisp, lisp.keysyms.nil, OBJ_NIL_TAG);
  lisp_define_global(&lisp, lisp.keysyms.t, lisp.keysyms.t);
  lisp_define_global(&lisp, lisp.keysyms.stdin,
                     lisp_store_stream_handle(&lisp, stdin));
  lisp_define_global(&lisp, lisp.keysyms.stdout,
                     lisp_store_stream_handle(&lisp, stdout));
  lisp_define_global(&lisp, lisp.keysyms.stderr,
                     lisp_store_stream_handle(&lisp, stderr));
  lisp_define_global(&lisp, lisp.keysyms.eof, OBJ_BOX(EOF, CHAR));

  lisp_install_primitives(&lisp);
  return lisp;
}

Object lisp_store_string(LispEnv *lisp, s8 string) {
  if (string.len > LISP_MAX_STRING_LENGTH) {
    WRONG("Symbol name too long.");
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
      WRONG("Failed to intern a symbol: couldn't add it to the symbol table.");
      return OBJ_UNDEFINED_TAG;
    }
    kh_value(lisp->symbols, interned_key) = symbol;
    return symbol;
  } else {
    return kh_value(lisp->symbols, interned_key);
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

static Object lisp_assoc(LispEnv *lisp, Object key, Object alist) {
  while (OBJ_TYPE(alist) == OBJ_PAIR_TAG) {
    Object element = LISP_CAR(lisp, alist);
    if (OBJ_TYPE(element) == OBJ_PAIR_TAG)
      if (EQ(LISP_CAR(lisp, element), key))
        return element;
    alist = LISP_CDR(lisp, alist);
  }
  return OBJ_NIL_TAG;
}

static Object *lisp_lookup_variable(LispEnv *lisp, Object symbol,
                                    Object context) {
  LISP_ASSERT_TYPE(symbol, SYMBOL);
  /* TODO: Search in the lexical context. */
  while (OBJ_TYPE(context) == OBJ_PAIR_TAG) {
    Object element = lisp_assoc(lisp, symbol, LISP_CAR(lisp, context));
    if (element != OBJ_NIL_TAG) {
      return LISP_CDR_PLACE(lisp, element);
    }
    context = LISP_CDR(lisp, context);
  }
  khint_t global_key = kh_get(var_syms, lisp->globals, symbol);
  if (global_key == kh_end(lisp->globals)) {
    return NULL;
  }
  return &kh_value(lisp->globals, global_key);
}

/* We don't support lexical function definitions, so this function takes no
 * context argument. */
Object lisp_lookup_function(LispEnv *lisp, Object symbol) {
  LISP_ASSERT_TYPE(symbol, SYMBOL);
  /* TODO: Search in the lexical context. */
  khint_t function_key = kh_get(var_syms, lisp->functions, symbol);
  if (function_key == kh_end(lisp->functions)) {
    WRONG("Undefined function", symbol);
    return OBJ_UNDEFINED_TAG;
  }
  return kh_value(lisp->functions, function_key);
}

Object lisp_bind_recur(LispEnv *lisp, Object parameters, Object arguments) {
  Object tmp;
  if (OBJ_TYPE(parameters) == OBJ_NIL_TAG) {
    if (OBJ_TYPE(arguments) != OBJ_NIL_TAG) {
      WRONG("Too many arguments.");
    } else {
      /* End of parameters and arguments lists */
      return OBJ_NIL_TAG;
    }
  } else if (OBJ_TYPE(parameters) == OBJ_PAIR_TAG) {
    if (OBJ_TYPE(arguments) == OBJ_PAIR_TAG) {
      tmp = lisp_cons(lisp, LISP_CAR(lisp, parameters),
                      LISP_CAR(lisp, arguments));
      return lisp_cons(lisp, tmp,
                       lisp_bind_recur(lisp, LISP_CDR(lisp, parameters),
                                       LISP_CDR(lisp, arguments)));
    } else {
      WRONG("Too few arguments.");
    }
  }

  /* This allows rest parameters: ((lambda (x . xs)) 1 2 3) binds ((x . 1) (xs .
   * (2 3))) */
  return lisp_cons(lisp, lisp_cons(lisp, parameters, arguments), OBJ_NIL_TAG);
}

/* Create a context (alist) on top of 'context' with 'parameters' bound to
 * 'arguments'. */
Object lisp_bind(LispEnv *lisp, Object parameters, Object arguments,
                 Object context) {
  return lisp_cons(lisp, lisp_bind_recur(lisp, parameters, arguments), context);
}

static bool lisp_check_argument_types(LispEnv *lisp, InterpreterPrimitive prim,
                                      Object arguments) {
  bool res = lisp_type_spec_matches(lisp, arguments, prim.argument_types);
  if (!res) {
    WRONG("Invalid argument(s) to primitive function",
          lisp_cons(lisp, prim.id_symbol, arguments));
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
  case OBJ_CLOSURE_TAG: {
    /* (context args body) */
    /* Lambda forms have an implicit top-level progn. */
    Object form = LISP_CDR(lisp, function);

    return lisp_evaluate_sequence(lisp, LISP_CDR(lisp, form),
                                  lisp_bind(lisp, LISP_CAR(lisp, form),
                                            arguments,
                                            LISP_CAR(lisp, function)));
  }
  default:
    WRONG("Unsupported type of function", lisp_type_of(lisp, function));
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

Object lisp_make_closure(LispEnv *lisp, Object body, Object context) {
  if (OBJ_TYPE(body) != OBJ_PAIR_TAG) {
    WRONG("Lambda forms require an argument list and body.");
  }
  Object expanded = lisp_macroexpand_list(lisp, LISP_CDR(lisp, body));
  return OBJ_REINTERPRET(
      lisp_cons(lisp, context, lisp_cons(lisp, LISP_CAR(lisp, body), expanded)),
      CLOSURE);
}

Object lisp_macroexpand_top(LispEnv *lisp, Object expression) {
  if (OBJ_TYPE(expression) != OBJ_PAIR_TAG) {
    return expression;
  }

  /* Recursively macroexpand_top the head. */
  Object macro;
  Object original_car = LISP_CAR(lisp, expression);
  if (EQ(original_car, lisp->keysyms.quote))
    return expression;

  {
    Object car = original_car;
    macro = lisp_macroexpand_top(lisp, car);
    while (!EQ(macro, car)) {
      car = macro;
      macro = lisp_macroexpand_top(lisp, car);
    }
  }

  /* Expand the macro form itself. */
  u32 iter = kh_get(var_syms, lisp->macros, macro);
  if (iter == kh_end(lisp->macros)) {
    /* car of list was not a macro name: just update if the car expression
     * expanded. */
    if (!EQ(macro, original_car))
      expression = lisp_cons(lisp, macro, LISP_CDR(lisp, expression));
  } else {
    expression = lisp_apply(lisp, kh_value(lisp->macros, iter),
                            LISP_CDR(lisp, expression));
  }
  return expression;
}

Object lisp_macroexpand(LispEnv *lisp, Object expression) {
  Object tmp = lisp_macroexpand_top(lisp, expression);
  while (!EQ(tmp, expression)) {
    expression = tmp;
    tmp = lisp_macroexpand_top(lisp, expression);
  }

  /* Macroexpand sub-structure, handling special forms specially.
   * See:
   * https://stackoverflow.com/questions/72865649/how-does-macroexpansion-actually-work-in-lisp
   */

  if (!EQ(OBJ_TYPE(expression), OBJ_PAIR_TAG)) {
    return expression;

  } else {
    Object car = LISP_CAR(lisp, expression);
    Object cdr = LISP_CDR(lisp, expression);
    if (EQ(car, lisp->keysyms.quote)) {
      /* Don't macroexpand a quoted form at all. */
      return expression;

    } else if (EQ(car, lisp->keysyms.lambda) && lisp_length(lisp, cdr) >= 2) {
      /* Don't macroexpand the parameter list of a lambda form. */
      return lisp_cons(
          lisp, lisp->keysyms.lambda,
          lisp_cons(lisp, LISP_CAR(lisp, cdr),
                    lisp_macroexpand_list(lisp, LISP_CDR(lisp, cdr))));

    } else {
      return lisp_macroexpand_list(lisp, expression);
    }
  }
}

Object lisp_macroexpand_list(LispEnv *lisp, Object list) {
  if (OBJ_TYPE(list) == OBJ_PAIR_TAG) {
    return lisp_cons(lisp, lisp_macroexpand(lisp, LISP_CAR(lisp, list)),
                     lisp_macroexpand_list(lisp, LISP_CDR(lisp, list)));
  } else {
    return list;
  }
}

Object lisp_evaluate(LispEnv *lisp, Object expression, Object context) {
  /* fprintf(stderr, "Macroexpanded: "); */
  /* lisp_print(lisp, expression, stderr); */
  /* fputc('\n', stderr); */

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
  case OBJ_VECTOR_TAG:
    return expression;
  case OBJ_SYMBOL_TAG:
    /* Variable name */
    tmp_ptr = lisp_lookup_variable(lisp, expression, context);
    if (tmp_ptr == NULL) {
      WRONG("Undefined variable", expression);
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
      return lisp_lookup_function(lisp, LISP_CAR(lisp, tmp));

    } else if (EQ(tmp, lisp->keysyms.progn)) {
      return lisp_evaluate_sequence(lisp, LISP_CDR(lisp, expression), context);

    } else if (EQ(tmp, lisp->keysyms.and)) {
      return lisp_and(lisp, LISP_CDR(lisp, expression), context);

    } else if (EQ(tmp, lisp->keysyms.or)) {
      return lisp_or(lisp, LISP_CDR(lisp, expression), context);

    } else if (EQ(tmp, lisp->keysyms.if_k)) {
      tmp = LISP_CDR(lisp, expression);
      if (lisp_length(lisp, tmp) < 2) {
        WRONG("Need at least the CONDITION and THEN clauses for an if form.");
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
        WRONG("Need at least the CONDITION clause for a while form.");
        return OBJ_UNDEFINED_TAG;
      }

      Object test = LISP_CAR(lisp, tmp);
      tmp = LISP_CDR(lisp, tmp);
      while (lisp_true(lisp_evaluate(lisp, test, context))) {
        lisp_evaluate_sequence(lisp, tmp, context);
      }
      /* Follow Emacs Lisp's behaviour. */
      return OBJ_NIL_TAG;

    } else if (EQ(tmp, lisp->keysyms.setq)) {
      tmp = LISP_CDR(lisp, expression);
      if (lisp_length(lisp, tmp) != 2) {
        WRONG("Currently we only accept 2 arguments for setq: VARIABLE and "
              "VALUE.");
        return OBJ_UNDEFINED_TAG;
      }
      tmp_ptr = lisp_lookup_variable(lisp, LISP_CAR(lisp, tmp), context);
      if (tmp_ptr == NULL) {
        WRONG("Undefined variable.");
        return OBJ_UNDEFINED_TAG;
      }
      return *tmp_ptr = lisp_evaluate(lisp, LISP_CAR(lisp, LISP_CDR(lisp, tmp)),
                                      context);
    } else if (EQ(tmp, lisp->keysyms.lambda)) {
      return lisp_make_closure(lisp, LISP_CDR(lisp, expression), context);

    } else if (OBJ_TYPE(tmp) == OBJ_PAIR_TAG) {
      if (!EQ(LISP_CAR(lisp, tmp), lisp->keysyms.lambda)) {
        WRONG("Invalid function form.");
        return OBJ_UNDEFINED_TAG;
      }
      return lisp_apply(
          lisp, lisp_make_closure(lisp, LISP_CDR(lisp, tmp), context),
          lisp_eval_argument_list(lisp, LISP_CDR(lisp, expression), context));
    } else {
      tmp = lisp_lookup_function(lisp, LISP_CAR(lisp, expression));
      return lisp_apply(
          lisp, tmp,
          lisp_eval_argument_list(lisp, LISP_CDR(lisp, expression), context));
    }
  default:
    WRONG("Cannot evaluate object: unhandled type",
          lisp_type_of(lisp, expression));
    break;
  }

  return OBJ_UNDEFINED_TAG;
}

Object lisp_eval(LispEnv *lisp, Object expression) {
  return lisp_evaluate(lisp, expression, OBJ_NIL_TAG);
}

static Object lisp_list_recurse(LispEnv *lisp, va_list args) {
  Object arg = va_arg(args, Object);
  if (OBJ_TYPE(arg) == OBJ_NIL_TAG) {
    return arg;
  } else {
    return lisp_cons(lisp, arg, lisp_list_recurse(lisp, args));
  }
}

Object lisp_list(LispEnv *lisp, ...) {
  va_list args;
  va_start(args, lisp);
  Object res = lisp_list_recurse(lisp, args);
  va_end(args);
  return res;
}
