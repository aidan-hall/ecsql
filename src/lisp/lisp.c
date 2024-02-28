#include "ecs/ecs.h"
#include <lisp/lisp.h>
#include <lisp/object.h>
#include <lisp/primitives.h>
#include <lisp/print.h>
#include <lisp/reader.h>
#include <stdarg.h>
#include <threads.h>

void wrong(LispEnv *lisp, const char *message, Object arg) {
  fputs(message, stderr);
  if (!EQ(arg, NIL)) {
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

  u32 iter = kh_get(var_syms, env, symbol.bits);
  if (iter != kh_end(env)) {
    fprintf(stderr, "WARNING: Re-defining a name in the given namespace: ");
    lisp_print(lisp, symbol, stderr);
    fputc('\n', stderr);
  } else {
    int absent;
    iter = kh_put(var_syms, env, symbol.bits, &absent);
    if (absent < 1) {
      WRONG("Failed to allocate a place in the namespace for a new name",
            symbol);
      return UNDEFINED;
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
  } else if (EQ(ns, lisp->keysyms.struct_k)) {
    env = lisp->structs;
  } else {
    WRONG("Invalid namespace", ns);
    return UNDEFINED;
  }

  return lisp_add_to_namespace(lisp, env, symbol, value);
}

static Object lisp_define_global(LispEnv *lisp, Object symbol, Object value) {
  /* We don't need to do anything with the returned (boxed) key, so the tag is
   * irrelevant. */
  /* TODO: Do something better here? We've created an over-general API. */
  symbol = lisp_add_to_namespace(lisp, lisp->globals, symbol, value);
  if (EQ(symbol, UNDEFINED)) {
    WRONG("Failed to define global variable");
  }
  return value;
}

Object lisp_store_stream_handle(LispEnv *lisp, FILE *stream) {
  size i;
  for (i = 0; i < LISP_MAX_OPEN_STREAMS; i++) {
    if (lisp->open_streams[i] == NULL) {
      /* Store the stream in the first available slot. */
      lisp->open_streams[i] = stream;
      return OBJ_BOX(i, FILE_PTR);
    } else if (lisp->open_streams[i] == stream) {
      /* If that stream is already stored, reuse the slot it's in. */
      return OBJ_BOX(i, FILE_PTR);
    }
  }
  WRONG("The maximum number of streams is already open.");
  return UNDEFINED;
}
LispEnv new_lisp_environment() {
  LispEnv lisp = {0};
  if(mtx_init(&lisp.memory_lock, mtx_plain) != thrd_success) {
    fprintf(stderr, "Failed to initialise memory lock.\n");
    exit(1);
  }

  if (setjmp(lisp.error_loc) != 0) {
    fprintf(stderr, "VERY BAD: longjmp called on error_loc before a proper "
                    "jump location was set.\n");
    exit(1);
  }

  /* 8 Gigabytes of RAM should do the trick. */
  lisp.memory = new_lisp_memory(8L * 1024 * 1024 * 1024);

  static_assert(sizeof(void *) == sizeof(Object), "Can't use clibs/hash.");
  lisp.symbols = kh_init(sym_name);
  lisp.globals = kh_init(var_syms);
  lisp.functions = kh_init(var_syms);
  lisp.struct_ids = kh_init(struct_ids);
  lisp.primitive_functions = kh_init(primitives);
  lisp.macros = kh_init(var_syms);
  lisp.structs = kh_init(var_syms);

#define REGISTER_KEYSYM(K) lisp.keysyms.K = lisp_intern(&lisp, s8(#K));
  LISP_KEYSYMS(REGISTER_KEYSYM);
#undef REGISTER_KEYSYM

  lisp.keysyms.if_k = lisp_intern(&lisp, s8("if"));
  lisp.keysyms.while_k = lisp_intern(&lisp, s8("while"));
  lisp.keysyms.struct_k = lisp_intern(&lisp, s8("struct"));
  lisp.keysyms.not_k = lisp_intern(&lisp, s8("not"));
  lisp.keysyms.star_k = lisp_intern(&lisp, s8("*"));

  lisp_define_global(&lisp, lisp.keysyms.nil, NIL);
  lisp_define_global(&lisp, lisp.keysyms.t, lisp.keysyms.t);
  lisp_define_global(&lisp, lisp.keysyms.stdin,
                     lisp_store_stream_handle(&lisp, stdin));
  lisp_define_global(&lisp, lisp.keysyms.stdout,
                     lisp_store_stream_handle(&lisp, stdout));
  lisp_define_global(&lisp, lisp.keysyms.stderr,
                     lisp_store_stream_handle(&lisp, stderr));
  lisp_define_global(&lisp, lisp.keysyms.eof, OBJ_BOX(EOF, CHAR));

  lisp_install_primitives(&lisp);

  FILE *load_file = fopen("lisp/load.lisp", "r");
  if (load_file == NULL) {
    fprintf(stderr, "Couldn't open file for load function.\n");
    exit(1);
  }
  lisp_eval(&lisp, lisp_read(&lisp, load_file));
  fclose(load_file);

  /* ECS Initialisation */
  lisp.world = init_world();
  if (lisp.world == NULL) {
    wrong(&lisp, "Failed to initialise ECS world.\n", NIL);
  }
  WorldComponents *world_components = ecs_world_components(lisp.world);

  lisp.comp.lisp_component_storage =
      ECS_NEW_COMPONENT(lisp.world, struct LispComponentStorage);
  if (!ecs_set_name(lisp.world, lisp.comp.lisp_component_storage,
                    SYM(&lisp, "LispStorage"))) {
    wrong(&lisp, "Failed to set name for LispStorage component", NIL);
  }

  lisp.comp.lisp_system = ecs_new(lisp.world);
  if (!ecs_set_name(lisp.world, lisp.comp.lisp_system,
                    SYM(&lisp, "LispSystem"))) {
    wrong(&lisp, "Failed to set name for LispSystem component", NIL);
  }


  assert(ecs_set_name(lisp.world, world_components->storage,
                      SYM(&lisp, "Storage")));
  assert(
      ecs_set_name(lisp.world, world_components->system, SYM(&lisp, "System")));
  assert(ecs_set_name(lisp.world, world_components->nwise_system,
                      SYM(&lisp, "NWiseSystem")));
  assert(ecs_set_name(lisp.world, world_components->self_join_system,
                      SYM(&lisp, "SelfJoin")));
  assert(ecs_set_name(lisp.world, world_components->system_data,
                      SYM(&lisp, "SystemData")));

  /* This component should be a Lisp data component, so it has to be initialised
   * here, rather than in init_world with the rest of them. */
  world_components->query = lisp_new_ecs_component(&lisp, SYM(&lisp, "pair"));
  assert(
      ecs_set_name(lisp.world, world_components->query, SYM(&lisp, "Query")));

  /* Load the standard library */
  lisp_eval(&lisp, OBJS(&lisp, "(load \"lisp/stdlib.lisp\")"));
  lisp.keysyms.print_struct =
      lisp_lookup_function(&lisp, SYM(&lisp, "prin1-struct-to"));

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

  Object symbol = UNDEFINED;
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

  if (interned_key != kh_end(lisp->symbols)) {
    return kh_value(lisp->symbols, interned_key);
  }
  /* Create a symbol object pointing to the symbol name in memory. */
  if (EQ(symbol, UNDEFINED)) {
    symbol = lisp_store_string(lisp, string);
    string = lisp_string_to_s8(lisp, symbol);
    symbol = OBJ_REINTERPRET(symbol, SYMBOL);
  }

  /* Add 'name' to the symbol table, "interning" it. */
  int absent;
  interned_key = kh_put(sym_name, lisp->symbols, (char *)string.data, &absent);
  if (absent < 0) {
    WRONG("Failed to intern a symbol: couldn't add it to the symbol table.");
    return UNDEFINED;
  }
  kh_value(lisp->symbols, interned_key) = symbol;
  return symbol;
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

Object lisp_assoc(LispEnv *lisp, Object key, Object alist) {
  while (OBJ_TYPE(alist) == OBJ_PAIR_TAG) {
    Object element = LISP_CAR(lisp, alist);
    if (OBJ_TYPE(element) == OBJ_PAIR_TAG)
      if (EQ(LISP_CAR(lisp, element), key))
        return element;
    alist = LISP_CDR(lisp, alist);
  }
  return NIL;
}

static Object *lisp_lookup_variable(LispEnv *lisp, Object symbol,
                                    Object context) {
  LISP_ASSERT_TYPE(symbol, SYMBOL);
  /* TODO: Search in the lexical context. */
  while (OBJ_TYPE(context) == OBJ_PAIR_TAG) {
    Object element = lisp_assoc(lisp, symbol, LISP_CAR(lisp, context));
    if (!EQ(element, NIL)) {
      return LISP_CDR_PLACE(lisp, element);
    }
    context = LISP_CDR(lisp, context);
  }
  khint_t global_key = kh_get(var_syms, lisp->globals, symbol.bits);
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
  khint_t function_key = kh_get(var_syms, lisp->functions, symbol.bits);
  if (function_key == kh_end(lisp->functions)) {
    WRONG("Undefined function", symbol);
    return UNDEFINED;
  }
  return kh_value(lisp->functions, function_key);
}

Object lisp_bind_recur(LispEnv *lisp, Object parameters, Object arguments) {
  Object tmp;
  if (EQ(parameters, NIL)) {
    if (!EQ(arguments, NIL)) {
      WRONG("Too many arguments.");
    } else {
      /* End of parameters and arguments lists */
      return NIL;
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
  return lisp_cons(lisp, lisp_cons(lisp, parameters, arguments), NIL);
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
                 kh_get(primitives, lisp->primitive_functions, function.bits));
    if (!lisp_check_argument_types(lisp, prim, arguments)) {
      return UNDEFINED;
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
  return UNDEFINED;
}

Object lisp_or(LispEnv *lisp, Object sequence, Object context) {
  Object statement;
  if (EQ(sequence, NIL)) {
    return NIL;
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
  if (EQ(sequence, NIL)) {
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
  if (EQ(sequence, NIL)) {
    return NIL;
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
  u32 iter = kh_get(var_syms, lisp->macros, macro.bits);
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

  if (OBJ_TYPE(expression) != OBJ_PAIR_TAG) {
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
        return UNDEFINED;
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
        return UNDEFINED;
      }

      Object test = LISP_CAR(lisp, tmp);
      tmp = LISP_CDR(lisp, tmp);
      while (lisp_true(lisp_evaluate(lisp, test, context))) {
        lisp_evaluate_sequence(lisp, tmp, context);
      }
      /* Follow Emacs Lisp's behaviour. */
      return NIL;

    } else if (EQ(tmp, lisp->keysyms.setq)) {
      tmp = LISP_CDR(lisp, expression);
      if (lisp_length(lisp, tmp) != 2) {
        WRONG("Currently we only accept 2 arguments for setq: VARIABLE and "
              "VALUE.");
        return UNDEFINED;
      }
      tmp_ptr = lisp_lookup_variable(lisp, LISP_CAR(lisp, tmp), context);
      if (tmp_ptr == NULL) {
        WRONG("Undefined variable.");
        return UNDEFINED;
      }
      return *tmp_ptr = lisp_evaluate(lisp, LISP_CAR(lisp, LISP_CDR(lisp, tmp)),
                                      context);
    } else if (EQ(tmp, lisp->keysyms.lambda)) {
      return lisp_make_closure(lisp, LISP_CDR(lisp, expression), context);

    } else if (OBJ_TYPE(tmp) == OBJ_PAIR_TAG) {
      if (!EQ(LISP_CAR(lisp, tmp), lisp->keysyms.lambda)) {
        WRONG("Invalid function form.");
        return UNDEFINED;
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

  return UNDEFINED;
}

Object lisp_eval(LispEnv *lisp, Object expression) {
  return lisp_evaluate(lisp, expression, NIL);
}

static Object lisp_list_recurse(LispEnv *lisp, va_list args) {
  Object arg = va_arg(args, Object);
  if (EQ(arg, NIL)) {
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

u8 *lisp_to_string(LispEnv *lisp, Object object) {
  /* We want this string to persist for a little bit after we return it. */
  static u8 buf[LISP_MAX_STRING_LENGTH];

  FILE *string_stream = fmemopen(buf, LISP_MAX_STRING_LENGTH - 1, "w");
  lisp_print(lisp, object, string_stream);
  fputc('\0', string_stream);
  fclose(string_stream);
  return buf;
}

Object lisp_new_ecs_component(LispEnv *lisp, Object type) {
  khiter_t iter = kh_get(var_syms, lisp->structs, type.bits);
  struct Storage storage;
  struct LispComponentStorage lisp_storage;

  if (iter != kh_end(lisp->structs)) {
    /* store a struct type */
    lisp_storage.type = STORE_STRUCT;
    Object metadata = kh_value(lisp->structs, iter);

    lisp_storage = (struct LispComponentStorage){

        .type = STORE_STRUCT,
        /* struct-id stored at index 1 in struct metadata vector */
        .struct_id = OBJ_UNBOX(*lisp_get_vector_item(lisp, metadata, 1)),
        /* struct size stored at index 0 in struct metadata vector */
        .size = lisp_storage.size =
            OBJ_UNBOX(*lisp_get_vector_item(lisp, metadata, 0))};

    /* TODO: Calculate Lisp struct alignment and include it here */
    storage = (struct Storage){
        .size = lisp_storage.size,
        .alignment =
            BIT_CAST(i32, OBJ_UNBOX(*lisp_get_vector_item(lisp, metadata, 4)))};

  } else {
    /* store an object type */
    enum ObjectTag tag = lisp_type_tag(lisp, type);

#define UNBOXED_CASE(TAG, TYPE)                                                \
  case TAG:                                                                    \
    lisp_storage = (struct LispComponentStorage){                              \
        .type = STORE_UNBOXED, .size = sizeof(TYPE), .object_type = TAG};      \
    storage =                                                                  \
        (struct Storage){.size = sizeof(TYPE), .alignment = alignof(TYPE)};    \
    break

    switch (tag) {
      UNBOXED_CASE(OBJ_INT_TAG, i32);
      UNBOXED_CASE(OBJ_FLOAT_TAG, f32);
      UNBOXED_CASE(OBJ_CHAR_TAG, u8);
    case OBJ_NIL_TAG:
      /* NIL type means no storage is required: can just use a normal Entity. */
      return ecs_new(lisp->world);
    default:
      lisp_storage = (struct LispComponentStorage){.type = STORE_OBJECT,
                                                   .object_type = tag};
      storage = (struct Storage){.size = sizeof(Object),
                                 .alignment = alignof(Object)};
    }
#undef UNBOXED_CASE
  }

  Object obj = ecs_new_component(lisp->world, storage);

  ecs_add(lisp->world, obj, lisp->comp.lisp_component_storage);
  *(struct LispComponentStorage *)ecs_get(
      lisp->world, obj, lisp->comp.lisp_component_storage) = lisp_storage;
  return obj;
}
