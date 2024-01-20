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

LispEnv new_lisp_environment() {
  LispEnv lisp = {0};
  /* A Gigabyte of RAM should do the trick. */
  lisp.memory = new_lisp_memory(1024 * 1024 * 1024);
  lisp.jit = gcc_jit_context_acquire();

  static_assert(sizeof(void *) == sizeof(Object), "Can't use clibs/hash.");
  lisp.symbols = kh_init(sym_name);
  lisp.reader_macros['\''] = lisp_quote;
  lisp.reader_macros['`'] = lisp_quasiquote;
  lisp.reader_macros[','] = lisp_unquote;
  lisp.reader_macros['?'] = lisp_question_mark;

#define REGISTER_KEYSYM(K) lisp.keysyms.K = lisp_intern(&lisp, s8(#K));
  LISP_KEYSYMS(REGISTER_KEYSYM);
#undef REGISTER_KEYSYM
  return lisp;
}

Object lisp_store_string(LispEnv *lisp, s8 string) {
  if (string.len > LISP_MAX_STRING_LENGTH) {
    wrong("Symbol name too long.");
  }

  /* We will include a NULL terminator byte for security. */
  size data_index = lisp_allocate_bytes(lisp, string.len + 1);
  u8 *data = (u8 *)lisp_cell_at(lisp, data_index);
  strncpy(data, string.data, string.len);

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
    symbol = lisp_store_string(lisp, string);
    string = lisp_string_to_s8(lisp, symbol);
    /* Symbols are encoded as a string reference, with the SYMBOL tag. */
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

void lisp_print(LispEnv *lisp, Object object, FILE *stream, int depth) {
  for (int i = 0; i < depth; i++) {
    fputc(' ', stream);
  }
  switch (OBJ_TYPE(object)) {
  case OBJ_FLOAT_TAG: {
    u32 val_bits = (u32)OBJ_UNBOX(object);
    float val = *(float *)&val_bits;
    fprintf(stream, "float: %f\n", val);
  } break;
  case OBJ_INT_TAG: {
    i32 val_bits = (i32)OBJ_UNBOX(object);
    fprintf(stream, "i32: %d (%x), %x\n", val_bits, OBJ_UNBOX(object));
  } break;
  case OBJ_STRING_TAG:
    fprintf(stream, "string: (%x) '%s'\n", object,
            (char *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object)));
    break;
  case OBJ_SYMBOL_TAG:
    fprintf(stream, "symbol: (%x) '%s'\n", object,
            (char *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object)));
    break;
  case OBJ_PAIR_TAG:
    fprintf(stream, "cons: %x\n", object);
    lisp_print(lisp, LISP_CAR(lisp, object), stream, depth + 1);
    lisp_print(lisp, LISP_CDR(lisp, object), stream, depth + 1);
    break;
  default:
    fprintf(stream, "other type: %x.\n", OBJ_TYPE(object));
    break;
  }
}
