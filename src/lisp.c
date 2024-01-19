#include "lisp.h"
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

LispEnv new_lisp_environment() {
  LispEnv lisp = {0};
  /* A Gigabyte of RAM should do the trick. */
  lisp.memory = new_lisp_memory(1024 * 1024 * 1024);
  lisp.jit = gcc_jit_context_acquire();

  static_assert(sizeof(void *) == sizeof(Object), "Can't use clibs/hash.");
  lisp.symbols = kh_init(sym_name);
  /* lisp.functions = hash_new(); */
  /* lisp.globals = hash_new(); */
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
    interned_key = kh_put(sym_name, lisp->symbols, (char *)string.data, &absent);
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
