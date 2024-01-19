#include "lexer.h"
#include "lisp.h"
#include "reader.h"
#include <libgccjit.h>
#include <math.h>
#include <readline/history.h>
#include <readline/readline.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Object lisp_car(LispEnv *lisp, Object pair) {
  return *lisp_cell_at(lisp, OBJ_UNBOX_INDEX(pair) + LISP_CAR_INDEX);
}
static Object lisp_cdr(LispEnv *lisp, Object pair) {
  return *lisp_cell_at(lisp, OBJ_UNBOX_INDEX(pair) + LISP_CDR_INDEX);
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
  case OBJ_INT_TAG:
    fprintf(stream, "int: %ld\n", OBJ_UNBOX(object));
    break;
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

int main(int argc, char *argv[]) {
  /* if (argc <= 1) */
  /*   return 1; */

  /* s8 stream = (s8){(u8 *)argv[1], strlen(argv[1])}; */

  /* for (Token tok = get_token(stdin); tok.t != TOK_ERROR && tok.t != TOK_END;
   */
  /*      tok = get_token(stdin)) { */
  /*   printf("Token, t=%c, %d, '", tok.t, tok.lex_char); */
  /*   fwrite(tok.lexeme.data, sizeof(u8), tok.lexeme.len, stdout); */
  /*   printf("'\n"); */
  /* } */
  LispEnv lisp = new_lisp_environment();
  Object first;
  do {
    first = lisp_read(&lisp, stdin);
    lisp_print(&lisp, first, stdout, 0);
  } while (first != OBJ_UNDEFINED_TAG);
  return 0;
}
