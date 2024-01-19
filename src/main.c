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
  Object l = lisp_list(&lisp, lisp.keysyms.quote, lisp.keysyms.t, OBJ_NIL_TAG);
  lisp_print(&lisp, l, stdout, 0);
  Object first;
  do {
    first = lisp_read(&lisp, stdin);
    lisp_print(&lisp, first, stdout, 0);
  } while (first != OBJ_UNDEFINED_TAG);
  return 0;
}
