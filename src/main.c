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
  lisp_print(&lisp, l, stdout);
  fputc('\n', stdout);
  Object first;
  Object eof = OBJ_BOX(EOF, CHAR);
  do {
    if (setjmp(lisp.error_loc) != 0) {
      fprintf(stderr, "Resuming from top level...\n");
    }
    fputs(LISP_PROMPT, stdout);
    first = lisp_read(&lisp, stdin);
    if (EQ(first, eof)) {
      puts("End of file reached. Goodbye.");
      return 0;
    }
    first = lisp_eval(&lisp, first);
    lisp_print(&lisp, first, stdout);
    fputc('\n', stdout);
  } while (first != OBJ_UNDEFINED_TAG && !feof(stdin));
  return 0;
}
