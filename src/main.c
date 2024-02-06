#include <common.h>
#include <lisp/lexer.h>
#include <lisp/lisp.h>
#include <lisp/print.h>
#include <lisp/reader.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ecs/ecs.h>

int main(int argc, char *argv[]) {
  LispEnv lisp = new_lisp_environment();
  if (setjmp(lisp.error_loc) != 0) {
    fprintf(stderr, "Error in a file loaded at startup: no good!\n");
    exit(1);
  }
  Object l = lisp_list(&lisp, lisp.keysyms.quote, lisp.keysyms.t, OBJ_NIL_TAG);
  lisp_print(&lisp, l, stdout);
  fputc('\n', stdout);

  if (setjmp(lisp.error_loc) != 0) {
    fprintf(stderr, "Resuming from top level...\n");
  }


  struct World *world = init_world();
  /* lisp_eval(&lisp, OBJS(&lisp, "(repl)")); */
  return 0;
}
