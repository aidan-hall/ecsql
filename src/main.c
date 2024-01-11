#include "lexer.h"
#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
  if (argc <= 1)
    return 1;

  s8 stream = (s8){(u8*) argv[1], strlen(argv[1])};
  for (Token tok = get_token(&stream); tok.t != TOK_ERROR && tok.t != TOK_END; tok = get_token(&stream)) {
    printf("Token, t=%c '", tok.t);
    fwrite(tok.lexeme.data, sizeof(u8), tok.lexeme.len, stdout);
    printf("'\n");
  }

  return 0;
}
