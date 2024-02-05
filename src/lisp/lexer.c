#include <arena.h>
#include <lisp/lexer.h>
#include <lisp/lisp.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>

/* Make and return a 1-character token. */
static inline Token one_char_token(char value,
                                   TokenType type) {
  /* Move stream past the consumed character, and include it in the returned
   * Token. */
  return (Token){.lex_char = value, .t = type};
}

/* /\* Read the portion of stream [start,end) into a malloc'd string. *\/ */
/* static inline Token multi_character_token(FILE* stream, size start, size len, TokenType type) { */
/*   /\* Includes NULL terminating byte. *\/ */
/*   char *buffer = calloc(len + 1, sizeof(char)); */
/*   fseek(stream, start, SEEK_SET); */
/*   fread(buffer, sizeof(char), len, stream); */
/*   return (Token){.lexeme = {buffer, len}, .t = type}; */
/* } */

/* Moves read head to first non-whitespace, non-comment character in 'stream',
 * or to EOF if one is not found. */
static void skip_whitespace_and_comments(FILE *stream) {
  while (!feof(stream)) {
    /* Skip a comment. */
    char c = fgetc(stream);
    if (c == LEX_COMMENT_CHAR) {
      while (!feof(stream) && c != LEX_COMMENT_END) {
	c = fgetc(stream);
      }

      if (feof(stream)) {
        fputs("end of stream in comment\n", stderr);
        return;
      }

    } else if (!isspace(c)) {
      /* The character read wasn't whitespace or a comment: put it back in there. */
      ungetc(c, stream);
      /* Done */
      return;
    }
  }

  /* fputs("end of stream in whitespace\n", stderr); */
  return;
}

/* This is the longest length of string my Lisp can handle so it is enough. */
char buffer[LISP_MAX_STRING_LENGTH+1];
size buffer_index;

Token get_token(LispEnv *lisp, FILE *stream) {
  skip_whitespace_and_comments(stream);

  if (feof(stream)) {
    /* End of stream reached. */
    return (Token){.t = TOK_END};
  }

  char c = fgetc(stream);
  /* fputs("Up to char:", stderr); */
  /* fputc(c, stderr); */

  Token res;
  switch (c) {
  case TOK_LPAR:
  case TOK_RPAR:
    /* Parens */
    return one_char_token(c, c);
  case '"':
    /* String */
    buffer[0] = c = fgetc(stream);
    buffer_index = 1;
    while (!feof(stream) && c != '"')
      buffer[buffer_index++] = c = fgetc(stream);
    if (feof(stream)) {
      fputs("end of stream while reading string\n", stderr);
      return (Token){.t = TOK_ERROR};
    }
    /* The last character read was the closing '"'. */
    buffer[buffer_index - 1] = '\0';
    res = (Token){.lexeme = {(u8*) buffer, buffer_index - 1}, .t = TOK_STRING};
    
    return res;

  default:
    /* Reader macros: represented by individual ASCII characters. */
    if (0 <= c && c < 128) {
      if (lisp->reader_macros[(size) c] != NULL) {
      return one_char_token(c, TOK_LEX_CHAR);
      }
    }

    /* Symbol */
    buffer[0] = c;
    buffer_index = 1;
    while (!strchr(LEX_SYMBOL_TERMINATORS, c) && !isspace(c) &&
           !feof(stream) && lisp->reader_macros[(size) c] == NULL) {
      buffer[buffer_index++] = c = fgetc(stream);
    }
    /* The last character might be something we need. */
    ungetc(c, stream);
    buffer[buffer_index - 1] = '\0';
    
    res = (Token){.lexeme = {(u8*)buffer, buffer_index - 1}, .t = TOK_SYMBOL};
    if (strcmp(buffer, ".") == 0) {
      return (Token){.lex_char = '.', .t = TOK_POINT};
    }
    return res;
  }
}
