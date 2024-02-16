#include <common.h>
#include <lisp/reader.h>
#include <lisp/lexer.h>
#include <lisp/lisp.h>

#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static Object lisp_read_list_tail(LispEnv *lisp, FILE *stream);

static Object read_with_token(LispEnv *lisp, Token tok, FILE *stream) {
  switch (tok.t) {
  case TOK_STRING:
    return lisp_store_string(lisp, tok.lexeme);
  case TOK_SYMBOL: {
    /* Symbol tokens get interpreted as an integer, or a float, if possible. */
    u8 *endptr;
    errno = 0;
    i64 int_value = strtoll((char *)tok.lexeme.data, (char **)&endptr, 0);
    /* Only store as a 32-bit integer if its magnitude is small enough. */
    if (*endptr == '\0' && errno == 0 && int_value <= INT32_MAX &&
        int_value >= INT32_MIN) {
      return OBJ_BOX(int_value, INT);
    }
    float float_value = strtof((char *)tok.lexeme.data, (char **)&endptr);
    if (*endptr == '\0') {
      return OBJ_IMM(float_value);
    }
    /* Symbol could not be parsed as an integer or float, so it really is a
     * symbol. */
    return lisp_intern(lisp, tok.lexeme);
  }
  case TOK_ERROR:
    WRONG("Lexical error.");
    break;
  case TOK_END:
    return OBJ_BOX(EOF, CHAR);
  default:
    WRONG("Unexpected token.");
    break;
  case TOK_RPAR:
    WRONG("Unexpected closing parenthesis.");
    break;
  case TOK_LPAR:
    /* tmp = lisp_read(lisp, stream); */
    /* return lisp_cons(lisp, tmp, lisp_read_list_tail(lisp, stream)); */
    return lisp_read_list_tail(lisp, stream);
    break;
  case TOK_LEX_CHAR: {
    ReaderMacro macro = lisp->reader_macros[(usize)tok.lex_char];
    if (macro == NULL) {
      WRONG("Nonexistent reader macro", OBJ_BOX(tok.lex_char, CHAR));
      return UNDEFINED;
    } else {
      return macro(lisp, stream);
    }
  }
  }

  return UNDEFINED;
}

static Object lisp_read_list_tail(LispEnv *lisp, FILE *stream) {
  Token tok = get_token(lisp, stream);
  Object tmp;
  switch (tok.t) {
  case TOK_RPAR:
    return NIL;
  case TOK_POINT:
    tmp = read_with_token(lisp, get_token(lisp, stream), stream);
    tok = get_token(lisp, stream);
    if (tok.t != TOK_RPAR) {
      WRONG("More than one object follows . in list.");
    }
    return tmp;
  case TOK_END:
    WRONG("Unexpected end of stream in list tail.");
    return UNDEFINED;
  default:
    tmp = read_with_token(lisp, tok, stream);
    return lisp_cons(lisp, tmp, lisp_read_list_tail(lisp, stream));
    break;
  }
}

Object lisp_read(LispEnv *lisp, FILE *stream) {
  Token tok = get_token(lisp, stream);

  return read_with_token(lisp, tok, stream);
}

Object lisp_read_from_string(LispEnv *lisp, s8 str) {
  /* Discarding the const qualifier here is ONLY fine because we know we aren't
   * modifying the string. */
  FILE *stream = fmemopen((u8 *)str.data, str.len, "r");
  Object result = lisp_read(lisp, stream);
  fclose(stream);
  return result;
}
