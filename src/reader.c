#include "reader.h"
#include "common.h"
#include "lexer.h"
#include "lisp.h"

#include <string.h>
/* #include <readline/readline.h> */
/* #include <readline/history.h> */
#include "common.h"

/* Stores the line most recently read by readline. */
/* char *line; */
/* A slice into line. */
/* s8 buffer; */

static Object lisp_read_list_tail(LispEnv *lisp, FILE *stream);

static Object read_with_token(LispEnv *lisp, Token tok, FILE *stream) {
  Object tmp;
  switch (tok.t) {
  case TOK_STRING:
    return lisp_store_string(lisp, tok.lexeme);
  case TOK_SYMBOL: {
    /* Symbol tokens get interpreted as an integer, or a float, if possible. */
    u8 *endptr;
    i64 int_value = strtoll((char *)tok.lexeme.data, (char **)&endptr, 0);
    if (*endptr == '\0') {
      return OBJ_BOX(int_value, INT);
    }
    float float_value = strtof((char *)tok.lexeme.data, (char **)&endptr);
    if (*endptr == '\0') {
      return OBJ_BOX(*(u64 *)&float_value, FLOAT);
    }
    /* Symbol could not be parsed as an integer or float, so it really is a symbol. */
    return lisp_intern(lisp, tok.lexeme);
  }
  case TOK_ERROR:
    wrong("Lexical error.");
    break;
  case TOK_END:
    return OBJ_UNDEFINED_TAG;
  default:
    wrong("Unexpected token.");
  case TOK_RPAR:
    wrong("Unexpected closing parenthesis.");
    break;
  case TOK_LPAR:
    /* tmp = lisp_read(lisp, stream); */
    /* return lisp_cons(lisp, tmp, lisp_read_list_tail(lisp, stream)); */
    return lisp_read_list_tail(lisp, stream);
    break;
  }

  return OBJ_UNDEFINED_TAG;
  /* default: */
  /*   wrong("Supplied token was not handled."); */
  /*   return OBJ_UNDEFINED_TAG; */
  /* } */
}

static Object lisp_read_list_tail(LispEnv *lisp, FILE *stream) {
  Token tok = get_token(stream);
  Object tmp;
  switch (tok.t) {
  case TOK_RPAR:
    return OBJ_NIL_TAG;
  case TOK_POINT:
    tmp = read_with_token(lisp, get_token(stream), stream);
    tok = get_token(stream);
    if (tok.t != TOK_RPAR) {
      wrong("More than one object follows . in list.");
    }
    return tmp;
  case TOK_END:
    wrong("Unexpected end of stream in list tail.");
    return OBJ_UNDEFINED_TAG;
  default:
    tmp = read_with_token(lisp, tok, stream);
    return lisp_cons(lisp, tmp, lisp_read_list_tail(lisp, stream));
    break;
  }
}

Object lisp_read(LispEnv *lisp, FILE *stream) {
  /* if (line == nullptr) { */
  /*   line = readline(""); */
  /*   if (line == nullptr) { */
  /*     /\* EOF *\/ */
  /*     fputs("Reached EOF; exiting...", stderr); */
  /*     exit(0); */
  /*   } */
  /*   buffer = (s8){line, strlen(line)}; */
  /* } */

  /* /\* Readline reached EOF. *\/ */
  /* if (line == nullptr) */
  /*   return OBJ_UNDEFINED_TAG; */

  Token tok = get_token(stream);

  return read_with_token(lisp, tok, stream);
  
}
