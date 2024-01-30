#ifndef LEXER_H
#define LEXER_H

#include "common.h"
#include "lisp.h"
#include <stdio.h>

#define LEX_COMMENT_CHAR ';'
#define LEX_COMMENT_END '\n'

/* Characters that should each be treated as an individual lexeme. */
/* #define LEXEME_CHARS u8"',`@" */

/* Non-space characters that terminate a symbol. */
#define LEX_SYMBOL_TERMINATORS "\"()"

typedef enum {
  TOK_ERROR = '!',
  /* Indicates that all tokens have been read from the stream. */
  TOK_END = '$',
  TOK_SYMBOL = 'L',
  TOK_STRING = '\"',
  TOK_LPAR = '(',
  TOK_RPAR = ')',
  TOK_LEX_CHAR = '\'',
  TOK_POINT = '.'
} TokenType;

typedef struct {
  union {
    /* The lexeme string. Persists until the next call to get_token. */
    s8 lexeme;
    /* Stores the lexeme if it is one of the special single-character tokens. */
    char lex_char;
  };
  TokenType t;
} Token;

/* Consumes 1 token from the stream. */
Token get_token(LispEnv *lisp, FILE *stream);

#endif
