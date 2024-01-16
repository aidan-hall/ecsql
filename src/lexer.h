#ifndef LEXER_H
#define LEXER_H

#include "common.h"

#define COMMENT_CHAR ';'

typedef enum : u8 {
  TOK_ERROR = '!',
  /* Indicates that all tokens have been read from the stream. */
  TOK_END = '$',
  TOK_SYMBOL = 'L',
  TOK_BACKQUOTE = '`',
  TOK_COMMA = ',',
  TOK_LPAR = '(',
  TOK_RPAR = ')',
  TOK_QUOTE = '\'',
  TOK_STRING = '\"',
  TOK_SPLICE = '@',
} TokenType;

typedef struct {
  s8 lexeme;
  TokenType t;
} Token;

/* Consumes 1 token from the stream. */
Token get_token(s8 stream[static 1]);

#endif
