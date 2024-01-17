#include "lexer.h"
#include "arena.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>

/* Consume and return the 1-character token at the given index in 'stream'. */
static inline Token one_char_token(s8 stream[static 1], size index,
                                   TokenType type) {
  /* Move stream past the consumed character, and include it in the returned
   * Token. */
  stream->data += index + 1;
  stream->len -= index + 1;
  return (Token){{&stream->data[-1], 1}, type};
}

/* Returns a pointer to first non-whitespace, non-comment character in 'stream',
 * or nullptr if one was not found. */
u8 *skip_whitespace_and_comments(s8 stream[static 1]) {
  u8 *point = stream->data;
  const u8 *stream_end = stream->data + stream->len;

  while (point < stream_end) {
    /* Skip the comment. */
    if (*point == LEX_COMMENT_CHAR) {
      point = memchr(point, LEX_COMMENT_END, stream_end - point);

      if (point == nullptr) {
        /* fputs("end of stream in comment\n", stderr); */
        return nullptr;
      }

      /* Move past the newline character. */
      point += 1;
    } else if (isspace(*point)) {
      point++;
    } else {
      /* Done */
      return point;
    }
  }

  /* fputs("end of stream in whitespace\n", stderr); */
  return nullptr;
}

Token get_token(s8 stream[static 1]) {
  u8 *point = skip_whitespace_and_comments(stream);

  if (point == nullptr) {
    /* End of stream reached. */
    return (Token){{0}, TOK_END};
  }

  size index = point - stream->data;

  Token res;
  switch (*point) {
  case TOK_LPAR:
  case TOK_RPAR:
    return one_char_token(stream, index, *point);
  case '"':
    /* String */
    point++;
    index++;
    /* Move point forwards while keeping index at the start of the string data.
     * TODO: Properly handle escape characters in strings. */
    point = memchr(point, '"', stream->len - index);
    if (point == nullptr) {
      fputs("end of stream while reading string\n", stderr);
      return (Token){{0}, TOK_ERROR};
    }

    /* Update index so &(point+1)[index] is the start of the string */
    res = (Token){{&stream->data[index], point - &stream->data[index]},
                  TOK_STRING};
    stream->len = stream->data + stream->len - point - 1;
    stream->data = point + 1;
    return res;

  default:
    /* Single-character tokens */
    if (strchr(LEXEME_CHARS, *point)) {
      return one_char_token(stream, index, TOK_LEX_CHAR);
    }

    /* Symbol */
    while (!strchr(LEX_SYMBOL_TERMINATORS, *point) && !isspace(*point) &&
           point < stream->data + stream->len) {
      point++;
    }
    res = (Token){{&stream->data[index], point - &stream->data[index]},
                  TOK_SYMBOL};
    stream->len = stream->data + stream->len - point;
    stream->data = point;
    return res;
  }
}
