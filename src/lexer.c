#include "lexer.h"
#include "arena.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>

Token get_token(s8 stream[static 1]) {
  /* if (stream->len == 0) { */
  /*   return (Token){{0}, TOK_ERROR}; */
  /* } */

  /* Consume whitespace and skip comments. */
  size index = 0;
  /* Points at the character at stream->data[index].
   * This implicitly handles a NULL stream->data.
   */
  u8 *point = stream->data;
  while (index < stream->len) {
    /* Skip the comment, which continues until the end of the current line. */
    if (*point == COMMENT_CHAR) {
      point = memchr(point, '\n', stream->len - index);
      if (point == nullptr) {
        fputs("end of stream in comment\n", stderr);
        return (Token){{0}, TOK_ERROR};
      }

      /* Move past the newline character. */
      point += 1;
      index = point - stream->data;
    } else if (isspace(*point)) {
      point++;
      index++;
    } else {
      break;
    }
  }

  if (index >= stream->len) {
    /* End of stream reached. */
    return (Token){{0}, TOK_END};
  }

  Token res;
  switch (*point) {
    /* Single-character tokens: handled identically. */
  case TOK_SPLICE:
  case TOK_COMMA:
  case TOK_BACKQUOTE:
  case TOK_LPAR:
  case TOK_RPAR:
  case TOK_QUOTE:
    /* Move stream past the consumed character, and include it in the returned
     * Token. */
    stream->data = point + 1;
    stream->len -= index + 1;
    return (Token){{point, 1}, *point};
  case '"':
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
    while (!strchr("()'\",`@;", *point) && !isspace(*point) && point < stream->data + stream->len) {
      point++;
    }
    res = (Token){{&stream->data[index], point - &stream->data[index]},
                  TOK_SYMBOL};
    stream->len = stream->data + stream->len - point;
    stream->data = point;
    return res;
  }
}
