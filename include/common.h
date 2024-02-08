#ifndef COMMON_H
#define COMMON_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <uchar.h>

typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
typedef int32_t i32;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int64_t i64;
typedef float f32;
typedef double f64;
typedef ptrdiff_t size;
typedef size_t usize;

#define countof(a) (size)(sizeof(a) / sizeof(*(a)))
#define lengthof(s) (countof(s) - 1)

typedef struct {
  const u8 *data;
  size len;
} s8;
#define s8(s)                                                                  \
  (s8) { (u8 *)s, lengthof(s) }

static inline void print_s8(FILE *stream, s8 s) {
  for (size i = 0; i < s.len; ++i) {
    fputc(s.data[i], stream);
  }
}

#define IGNORE(X) ((void)(X))
#define BIT_CAST(TO, VALUE)                                                    \
  (((union {                                                                   \
     typeof(VALUE) from;                                                       \
     TO to;                                                                    \
   }){.from = (VALUE)})                                                        \
       .to)
#endif
