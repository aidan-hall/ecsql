#ifndef COMMON_H
#define COMMON_H

#include <stddef.h>
#include <stdint.h>
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
  u8 *data;
  size len;
} s8;
#define s8(s)                                                                  \
  (s8) { (u8 *)s, lengthof(s) }

#endif
