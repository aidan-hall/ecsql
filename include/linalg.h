#ifndef LINALG_H
#define LINALG_H

#include <lisp/types.h>
#include <math.h>
#define VFOREACH()
typedef struct Vec2 {
  float x;
  float y;
} Vec2;

typedef struct Vec4i {
  i32 x;
  i32 y;
  i32 z;
  i32 w;
} Vec4i;

static inline float v2len2(Vec2 v) { return v.x * v.x + v.y * v.y; }

static inline float v2dot(Vec2 a, Vec2 b) { return a.x * b.x + a.y * b.y; }

static inline Vec2 v2add(Vec2 a, Vec2 b) {
  return (Vec2){a.x + b.x, a.y + b.y};
}

static inline Vec2 v2sub(Vec2 a, Vec2 b) {
  return (Vec2){a.x - b.x, a.y - b.y};
}

static inline Vec2 v2mul(Vec2 a, Vec2 b) {
  return (Vec2){a.x * b.x, a.y * b.y};
}

static inline Vec2 v2smul(Vec2 a, float m) {
  return (Vec2){a.x * m, a.y * m};
}

static inline Vec2 v2div(Vec2 a, Vec2 b) {
  return (Vec2){a.x / b.x, a.y / b.y};
}

static inline Vec2 v2norm(Vec2 v) {
  float len = sqrtf(v2len2(v));
  v.x /= len;
  v.y /= len;
  return v;
}

/* Assumes 'normal' is a unit vector. */
static inline Vec2 v2reflect(Vec2 incident, Vec2 normal) {
  float dot = v2dot(incident, normal);
  return v2sub(incident, v2smul(normal, 2 * dot));
}

#endif
