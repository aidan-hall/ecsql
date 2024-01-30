#ifndef OBJECT_H
#define OBJECT_H

#include "types.h"

#define OBJ_MASK (0xffffffffffffffe0)
#define OBJ_TAG_LENGTH (5)

#define LISP_INDEX_METADATA_LENGTH (16)
#define LISP_INDEX_OFFSET (LISP_INDEX_METADATA_LENGTH + OBJ_TAG_LENGTH)
#define LISP_INDEX_METADATA_MASK (0xffff << OBJ_TAG_LENGTH)
#define LISP_MAX_STRING_LENGTH ((2 << LISP_INDEX_METADATA_LENGTH) - 2)

/* 5-bit object type tag: allows up to 32 built-in types */
enum ObjectTag {
  /* Atomic objects (evaluate to themselves) */
  OBJ_NIL_TAG = 0,
  OBJ_STRING_TAG,
  OBJ_CHAR_TAG,
  OBJ_INT_TAG,
  OBJ_FLOAT_TAG,
  OBJ_FILE_PTR_TAG,
  /* Non-atomic objects */
  OBJ_UNDEFINED_TAG,
  OBJ_SYMBOL_TAG,
  OBJ_PAIR_TAG,
  OBJ_PRIMITIVE_TAG,
  OBJ_CLOSURE_TAG
};

/* Filter out just the type tag of an object */
#define OBJ_TAG_NAME(NAME) (OBJ_##NAME##_TAG)
#define OBJ_TYPE(OBJ) ((enum ObjectTag)(OBJ & ~OBJ_MASK))
#define OBJ_BOX_RAWTAG(VALUE, RAWTAG)                                          \
  ((Object)(((VALUE) << (OBJ_TAG_LENGTH)) | (RAWTAG)))
#define OBJ_BOX(VALUE, TAG) OBJ_BOX_RAWTAG(VALUE, OBJ_TAG_NAME(TAG))
#define OBJ_BOX_INDEX(INDEX, METADATA, TAG)                                    \
  (OBJ_BOX(((INDEX) << LISP_INDEX_METADATA_LENGTH) | (METADATA), TAG))
#define OBJ_UNBOX(BOX) (BOX >> OBJ_TAG_LENGTH)
#define OBJ_UNBOX_INDEX(BOX) (BOX >> LISP_INDEX_OFFSET)
#define OBJ_UNBOX_METADATA(BOX)                                                \
  ((BOX & LISP_INDEX_METADATA_MASK) >> OBJ_TAG_LENGTH)
#define OBJ_REINTERPRET_RAWTAG(OBJ, NEWTAG) ((OBJ & OBJ_MASK) | (NEWTAG))
#define OBJ_REINTERPRET(OBJ, NEWTAG)                                           \
  OBJ_REINTERPRET_RAWTAG(OBJ, OBJ_TAG_NAME(NEWTAG))

static inline float lisp_unbox_float(Object box) {
  if (OBJ_TYPE(box) == OBJ_FLOAT_TAG) {
    u32 val_bits = (u32)OBJ_UNBOX(box);
    return *(float *)&val_bits;
  } else if (OBJ_TYPE(box) == OBJ_INT_TAG) {
    return (float)(i32)OBJ_UNBOX(box);
  } else {
    return OBJ_UNDEFINED_TAG;
  }
}

#define EQ(X, Y) ((X) == (Y))

/* Get the Lisp cell at the given index, with no error handling. */
static inline Object *lisp_cell_at(struct LispEnv *lisp, size index) {
  return &((Object *)lisp->memory.space.begin)[index];
}

#define LISP_CAR_INDEX (0)
#define LISP_CDR_INDEX (1)
#define LISP_CAR_PLACE(LISP, PAIR)                                             \
  (lisp_cell_at(LISP, OBJ_UNBOX_INDEX(PAIR) + LISP_CAR_INDEX))
#define LISP_CDR_PLACE(LISP, PAIR)                                             \
  (lisp_cell_at(LISP, OBJ_UNBOX_INDEX(PAIR) + LISP_CDR_INDEX))
#define LISP_CAR(LISP, PAIR) (*LISP_CAR_PLACE(LISP, PAIR))
#define LISP_CDR(LISP, PAIR) (*LISP_CDR_PLACE(LISP, PAIR))

static inline bool lisp_true(Object value) {
  static_assert(!OBJ_NIL_TAG, "Lisp and C have the same truthy semantics.");
  return value;
}

static inline bool lisp_false(Object value) { return !lisp_true(value); }

static inline Object lisp_bool(LispEnv *lisp, bool value) {
  return value ? lisp->keysyms.t : OBJ_NIL_TAG;
}

#endif
