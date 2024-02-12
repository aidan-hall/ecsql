#ifndef OBJECT_H
#define OBJECT_H

#include "common.h"
#include <klib/kvec.h>
#include <stdlib.h>

#define OBJ_MASK (0xffffffffffffffe0)
#define OBJ_TAG_LENGTH (5)
#define OBJ_FLAGS_LENGTH (4)

#define LISP_INDEX_METADATA_LENGTH (16)
#define LISP_INDEX_OFFSET (LISP_INDEX_METADATA_LENGTH + OBJ_TAG_LENGTH)
#define LISP_INDEX_METADATA_MASK (0xffff << OBJ_TAG_LENGTH)
#define LISP_MAX_STRING_LENGTH ((2 << LISP_INDEX_METADATA_LENGTH) - 2)

#define DEF_TYPEID(NAME, REP)                                                  \
  typedef struct NAME##ID {                                                    \
    REP val;                                                                   \
  } NAME##ID
DEF_TYPEID(Archetype, u32);
DEF_TYPEID(Entity, u32);

typedef union Object {
  u64 bits;
  struct {
    u8 tag : OBJ_TAG_LENGTH;
    u64 val : 59;
  };
  struct {
    u8 : OBJ_TAG_LENGTH; /* enum ObjectTag */
    u32 metadata : 16;
    /* Must be signed to support indirect addressing */
    i64 index : 43;
  };
  struct {
    u8 : OBJ_TAG_LENGTH;
    u8 flags : OBJ_FLAGS_LENGTH;
    u16 gen;
    EntityID entity;
  };
  struct {
    u16 : OBJ_TAG_LENGTH + OBJ_FLAGS_LENGTH;
    u32 relation : 23; /* EntityID */
    EntityID id;
  };
} Object;

/* Object must fit within 64 bits. */
static_assert(sizeof(Object) == sizeof(u64));

typedef kvec_t(Object) ObjectVector;

/* 5-bit object type tag: allows up to 32 built-in types */
enum ObjectTag {
  /* Atomic objects (evaluate to themselves) */
  OBJ_NIL_TAG = 0,
  OBJ_STRING_TAG,
  OBJ_CHAR_TAG,
  OBJ_INT_TAG,
  OBJ_FLOAT_TAG,
  OBJ_FILE_PTR_TAG,
  OBJ_VECTOR_TAG,
  OBJ_STRUCT_TAG,
  /* Non-atomic objects */
  OBJ_UNDEFINED_TAG,
  OBJ_SYMBOL_TAG,
  OBJ_PAIR_TAG,
  OBJ_PRIMITIVE_TAG,
  OBJ_CLOSURE_TAG,
  OBJ_ENTITY_TAG,
  OBJ_RELATION_TAG
};

/* Filter out just the type tag of an object */
#define OBJ_TAG_NAME(NAME) (OBJ_##NAME##_TAG)

static inline enum ObjectTag OBJ_TYPE(Object obj) { return obj.tag; }
static inline Object OBJ_BOX_RAWTAG(u64 value, enum ObjectTag tag) {
  Object obj = {0};
  obj.tag = tag;
  obj.val = value;
  return obj;
}
#define OBJ_BOX(VALUE, TAG) OBJ_BOX_RAWTAG((VALUE), OBJ_TAG_NAME(TAG))
static inline Object OBJ_BOX_INDEX_RAWTAG(i64 index, u16 metadata,
                                          enum ObjectTag tag) {
  Object obj = {0};
  obj.metadata = metadata;
  obj.index = index;
  obj.tag = tag;
  return obj;
}
#define OBJ_BOX_INDEX(INDEX, METADATA, TAG)                                    \
  (OBJ_BOX_INDEX_RAWTAG(INDEX, METADATA, OBJ_TAG_NAME(TAG)))
/* Automatically box an immediate value based on the type. */
#define OBJ_IMM(VALUE)                                                         \
  OBJ_BOX_RAWTAG(BIT_CAST(u64, VALUE), _Generic((VALUE),                       \
                                       float: OBJ_FLOAT_TAG,                   \
                                       i32: OBJ_INT_TAG,                       \
                                       u8: OBJ_CHAR_TAG))
static inline u64 OBJ_UNBOX(Object obj) { return obj.val; }
static inline u64 OBJ_UNBOX_INDEX(Object obj) { return obj.index; }
static inline u16 OBJ_UNBOX_METADATA(Object obj) { return obj.metadata; }

static inline Object OBJ_REINTERPRET_RAWTAG(Object obj, enum ObjectTag newtag) {
  obj.tag = newtag;
  return obj;
}

#define OBJ_REINTERPRET(OBJ, NEWTAG)                                           \
  OBJ_REINTERPRET_RAWTAG(OBJ, OBJ_TAG_NAME(NEWTAG))

#define NIL (OBJ_BOX(0, NIL))
#define UNDEFINED (OBJ_BOX(0, UNDEFINED))

static inline float lisp_unbox_float(Object box) {
  if (OBJ_TYPE(box) == OBJ_FLOAT_TAG) {
    return BIT_CAST(float, (u32)OBJ_UNBOX(box));
  } else if (OBJ_TYPE(box) == OBJ_INT_TAG) {
    return (float)(i32)OBJ_UNBOX(box);
  } else {
    fprintf(stderr, "Attempted to unbox non-float as float\n");
    exit(1);
  }
}

static inline bool EQ(Object x, Object y) { return x.bits == y.bits; }

#define LISP_CAR_INDEX (0)
#define LISP_CDR_INDEX (1)
#define LISP_CAR_PLACE(LISP, PAIR)                                             \
  (lisp_cell_at(LISP, OBJ_UNBOX_INDEX(PAIR) + LISP_CAR_INDEX))
#define LISP_CDR_PLACE(LISP, PAIR)                                             \
  (lisp_cell_at(LISP, OBJ_UNBOX_INDEX(PAIR) + LISP_CDR_INDEX))
#define LISP_CAR(LISP, PAIR) (*LISP_CAR_PLACE(LISP, PAIR))
#define LISP_CDR(LISP, PAIR) (*LISP_CDR_PLACE(LISP, PAIR))

#endif
