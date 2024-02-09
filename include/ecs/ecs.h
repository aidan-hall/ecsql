#ifndef ECS_H
#define ECS_H

#include <assert.h>
#include <common.h>
#include <klib/khash.h>
#include <klib/kvec.h>
#include <lisp/object.h>
#include <stdint.h>

/* Entity ID → newest generation. */
KHASH_MAP_INIT_INT(gen, u16);
/* Entity IDs of live entities. */
KHASH_SET_INIT_INT(live);

struct World;
struct Archetype;
typedef kvec_t(Object) Type;

#define ENT_ID_OFFSET (32 - OBJ_TAG_LENGTH)
#define ENT_GEN_OFFSET (16 - OBJ_TAG_LENGTH)

void initialise_core_components(struct World *world);

/* Information about the data storage for a Component. */
struct Storage {
  size size;
  size alignment;
};

struct StructMember {
  size offset;
};

struct Vec3 {
  float x;
  float y;
  float z;
};

static inline Object ENT_BOX(u32 id, u16 gen) {
  Object ent = {0};
  ent.id = id;
  ent.gen = gen;
  ent.tag = OBJ_ENTITY_TAG;
  return ent;
}
/* Get the tag bits of the  */
static inline u8 ENT_TAG(Object entity) { return entity.tag; }
/* Mask out the Object type and tag bits.
 * What remains is the "signature":
 * It uniquely identifies a particular *Component* type. */
static inline u64 ENT_SIG(Object entity) { return entity.sig; }

struct World *init_world();
Object ecs_new(struct World *world);
bool entity_live(struct World *world, Object entity);
void *ecs_get(struct World *world, Object entity, Object component);
void ecs_add(struct World *world, Object entity, Object component);
void ecs_remove(struct World *world, Object entity, Object component);
Object ecs_new_component(struct World *world, struct Storage storage);

#define ECS_NEW_COMPONENT(WORLD, TYPE)                                         \
  (ecs_new_component(WORLD, (struct Storage){.size = sizeof(TYPE),             \
                                             .alignment = alignof(TYPE)}))

/* Obtain the position of 'component' in 'a'.
 * Returns -1 if absent. */
static inline size type_pos(Type a, Object component) {
  /* Type vectors are sorted, so binary search. */
  size lo = 0, hi = kv_size(a);
  while (lo < hi) {
    size med = (lo + hi) / 2;
    Object val = kv_A(a, med);
    if (EQ(val, component)) {
      return med;
    } else if (val.bits < component.bits) {
      lo = med + 1;
    } else {
      hi = med;
    }
  }

  return -1;
}

struct Partition {
  size min;
  size max;
  size next;
};

#endif
