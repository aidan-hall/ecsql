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

static inline Object ENT_BOX(EntityID id, u16 gen) {
  Object ent = {0};
  ent.id = id;
  ent.gen = gen;
  ent.tag = OBJ_ENTITY_TAG;
  return ent;
}

static inline u64 ENT_SIG(Object ent) { return ent.bits; }

#define COMP_CMP(A, B, OP) (ENT_SIG(A) OP ENT_SIG(B))
#define COMP_LT(A, B) COMP_CMP((A), (B), <)
#define COMP_EQUIV(A, B) COMP_CMP((A), (B), ==)

/**
 * Ranges of allowed IDs for Entities and Components.
 * For now, don't allow Entity IDs to exceed 2^23 - 1,
 * so the 23 bits of the relation type in pairs is enough for any Entity.
 *
 * TODO: Implement a partitioning scheme to allow optimisations for small IDs,
 * and to support bigger ones.  Probably unnecessary for this project though!
 * @see new_entity_partitioned
 */
#define MIN_ENTITY (0)
#define MAX_ENTITY ((1 << 23))

static inline Object ecs_pair(Object relationship, Object entity) {
  /* TODO: Distinguish from normal Entities? */
  Object obj = {0};
  obj.entity = entity.id;
  obj.relation = relationship.id.val;
  obj.tag = OBJ_RELATION_TAG;
  return obj;
}

struct World *init_world(Object storage_name);
Object ecs_new(struct World *world);
/* Attempts to set a name for the given Entity.  Returns false if the operation
 * failed. */
[[nodiscard]] bool ecs_set_name(struct World *world, Object entity,
                                Object name);
Object ecs_lookup_by_name(struct World *world, Object name);
void ecs_destroy(struct World *world, Object entity);
bool ecs_alive(struct World *world, Object entity);
void *ecs_get(struct World *world, Object entity, Object component);
bool ecs_archetype_has(struct World *world, ArchetypeID archetype_id, Object component);
bool ecs_has(struct World *world, Object entity, Object component);
void ecs_add(struct World *world, Object entity, Object component);
void ecs_remove(struct World *world, Object entity, Object component);
Object ecs_new_component(struct World *world, struct Storage storage);
u16 *ecs_generation(struct World *world, EntityID id);

static inline Object ecs_object_with_id(struct World *world, EntityID id) {
  u16 gen = *ecs_generation(world, id);
  Object entity = ENT_BOX(id, gen);
  if (!ecs_alive(world, entity)) {
    fprintf(stderr, "BAD: Attempted to look up dead entity.\n");
    return NIL;
  }
  return entity;
}

/* TODO: Remove from public API */
typedef ObjectVector Type;
Type ecs_type(struct World *world, Object entity);

#define TYPE_STORAGE(TYPE)                                                     \
  ((struct Storage){.size = sizeof(TYPE), .alignment = alignof(TYPE)})
#define ECS_NEW_COMPONENT(WORLD, TYPE)                                         \
  (ecs_new_component(WORLD, TYPE_STORAGE(TYPE)))

#define NOT_PRESENT (-1)

/* Obtain the position of 'component' in 'a'.
 * Returns NOT_PRESENT (-1) if absent. */
static inline size type_pos(Type a, Object component) {
  /* Type vectors are sorted, so binary search. */
  size lo = 0, hi = kv_size(a);
  while (lo < hi) {
    size med = (lo + hi) / 2;
    Object val = kv_A(a, med);
    if (COMP_EQUIV(val, component)) {
      return med;
    } else if (COMP_LT(val, component)) {
      lo = med + 1;
    } else {
      hi = med;
    }
  }

  return NOT_PRESENT;
}

struct Partition {
  size min;
  size max;
  size next;
};

#endif
