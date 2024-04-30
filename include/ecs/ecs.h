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
/* Entity IDs of live entities.
 * Only stores IDs so we can use it when generating a new Entity ID. */
KHASH_SET_INIT_INT(live);

struct World;

/* Fundamental, built-in, public ECS Components. */
typedef struct WorldComponents {
  Object storage;
  Object system;
  Object nwise_system;
  Object self_join_system;
  Object system_data;
  Object query;
} WorldComponents;

/* Information about the data storage for a Component. */
struct Storage {
  size size;
  size alignment;
};

/* Construct an Object for an Entity with the given ID and Generation. */
static inline Object ENT_BOX(EntityID id, u16 gen) {
  Object ent = {0};
  ent.id = id;
  ent.gen = gen;
  ent.tag = OBJ_ENTITY_TAG;
  return ent;
}

/* Obtain a numeric "signature" that uniquely identifies an Entity. */
static inline u64 ENT_SIG(Object ent) { return ent.bits; }

/* Compare Entities A and B using OP */
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

/* Construct a Relationship pair with the given relationship/type, and
 * target/Entity. */
static inline Object ecs_pair(Object relationship, Object entity) {
  Object obj = {0};
  obj.entity = entity.id;
  obj.relation = relationship.id.val;
  obj.tag = OBJ_RELATION_TAG;
  return obj;
}

/* Create a new ECS world. */
struct World *init_world();
/* Create a new Entity in the World. */
Object ecs_new(struct World *world);
/* Attempts to set a name for the given Entity.  Returns false if the operation
 * failed. */
[[nodiscard]] bool ecs_set_name(struct World *world, Object entity,
                                Object name);
/* Get the Entity with the given name, or NIL if not found. */
Object ecs_lookup_by_name(struct World *world, Object name);
void ecs_destroy(struct World *world, Object entity);
bool ecs_alive(struct World *world, Object entity);
/* Obtain the value of 'component' for 'entity'.
 * Returns NULL if 'entity' doesn't have 'component', or 'component' doesn't
 * have Storage. */
void *ecs_get(struct World *world, Object entity, Object component);
/* Test if the supplied Archetype contains the supplied Component. */
bool ecs_archetype_has(struct World *world, ArchetypeID archetype_id,
                       Object component);
/* Get the number of Components in the given Archetype */
size ecs_archetype_size(struct World *world, ArchetypeID archetype);
bool ecs_has(struct World *world, Object entity, Object component);
void ecs_add(struct World *world, Object entity, Object component);
void ecs_remove(struct World *world, Object entity, Object component);
/* Create a new Component Entity, with the supplied value of Storage. */
Object ecs_new_component(struct World *world, struct Storage storage);
/* Gets the location of the Generation for ID 'id' in 'world's Generation map.
 */
u16 *ecs_generation(struct World *world, EntityID id);
/* Get the WorldComponents of world. */
WorldComponents *ecs_world_components(struct World *world);

/* Get the live Entity, if any, with the supplied id. */
static inline Object ecs_object_with_id(struct World *world, EntityID id) {
  u16 gen = *ecs_generation(world, id);
  Object entity = ENT_BOX(id, gen);
  if (!ecs_alive(world, entity)) {
    fprintf(stderr, "BAD: Attempted to look up dead entity.\n");
    return NIL;
  }
  return entity;
}

typedef ObjectVector Type;
Type ecs_type(struct World *world, Object entity);

/* Generate a Storage Component value for the given TYPE. */
#define TYPE_STORAGE(TYPE)                                                     \
  ((struct Storage){.size = sizeof(TYPE), .alignment = alignof(TYPE)})
/* Create a new Component with values of type TYPE. */
#define ECS_NEW_COMPONENT(WORLD, TYPE)                                         \
  (ecs_new_component(WORLD, TYPE_STORAGE(TYPE)))

/* Indicates that something is not present in an array. */
#define NOT_PRESENT (-1)

/* Obtain the position of 'component' in Type 'a'.
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

#endif
