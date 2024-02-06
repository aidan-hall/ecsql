#include "klib/khash.h"
#include "object.h"
#include <ecs.h>
#include <klib/kvec.h>

/**
 * Ranges of allowed IDs for Entities and Components.
 * @see new_entity_partitioned
 */
#define MIN_COMPONENT (0)
#define MAX_COMPONENT (UINT16_MAX + 1)

#define MIN_ENTITY (MAX_COMPONENT)
#define MAX_ENTITY (UINT32_MAX)

/* Storage for a single Component type in an Archetype. */
typedef struct Column {
  void *elements;
  size element_size;
  size count;
} Column;

KHASH_MAP_INIT_INT(archetype_edge, struct Archetype *);

typedef struct Archetype {
  u32 id;
  kvec_t(Object) type;
  kvec_t(Column) components;

  /* Component ID → Archetype with that Component removed from/added to this one. */
  khash_t(archetype_edge) neighbours;
} Archetype;

typedef struct Record {
  /* What archetype an Entity is in. */
  struct Archetype *archetype;
  /* Index into the Archetype's component arrays where the Entity's data is
   * stored. */
  size row;
} Record;

/* At what column in an Archetype is data for a give Component type stored. */
typedef struct ArchetypeRecord {
  size column;
} ArchetypeRecord;

KHASH_MAP_INIT_INT(component_archetype_column, ArchetypeRecord);
typedef khash_t(component_archetype_column) ArchetypeMap;

KHASH_MAP_INIT_INT(entity_data, Record);
KHASH_MAP_INIT_INT(component_archetypes, ArchetypeMap);

typedef struct World {
  khash_t(gen) * generations;
  khash_t(live) * live;
  u32 next_entity;
  u32 next_component;
  u32 next_archetype;

  khash_t(entity_data) * entity_index;
  khash_t(component_archetypes) * component_index;
  kvec_t(Archetype) archetypes;
} World;

/* Returns the generation for the given id stored in the generations hashmap.
 * The pointer is valid until the next update of the generations hashmap. */
u16 *entity_generation(World *world, u32 id) {
  khint_t iter = kh_get(gen, world->generations, id);

  int absent;

  if (iter == kh_end(world->generations)) {
    /* This ID has never been used before: "start" at generation 0. */
    iter = kh_put(gen, world->generations, id, &absent);
    if (absent < 0) {
      fprintf(stderr, "Failed to add a new entity ID to the generation set.\n");
      exit(1);
    }
    kh_value(world->generations, iter) = 0;
  }

  return &kh_value(world->generations, iter);
}

bool entity_alive(World *world, Object entity) {
  return *entity_generation(world, ENT_ID(entity)) == ENT_GEN(entity);
}

static Object new_entity_partitioned(World *world, u32 *start, u32 low,
                                     u32 high) {
  /* Find a free ID within the range [low,high), starting from *start. */
  u32 id = *start;
  const u32 initial = *start;
  if (id >= high || id < low) {
    id = low;
  }

  while (kh_get(live, world->live, id) != kh_end(world->live)) {
    id++;
    if (id >= high || id < low) {
      id = low;
    }
    if (id == initial) {
      fprintf(stderr, "Failed to allocate an ID in the range [%u, %u)\n", low,
              high);
      exit(1);
    }
  }
  *start = id;

  int absent;
  kh_put(live, world->live, id, &absent);
  if (absent < 0) {
    fprintf(stderr, "Failed to add a new entity to the live set.\n");
    exit(1);
  }

  return ENT_BOX(id, *entity_generation(world, id));
}

Object new_component(World *world) {
  return new_entity_partitioned(world, &world->next_entity, MIN_COMPONENT,
                                MAX_COMPONENT);
}

Object new_entity(World *world) {
  return new_entity_partitioned(world, &world->next_entity, MIN_ENTITY,
                                MAX_ENTITY);
}

void destroy_entity(World *world, Object entity) {
  khint_t live_iter = kh_get(live, world->live, ENT_ID(entity));
  if (live_iter == kh_end(world->live)) {
    fprintf(stderr, "Attempted to destroy an entity that was not alive.\n");
    exit(1);
  }
  kh_del(live, world->live, live_iter);
  /* Generation is updated "lazily" when the entity is destroyed. */
  u16 *gen = entity_generation(world, entity);
  *gen += 1;
}

struct World *init_world() {
  World *world = malloc(sizeof(World));
  *world = (World){0};
  /* Rudimentary ID space partitioning: [0,65536] for Components/Relations,
   * [65537,] for the rest. This makes it easier to ensure a Component is never
   * created with an id that can't be represented in 16 bits. */
  world->next_component = MIN_COMPONENT;
  world->next_entity = MIN_ENTITY;
  world->next_archetype = 0;
  world->generations = kh_init(gen);
  world->live = kh_init(live);
  world->component_index = kh_init(component_archetypes);
  world->entity_index = kh_init(entity_data);
  kv_init(world->archetypes);
  return world;
}

void *get_component(World *world, Object entity, Object component) {}
