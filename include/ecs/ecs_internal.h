#ifndef ECS_INTERNAL_H
#define ECS_INTERNAL_H

/* Private information that we should not expose outside the ECS library. */

#include "ecs/query.h"
#include <ecs/ecs.h>
#include <klib/khash.h>
#include <klib/ksort.h>
#include <lisp/object.h>

/* Storage for a single Component type in an Archetype. */
typedef struct Column {
  u8 *elements;
  size element_size;
  size capacity;
  size count;
} Column;

/* Information about where an Entity is stored */
typedef struct Record {
  /* What archetype an Entity is in. */
  ArchetypeID archetype;
  /* Index into the Archetype's component arrays where the Entity's data is
   * stored. */
  size row;
} Record;

typedef struct ArchetypeRecord {
  /* At what column in an Archetype is data for a give Component type stored,
   * or NOT_PRESENT if the Component has no data storage. */
  size column;
} ArchetypeRecord;

/* Archetype ID (u32) → ArchetypeRecord */
KHASH_MAP_INIT_INT(component_archetype_column, ArchetypeRecord);
typedef khash_t(component_archetype_column) ArchetypeMap;

KHASH_MAP_INIT_INT64(archetype_edge, ArchetypeID);

typedef struct Archetype {
  ArchetypeID id;
  Type type;
  /* Parallel with type: NOT_PRESENT if that Component does not have Storage,
   * otherwise, the Column where its data is stored in this Archetype. */
  kvec_t(size) component_columns;
  /* A special column for entity IDs. */
  kvec_t(EntityID) entities;
  kvec_t(Column) columns;

  /* Component ID → Archetype with that Component removed from/added to this
   * one. */
  khash_t(archetype_edge) * neighbours;
} Archetype;

/* Entity ID (32 bits) → Record
 * We don't use 64-bit keys because main ID is unique at any point in time. */
KHASH_MAP_INIT_INT(entity_data, Record);
/* Component *signature* (64 bits) → Component Metadata */
KHASH_MAP_INIT_INT64(component_metadata, ArchetypeMap *);
/* symbol → Entity for live entities */
KHASH_MAP_INIT_INT64(entity_name, Object);

typedef struct EcsIter {
  Archetype *archetype;
  /* The number of Columns of each Archetype accessed */
  size n_columns;
  /* Column in 'archetype' where each relevant Component can be found. */
  size *columns;
} EcsIter;

/* Cached Queries, a mostly abandoned concept.
 *
 * They store the set of Archetypes that satisfied a Query when it ran. */
typedef struct CachedQuery {
  kvec_t(ArchetypeID) archetypes;
  size n_columns;
  /* The column indices for all Archetypes, in one contiguous allocation. */
  kvec_t(size) columns;
  CachedQueryID id;
} CachedQuery;

/* An ECS world */
typedef struct World {
  /* Stores the Generation for each Entity ID, initialised lazily */
  khash_t(gen) * generations;
  /* Stores the set of IDs of living Entities */
  khash_t(live) * live;
  /* Maps symbols to Entities. */
  khash_t(entity_name) * entity_names;
  /* The next ID to try to use when creating an Entity */
  u32 next_entity;
  /* The next ID to use for an Archetype */
  u32 next_archetype;

  /* Refer to project.pdf for an explanation of these. */
  khash_t(entity_data) * entity_index;
  khash_t(component_metadata) * component_index;
  kvec_t(Archetype) archetypes;
  kvec_t(CachedQuery) cached_queries;
  /* The empty Archetype, for Entities with no Components */
  ArchetypeID empty_archetype;
  /* The World Components of this World */
  WorldComponents comp;
} World;

/* Look up an Archetype by its ID */
static inline Archetype *get_archetype(World *world, ArchetypeID archetype) {
  assert(archetype.val < kv_size(world->archetypes));
  return &kv_A(world->archetypes, archetype.val);
}

size ecs_archetype_component_column(struct World *world, Archetype *archetype,
                                    Object component);
#endif
