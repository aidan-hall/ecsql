#ifndef ECS_INTERNAL_H
#define ECS_INTERNAL_H

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

typedef struct World {
  khash_t(gen) * generations;
  khash_t(live) * live;
  khash_t(entity_name) * entity_names;
  u32 next_entity;
  u32 next_archetype;

  khash_t(entity_data) * entity_index;
  khash_t(component_metadata) * component_index;
  kvec_t(Archetype) archetypes;
  ArchetypeID empty_archetype;
  struct {
    Object storage;
  } comp;
} World;

static inline Archetype *get_archetype(World *world, ArchetypeID archetype) {
  assert(archetype.val < kv_size(world->archetypes));
  return &kv_A(world->archetypes, archetype.val);
}

size ecs_archetype_component_column(struct World *world, Archetype *archetype,
                                    Object component);
#endif
