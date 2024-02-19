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

#endif
