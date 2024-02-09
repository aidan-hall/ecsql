#include <arena.h>
#include <ecs.h>
#include <klib/khash.h>
#include <klib/ksort.h>
#include <klib/kvec.h>
#include <lisp/object.h>
#include <stdlib.h>

#define OBJ_LT(A, B) ((A).bits < (B).bits)
KSORT_INIT(Object, Object, OBJ_LT)

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

/* Storage for a single Component type in an Archetype. */
typedef struct Column {
  u8 *elements;
  size element_size;
  size capacity;
  size count;
} Column;

#define INITIAL_ARCHETYPE_CAPACITY (16)

static Column init_column(struct Storage storage) {
  /* We may have to manually manage the alignment if size ever excludes padding,
   * so we will keep this parameter around for now. */
  Column col = {.count = 0,
                /* Pre-allocate enough space for a few Entities.
                 * A moderate initial capacity prevents frequent reallocations
                 * when adding the first c. 20 members of the Archetype. */
                .capacity = INITIAL_ARCHETYPE_CAPACITY,
                .element_size = storage.size};
  col.elements = calloc(col.capacity, col.element_size);
  if (col.elements == NULL) {
    fprintf(stderr, "Failed to create a column.\n");
    exit(1);
  }
  return col;
}

/* Allocate an additional n spaces in the Column. */
static void column_add(Column *column, size n) {
  size required_capacity = column->count + n;
  while (column->capacity < required_capacity) {
    /* 2 is apparently a reasonable growth factor */
    column->capacity *= 2;
    column->elements =
        realloc(column->elements, column->capacity * column->element_size);
    if (column->elements == NULL) {
      fprintf(stderr, "Failed to extend column.\n");
      exit(1);
    }
  }
  column->count += n;
}

static inline void *column_at(Column *column, size row) {
  return (void *)&(column->elements[row * column->element_size]);
  /* return col->data.begin + (col->element_size * record->row); */
}

KHASH_MAP_INIT_INT64(archetype_edge, struct Archetype *);

typedef struct Archetype {
  u32 id;
  Type type;
  /* Parallel with type: -1 if that Component does not have Storage,
   * otherwise, the Column where its data is stored in this Archetype. */
  kvec_t(size) component_columns;
  /* A special column for entity IDs. */
  kvec_t(u32) entities;
  kvec_t(Column) columns;

  /* Component ID → Archetype with that Component removed from/added to this
   * one. */
  khash_t(archetype_edge) * neighbours;
} Archetype;

static inline bool types_match(Type a, Type b) {
  if (kv_size(a) != kv_size(b)) {
    return false;
  }
  /* Check that the Component vectors match. */
  for (size i = 0; i < kv_size(a); ++i) {
    if (kv_A(a, i).sig != kv_A(b, i).sig)
      return false;
  }
  return true;
}

/* Information about where an Entity is stored */
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

/* Archetype ID (u32) → ArchetypeRecord */
KHASH_MAP_INIT_INT(component_archetype_column, ArchetypeRecord);
typedef khash_t(component_archetype_column) ArchetypeMap;

/* Entity ID (32 bits) → Record
 * We don't use 64-bit keys because main ID is unique at any point in time. */
KHASH_MAP_INIT_INT(entity_data, Record);
/* Component (64 bits) → Component Metadata */
KHASH_MAP_INIT_INT64(component_metadata, ArchetypeMap *);
/* u8* name → Entity */
KHASH_MAP_INIT_STR(names, Object);

typedef struct World {
  khash_t(gen) * generations;
  khash_t(live) * live;
  u32 next_entity;
  u32 next_archetype;

  khash_t(names) * entity_names;
  khash_t(entity_data) * entity_index;
  khash_t(component_metadata) * component_index;
  kvec_t(Archetype *) archetypes;
  Archetype *empty_archetype;
  struct {
    Object storage;
    Object struct_member;
  } comp;
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
  return *entity_generation(world, entity.id) == entity.gen;
}

static inline Record *entity_record(World *world, u32 id) {
  return &kh_value(world->entity_index,
                   kh_get(entity_data, world->entity_index, id));
}

static Object new_entity_id_partitioned(World *world, u32 *start, u32 low,
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

/* Object new_component(World *world) { */
/*   /\* TODO: Add it to the empty Archetype *\/ */
/*   return new_entity_id_partitioned(world, &world->next_entity, MIN_COMPONENT,
 */
/*                                    MAX_COMPONENT); */
/* } */

/* Add the Entity to the Archetype.
 * Returns the row in the Archetype where the entity is stored. */
size archetype_add_entity(World *world, Archetype *archetype, u32 eid) {
  khiter_t iter = kh_get(entity_data, world->entity_index, eid);
  /* Add a new row of storage for the Entity in each Column. */
  for (size i = 0; i < kv_size(archetype->columns); ++i) {
    column_add(&(kv_A(archetype->columns, i)), 1);
  }
  /* The new row for the Entity is added at the bottom, so we use the length of
   * the 'entities' array before adding it to get the right index. */
  size row = kv_size(archetype->entities);
  kh_value(world->entity_index, iter) =
      (Record){.archetype = archetype, .row = row};
  kv_push(u32, archetype->entities, eid);
  return row;
}

Object ecs_new(World *world) {
  Object entity = new_entity_id_partitioned(world, &world->next_entity,
                                            MIN_ENTITY, MAX_ENTITY);
  u32 eid = entity.id;
  /* Add the entity to the Entity index, if necessary */
  khiter_t iter = kh_get(entity_data, world->entity_index, eid);
  if (iter == kh_end(world->entity_index)) {
    int absent;
    iter = kh_put(entity_data, world->entity_index, eid, &absent);
    if (absent < 0) {
      fprintf(stderr, "Failed to add Entity to entity index: %u.\n", eid);
      exit(1);
    }
  }
  archetype_add_entity(world, world->empty_archetype, eid);
  return entity;
}

void add_entity_name(World *world, Object entity, const char *name) {
  khiter_t iter = kh_get(names, world->entity_names, (char *)name);
  if (iter != kh_end(world->entity_names)) {
    fprintf(stderr, "Attempted to reuse entity name '%s'\n", name);
    exit(1);
  }
  int absent;
  iter = kh_put(names, world->entity_names, (char *)name, &absent);
  if (absent < 0) {
    fprintf(stderr, "Failed to add entity name.\n");
    exit(1);
  }
  kh_value(world->entity_names, iter) = entity;
}

/* Remove the Entity in the given row, and maintain packing. */
void archetype_remove_entity(World *world, Archetype *archetype, size row) {
  typeof(archetype->entities) *ids = &(archetype->entities);
  size end = kv_size(*ids) - 1;
  /* Packing is maintained by moving the Entity at the back into the given row.
   * This is not necessary if the removed Entity is in the last row, but that is
   * such a statistically rare case that it makes more sense to always perform
   * the move, since it is not incorrect in any case.
   */

  /* Move the Entity ID */
  u32 moved_id = kv_A(*ids, row) = kv_pop(*ids);

  /* Move the Column data */
  for (size i = 0; i < kv_size(archetype->columns); ++i) {
    Column *col = &kv_A(archetype->columns, i);
    /* The regions could overlap if we are operating on the last row,
     * but in that case we're discarding the data anyway so we don't care. */
    memcpy(column_at(col, row), column_at(col, end), col->element_size);
    col->count -= 1;
  }

  /* Update the row value for the moved Entity */
  kh_value(world->entity_index,
           kh_get(entity_data, world->entity_index, moved_id))
      .row = row;
}

void destroy_entity(World *world, Object entity) {
  u32 id = entity.id;
  khint_t live_iter = kh_get(live, world->live, id);
  if (live_iter == kh_end(world->live)) {
    fprintf(stderr, "Attempted to destroy an entity that was not alive.\n");
    exit(1);
  }
  /* Remove the Entity from its archetype. */
  Record *record = entity_record(world, entity.id);
  archetype_remove_entity(world, record->archetype, record->row);
  /* TODO: Do any necessary cleanup of its relations (e.g. cascade delete
   * children). */
  /* Remove the Entity from the live set */
  kh_del(live, world->live, live_iter);
  /* Update generation "lazily" when the Entity is destroyed. */
  *entity_generation(world, id) += 1;
}

static inline ArchetypeMap *component_archetypes(World *world,
                                                 Object component) {
  u64 sig = component.sig;
  khiter_t iter = kh_get(component_metadata, world->component_index, sig);
  if (iter == kh_end(world->component_index)) {
    int absent;
    iter = kh_put(component_metadata, world->component_index, sig, &absent);
    if (absent < 0) {
      fprintf(stderr,
              "Failed to add a Component (%lx) to the component index\n",
              component.bits);
      exit(1);
    }
    kh_value(world->component_index, iter) =
        kh_init(component_archetype_column);
  }
  return kh_value(world->component_index, iter);
}

void init_archetype(World *world, Archetype *archetype, Type type) {
  archetype->id = world->next_archetype++;
  kv_init(archetype->entities);
  kv_init(archetype->type);
  kv_init(archetype->columns);
  kv_init(archetype->component_columns);
  for (size i = 0; i < kv_size(type); ++i) {
    /* Initialise Columns based on type: if a Component has Storage, register
     * this as an archetype that has that Component, stored at the next Column.
     */
    Object component = kv_A(type, i);
    struct Storage *storage = ecs_get(world, component, world->comp.storage);
    ArchetypeMap *archetypes = component_archetypes(world, component);
    size col;
    /* Only assign a Column in the Archetype if the Component requires storage. */
    if (storage != NULL) {
      col = kv_size(archetype->columns);
      kv_push(Column, archetype->columns, init_column(*storage));
    } else {
      col = -1;
    }
    kv_push(size, archetype->component_columns, col);
    int absent;
    khiter_t iter =
        kh_put(component_archetype_column, archetypes, archetype->id, &absent);
    if (absent < 0) {
      fprintf(stderr,
              "Failed to add a new Archetype to a Component (%lx)'s set of "
              "archetypes.\n",
              component.bits);
      exit(1);
    }
    kh_value(archetypes, iter) = (ArchetypeRecord){col};
  }
  kv_copy(Object, archetype->type, type);
  archetype->neighbours = kh_init(archetype_edge);
}

struct Archetype *get_archetype(World *world, Type type) {
  typeof(world->archetypes) as = world->archetypes;
  for (size i = 0; i < kv_size(as); ++i) {
    if (types_match(kv_A(as, i)->type, type)) {
      return kv_A(as, i);
    }
  }
  kv_push(Archetype *, world->archetypes, malloc(sizeof(struct Archetype)));
  Archetype *a = kv_A(world->archetypes, kv_size(world->archetypes) - 1);
  if (a == NULL) {
    fprintf(stderr, "Failed to allocate memory for a new Archetype.\n");
    exit(1);
  }
  init_archetype(world, a, type);
  return a;
}

Object ecs_new_component(World *world, struct Storage storage) {
  Object obj = ecs_new(world);
  ecs_add(world, obj, world->comp.storage);
  *(struct Storage *)ecs_get(world, obj, world->comp.storage) = storage;
  return obj;
}

struct World *init_world() {
  World *world = malloc(sizeof(World));
  *world = (World){0};
  world->next_entity = MIN_ENTITY;
  world->generations = kh_init(gen);
  world->live = kh_init(live);
  world->entity_names = kh_init(names);
  world->component_index = kh_init(component_metadata);
  world->entity_index = kh_init(entity_data);
  kv_init(world->archetypes);
  Type some_type;
  kv_init(some_type);
  world->empty_archetype = get_archetype(world, some_type);

  /* Set up internal Components */
  world->comp.storage = ecs_new(world);
  add_entity_name(world, world->comp.storage, "storage");
  kv_push(Object, some_type, world->comp.storage);
  Archetype *only_storage_archetype = get_archetype(world, some_type);
  /* Create the Column to store Storage */
  struct Storage storage_storage = (struct Storage){
      .size = sizeof(struct Storage), .alignment = alignof(struct Storage)};
  kv_push(Column, only_storage_archetype->columns,
          init_column(storage_storage));
  /* Register that as the Column to store Storage in this Archetype. */
  ArchetypeMap *archetypes = component_archetypes(world, world->comp.storage);
  int absent;
  khiter_t iter = kh_put(component_archetype_column, archetypes,
                         only_storage_archetype->id, &absent);
  if (absent < 0) {
    fprintf(stderr, "Failed to register the [Storage] archetype for the "
                    "Storage Component.\n");
    exit(1);
  }
  /* We know Storage is the only (i.e. first) Column in the [Storage] archetype,
   * so we can hard-code column index 0. */
  kh_value(archetypes, iter) = (ArchetypeRecord){.column = 0};
  kv_push(size, only_storage_archetype->component_columns, 0);
  /* Add the Storage Component to itself, moving it into the [Storage]
   * archetype. */
  ecs_add(world, world->comp.storage, world->comp.storage);
  *(struct Storage *)ecs_get(world, world->comp.storage, world->comp.storage) =
      storage_storage;

  world->comp.struct_member = ECS_NEW_COMPONENT(world, struct StructMember);
  return world;
}

bool ecs_has(World *world, Object entity, Object component) {
  Record *record = entity_record(world, entity.id);
  Archetype *archetype = record->archetype;
  ArchetypeMap *archetypes = component_archetypes(world, component);
  return kh_get(component_archetype_column, archetypes, archetype->id) !=
         kh_end(archetypes);
}

/* Get the Component data of the given type for the given Entity.
 * Returns NULL if that Entity doesn't have storage allocated for that
 * Component. */
void *ecs_get(World *world, Object entity, Object component) {
  Record *record = entity_record(world, entity.id);
  Archetype *archetype = record->archetype;

  ArchetypeMap *archetypes = component_archetypes(world, component);
  khiter_t iter = kh_get(component_archetype_column, archetypes, archetype->id);
  if (iter == kh_end(archetypes)) {
    return NULL;
  }
  ArchetypeRecord *a_record = &kh_value(archetypes, iter);
  size col_idx = a_record->column;
  /* -1: no data storage */
  if (col_idx == -1) {
    return NULL;
  }
  Column *col = &kv_A(archetype->columns, col_idx);
  return column_at(col, record->row);
}

/* Get the Archetype that doesn't have 'component' if 'archetype' has it,
 * and vice versa if 'archetype' doesn't have 'component'. */
Archetype *toggle_component(World *world, Archetype *archetype,
                            Object component) {
  u64 sig = ENT_SIG(component);
  khiter_t iter = kh_get(archetype_edge, archetype->neighbours, sig);
  /* A link to that Archetype has already been made. */
  if (iter != kh_end(archetype->neighbours)) {
    return kh_value(archetype->neighbours, iter);
  }

  printf("Adding new archetype link.\n");

  size pos = type_pos(archetype->type, component);
  /* Obtain the Type of the new Archetype to reference. */
  Type other;
  kv_init(other);
  kv_copy(Object, other, archetype->type);
  if (pos != -1) {
    /* Removing the component */
    memmove(&kv_A(other, pos), &kv_A(other, pos + 1),
            (kv_size(other) - pos - 1) * sizeof(Object));
    IGNORE(kv_pop(other));
  } else {
    /* Adding the component */
    kv_push(Object, other, component);
    ks_introsort(Object, kv_size(other), &kv_A(other, 0));
  }

  Archetype *res = get_archetype(world, other);
  /* Add the archetypes as neighbours */
  int absent;
  iter = kh_put(archetype_edge, archetype->neighbours, sig, &absent);
  if (absent < 0) {
    exit(1);
  }
  kh_value(archetype->neighbours, iter) = res;
  iter = kh_put(archetype_edge, res->neighbours, sig, &absent);
  if (absent < 0) {
    exit(1);
  }
  kh_value(res->neighbours, iter) = archetype;

  kv_destroy(other);
  return res;
}

/* TODO: Refactor to 'move_entities' */
void move_entity(World *world, Archetype *from, size from_row, Archetype *to) {
  u32 eid = kv_A(from->entities, from_row);
  size to_row = archetype_add_entity(world, to, eid);
  /* Type vectors are sorted, so we can iterate forward through both, and copy
   * when they match. */
  for (size f = 0, t = 0; f < kv_size(from->type) && t < kv_size(to->type);) {
    /* TODO: Should we use sig here? */
    if (EQ(kv_A(from->type, f), kv_A(to->type, t))) {
      /* TODO: Copy this Component's data over:
       * Identify the Column in each Archetype storing its data, if it has data,
       * Copy. */
      Object component = kv_A(from->type, f);
      /* component_columns[f] != -1 iff type[f] has storage */
      if (kv_A(from->component_columns, f) != -1) {
        size from_col = kv_A(from->component_columns, f);
        size to_col = kv_A(to->component_columns, t);

        struct Storage *storage =
            ecs_get(world, component, world->comp.storage);
        memcpy(column_at(&kv_A(to->columns, to_col), to_row),
               column_at(&kv_A(from->columns, from_col), from_row),
               storage->size);
      }
      f++;
      t++;
    }
    /* Move forward to the next potential match. */
    while (f < kv_size(from->type)) {
      /* Must perform bounds check before comparison */
      if (OBJ_LT(kv_A(from->type, f), kv_A(to->type, t))) {
        f++;
      } else {
        break;
      }
    }
    while (t < kv_size(to->type)) {
      if (OBJ_LT(kv_A(to->type, t), kv_A(from->type, f))) {
        t++;
      } else {
        break;
      }
    }
  }

  archetype_remove_entity(world, from, from_row);
}

void ecs_add(World *world, Object entity, Object component) {
  u32 id = entity.id;
  Record *record = entity_record(world, id);
  Archetype *archetype = record->archetype;
  if (ecs_has(world, entity, component)) {
    fprintf(stderr,
            "BAD: Attempt to add a Component (%lx) that an Entity (%u) already "
            "has.\n",
            component.bits, id);
    return;
  }

  Archetype *added = toggle_component(world, archetype, component);
  move_entity(world, archetype, record->row, added);
}

void ecs_remove(World *world, Object entity, Object component) {
  u32 eid = entity.id;
  Record *record = entity_record(world, eid);
  Archetype *archetype = record->archetype;
  if (!ecs_has(world, entity, component)) {
    fprintf(stderr,
            "BAD: Attempt to remove a Component (%u) that an Entity (%u) "
            "didn't have.\n",
            component.id, eid);
    return;
  }

  Archetype *removed = toggle_component(world, archetype, component);
  move_entity(world, archetype, record->row, removed);
}
