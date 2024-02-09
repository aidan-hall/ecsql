#include <arena.h>
#include <ecs/ecs.h>
#include <klib/khash.h>
#include <klib/ksort.h>
#include <klib/kvec.h>
#include <lisp/object.h>
#include <stdlib.h>

KSORT_INIT(sort_type, Object, COMP_LT)

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

static inline bool types_match(Type a, Type b) {
  if (kv_size(a) != kv_size(b)) {
    return false;
  }
  /* Check that the Component vectors match. */
  for (size i = 0; i < kv_size(a); ++i) {
    if (ENT_SIG(kv_A(a, i)) != ENT_SIG(kv_A(b, i)))
      return false;
  }
  return true;
}

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

/* Entity ID (32 bits) → Record
 * We don't use 64-bit keys because main ID is unique at any point in time. */
KHASH_MAP_INIT_INT(entity_data, Record);
/* Component *signature* (64 bits) → Component Metadata */
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
  kvec_t(Archetype) archetypes;
  ArchetypeID empty_archetype;
  struct {
    Object storage;
    Object struct_member;
  } comp;
} World;

static inline Archetype *get_archetype(World *world, ArchetypeID archetype) {
  assert(archetype.val < kv_size(world->archetypes));
  return &kv_A(world->archetypes, archetype.val);
}

/* Returns the generation for the given id stored in the generations hashmap.
 * The pointer is valid until the next update of the generations hashmap. */
u16 *entity_generation(World *world, EntityID id) {
  khint_t iter = kh_get(gen, world->generations, id.val);

  int absent;

  if (iter == kh_end(world->generations)) {
    /* This ID has never been used before: "start" at generation 0. */
    iter = kh_put(gen, world->generations, id.val, &absent);
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

static inline Record *entity_record(World *world, EntityID id) {
  return &kh_value(world->entity_index,
                   kh_get(entity_data, world->entity_index, id.val));
}

Type ecs_type(World *world, Object entity) {
  u32 aid = entity_record(world, entity.id)->archetype.val;
  assert(aid < kv_size(world->archetypes));

  return kv_A(world->archetypes, aid).type;
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

  return ENT_BOX((EntityID){id}, *entity_generation(world, (EntityID){id}));
}

/* Add the Entity to the Archetype.
 * Returns the row in the Archetype where the entity is stored. */
static size archetype_add_entity(World *world, Archetype *archetype,
                                 EntityID eid) {
  khiter_t iter = kh_get(entity_data, world->entity_index, eid.val);

  /* Add a new row of storage for the Entity in each Column. */
  for (size i = 0; i < kv_size(archetype->columns); ++i) {
    column_add(&(kv_A(archetype->columns, i)), 1);
  }
  /* The new row for the Entity is added at the bottom, so we use the length of
   * the 'entities' array before adding it to get the right index. */
  size row = kv_size(archetype->entities);
  kh_value(world->entity_index, iter) =
      (Record){.archetype = archetype->id, .row = row};
  kv_push(EntityID, archetype->entities, eid);
  return row;
}

Object ecs_new(World *world) {
  Object entity = new_entity_id_partitioned(world, &world->next_entity,
                                            MIN_ENTITY, MAX_ENTITY);
  u32 eid = entity.id.val;
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
  archetype_add_entity(world, get_archetype(world, world->empty_archetype),
                       entity.id);
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
static void archetype_remove_entity(World *world, Archetype *archetype, size row) {
  typeof(archetype->entities) *ids = &(archetype->entities);
  size end = kv_size(*ids) - 1;
  /* Packing is maintained by moving the Entity at the back into the given row.
   * We can skip this step if the Entity removed was the last one in the
   * Archetype. */

  if (row == end) {
    /* We still need to reduce the size of each Column in this case. */
    for (size i = 0; i < kv_size(archetype->columns); ++i) {
      kv_A(archetype->columns, i).count -= 1;
    }
  } else {
    /* Move the Entity ID */
    EntityID moved_id = kv_A(*ids, row) = kv_pop(*ids);

    /* Move the data from the last row into the gap. */
    for (size i = 0; i < kv_size(archetype->columns); ++i) {
      Column *col = &kv_A(archetype->columns, i);
      memcpy(column_at(col, row), column_at(col, end), col->element_size);
      col->count -= 1;
    }

    /* Update the moved Entity's stored row to the one it was moved into. */
    kh_value(world->entity_index,
             kh_get(entity_data, world->entity_index, moved_id.val))
        .row = row;
  }
}

void destroy_entity(World *world, Object entity) {
  u32 id = entity.id.val;
  khint_t live_iter = kh_get(live, world->live, id);
  if (live_iter == kh_end(world->live)) {
    fprintf(stderr, "Attempted to destroy an entity that was not alive.\n");
    exit(1);
  }
  /* Remove the Entity from its archetype. */
  Record *record = entity_record(world, entity.id);
  Archetype *archetype = get_archetype(world, record->archetype);
  archetype_remove_entity(world, archetype, record->row);
  /* TODO: Do any necessary cleanup of its relations (e.g. cascade delete
   * children). */
  /* Remove the Entity from the live set */
  kh_del(live, world->live, live_iter);
  /* Update generation "lazily" when the Entity is destroyed. */
  *entity_generation(world, entity.id) += 1;
}

static inline ArchetypeMap *component_archetypes(World *world,
                                                 Object component) {
  u64 sig = ENT_SIG(component);
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

static Archetype init_archetype(World *world, Type type) {
  Archetype archetype;
  archetype.id = (ArchetypeID){world->next_archetype++};
  kv_init(archetype.entities);
  kv_init(archetype.type);
  kv_init(archetype.columns);
  kv_init(archetype.component_columns);
  for (size i = 0; i < kv_size(type); ++i) {
    /* Initialise Columns based on type: if a Component has Storage, register
     * this as an archetype that has that Component, stored at the next Column.
     */
    Object component = kv_A(type, i);
    struct Storage *storage = ecs_get(world, component, world->comp.storage);
    ArchetypeMap *archetypes = component_archetypes(world, component);
    size col;
    /* Only assign a Column if the Component requires storage. */
    if (storage != NULL) {
      col = kv_size(archetype.columns);
      kv_push(Column, archetype.columns, init_column(*storage));
    } else {
      col = NOT_PRESENT;
    }
    kv_push(size, archetype.component_columns, col);
    int absent;
    khiter_t iter = kh_put(component_archetype_column, archetypes,
                           archetype.id.val, &absent);
    if (absent < 0) {
      fprintf(stderr,
              "Failed to add a new Archetype to a Component (%lx)'s set of "
              "archetypes.\n",
              component.bits);
      exit(1);
    }
    kh_value(archetypes, iter) = (ArchetypeRecord){col};
  }
  kv_copy(Object, archetype.type, type);
  archetype.neighbours = kh_init(archetype_edge);
  return archetype;
}

ArchetypeID type_archetype(World *world, Type type) {
  typeof(world->archetypes) as = world->archetypes;
  for (size i = 0; i < kv_size(as); ++i) {
    if (types_match(kv_A(as, i).type, type)) {
      return kv_A(as, i).id;
    }
  }
  kv_push(Archetype, world->archetypes, init_archetype(world, type));
  return kv_A(world->archetypes, kv_size(world->archetypes) - 1).id;
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
  world->empty_archetype = type_archetype(world, some_type);

  /* Set up internal Components */
  world->comp.storage = ecs_new(world);
  add_entity_name(world, world->comp.storage, "storage");
  kv_push(Object, some_type, world->comp.storage);
  Archetype *only_storage_archetype =
      get_archetype(world, type_archetype(world, some_type));
  /* Create the Column to store Storage */
  struct Storage storage_storage = (struct Storage){
      .size = sizeof(struct Storage), .alignment = alignof(struct Storage)};
  kv_push(Column, only_storage_archetype->columns,
          init_column(storage_storage));
  /* Register that as the Column to store Storage in this Archetype. */
  ArchetypeMap *archetypes = component_archetypes(world, world->comp.storage);
  int absent;
  khiter_t iter = kh_put(component_archetype_column, archetypes,
                         only_storage_archetype->id.val, &absent);
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
  Archetype *archetype = &kv_A(world->archetypes, record->archetype.val);
  ArchetypeMap *archetypes = component_archetypes(world, component);
  return kh_get(component_archetype_column, archetypes, archetype->id.val) !=
         kh_end(archetypes);
}

/* Get the Component data of the given type for the given Entity.
 * Returns NULL if that Entity doesn't have storage allocated for that
 * Component. */
void *ecs_get(World *world, Object entity, Object component) {
  Record *record = entity_record(world, entity.id);
  Archetype *archetype = &kv_A(world->archetypes, record->archetype.val);

  ArchetypeMap *archetypes = component_archetypes(world, component);
  khiter_t iter =
      kh_get(component_archetype_column, archetypes, archetype->id.val);
  if (iter == kh_end(archetypes)) {
    return NULL;
  }
  ArchetypeRecord *a_record = &kh_value(archetypes, iter);
  size col_idx = a_record->column;

  if (col_idx == NOT_PRESENT) {
    return NULL;
  }

  Column *col = &kv_A(archetype->columns, col_idx);
  return column_at(col, record->row);
}

/* Get the Archetype that doesn't have 'component' if 'archetype' has it,
 * and vice versa if 'archetype' doesn't have 'component'. */
ArchetypeID toggle_component(World *world, ArchetypeID archetype_id,
                             Object component) {
  Archetype *archetype = get_archetype(world, archetype_id);
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
  if (pos != NOT_PRESENT) {
    /* Removing the component */
    memmove(&kv_A(other, pos), &kv_A(other, pos + 1),
            (kv_size(other) - pos - 1) * sizeof(Object));
    IGNORE(kv_pop(other));
  } else {
    /* Adding the component */
    kv_push(Object, other, component);
    ks_introsort(sort_type, kv_size(other), &kv_A(other, 0));
  }

  ArchetypeID res_id = type_archetype(world, other);
  /* The original archetype poiner may have been invalidated if a new archetype
   * was created. */
  archetype = get_archetype(world, archetype_id);
  /* Add the archetypes as neighbours */
  int absent;
  iter = kh_put(archetype_edge, archetype->neighbours, sig, &absent);
  if (absent < 0) {
    exit(1);
  }
  kh_value(archetype->neighbours, iter) = res_id;
  Archetype *res = get_archetype(world, res_id);
  iter = kh_put(archetype_edge, res->neighbours, sig, &absent);
  if (absent < 0) {
    exit(1);
  }
  kh_value(res->neighbours, iter) = archetype_id;

  kv_destroy(other);
  return res_id;
}

/* TODO: Refactor to 'move_entities' */
static void move_entity(World *world, ArchetypeID from_id, size from_row,
                        ArchetypeID to_id) {
  Archetype *from = get_archetype(world, from_id);
  Archetype *to = get_archetype(world, to_id);
  EntityID eid = kv_A(from->entities, from_row);
  size to_row = archetype_add_entity(world, to, eid);
  /* Type vectors are sorted, so we can iterate forward through both, and copy
   * when they match. */
  for (size f = 0, t = 0; f < kv_size(from->type) && t < kv_size(to->type);) {
    if (COMP_EQUIV(kv_A(from->type, f), kv_A(to->type, t))) {
      Object component = kv_A(from->type, f);
      if (kv_A(from->component_columns, f) != NOT_PRESENT) {
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
      if (COMP_LT(kv_A(from->type, f), kv_A(to->type, t))) {
        f++;
      } else {
        break;
      }
    }
    while (t < kv_size(to->type)) {
      if (COMP_LT(kv_A(to->type, t), kv_A(from->type, f))) {
        t++;
      } else {
        break;
      }
    }
  }

  archetype_remove_entity(world, from, from_row);
}

void ecs_add(World *world, Object entity, Object component) {
  Record *record = entity_record(world, entity.id);
  if (ecs_has(world, entity, component)) {
    fprintf(stderr,
            "BAD: Attempt to add a Component (%lx) that an Entity (%u) already "
            "has.\n",
            component.bits, entity.id.val);
    return;
  }

  move_entity(world, record->archetype, record->row,
              toggle_component(world, record->archetype, component));
}

void ecs_remove(World *world, Object entity, Object component) {
  Record *record = entity_record(world, entity.id);
  if (!ecs_has(world, entity, component)) {
    fprintf(stderr,
            "BAD: Attempt to remove a Component (%lx) that an Entity (%u) "
            "didn't have.\n",
            component.bits, entity.id.val);
    return;
  }

  move_entity(world, record->archetype, record->row,
              toggle_component(world, record->archetype, component));
}
