#include "ecs/ecs.h"
#include "klib/kvec.h"
#include "lisp/lisp.h"
#include "lisp/object.h"
#include <ecs/ecs_internal.h>
#include <ecs/query.h>

typedef struct EcsIter {
  Archetype *archetype;
  /* The number of Columns of each Archetype accessed */
  size n_columns;
  /* Column in 'archetype' where each relevant Component can be found. */
  size *columns;
} EcsIter;

typedef struct CachedQuery {
  kvec_t(ArchetypeID) archetypes;
  size n_columns;
  /* The column indices for all Archetypes, in one contiguous allocation. */
  kvec_t(size) columns;
} CachedQuery;

/* Query: (components-to-get . predicate)
 * Predicate: Component | Relation | (and ...) | (or ...) | (not Predicate)
 */

static bool ecs_query_matches(LispEnv *lisp, ArchetypeID archetype,
                              Object predicate) {
  struct World *world = lisp->world;
  switch (OBJ_TYPE(predicate)) {
  case OBJ_PAIR_TAG: {
    Object car = LISP_CAR(lisp, predicate);
    if (EQ(car, lisp->keysyms.and)) {
      predicate = LISP_CDR(lisp, predicate);
      while (OBJ_TYPE(predicate) == OBJ_PAIR_TAG) {
        if (!ecs_query_matches(lisp, archetype, LISP_CAR(lisp, predicate))) {
          return false;
        }

        predicate = LISP_CDR(lisp, predicate);
      }
      return true;
    } else if (EQ(car, lisp->keysyms.not )) {
      return !ecs_query_matches(lisp, archetype, LISP_CAR(lisp, predicate));
    } else if (EQ(car, lisp->keysyms.or)) {
      predicate = LISP_CDR(lisp, predicate);
      while (OBJ_TYPE(predicate) == OBJ_PAIR_TAG) {
        if (ecs_query_matches(lisp, archetype, LISP_CAR(lisp, predicate))) {
          return true;
        }

        predicate = LISP_CDR(lisp, predicate);
      }
      return false;
    }
    WRONG("Malformed query", predicate);
    return false;
  }
  case OBJ_ENTITY_TAG:
  case OBJ_RELATION_TAG:
    return ecs_archetype_has(world, archetype, predicate);
  default:
    WRONG("Malformed query", predicate);
    return false;
  }

  return false;
}

void ecs_do_query(LispEnv *lisp, Object query, SystemFunc *func, void *data) {
  struct World *world = lisp->world;
  /* TODO: Calculate how many Components the Query contains */
  Object components = LISP_CAR(lisp, query);
  Object predicate = LISP_CDR(lisp, query);
  EcsIter foo;
  foo.n_columns = lisp_vector_length(components);
  foo.columns = calloc(foo.n_columns, sizeof(foo.columns[0]));

  for (size i = 0; i < kv_size(world->archetypes); ++i) {
    ArchetypeID id = kv_A(world->archetypes, i).id;
    if (ecs_query_matches(lisp, id, predicate)) {
      Archetype *archetype = get_archetype(world, id);
      foo.archetype = archetype;
      /* Produce the array of Component columns */
      for (size j = 0; j < foo.n_columns; ++j) {
        foo.columns[j] = ecs_archetype_component_column(
            world, archetype, *lisp_get_vector_item(lisp, components, j));
      }
      func(lisp, &foo, data);
    }
  }

  free(foo.columns);
}

void ecs_do_cached_query(LispEnv *lisp, CachedQuery *query, SystemFunc *func,
                         void *data) {
  typeof(query->archetypes) archetypes = query->archetypes;
  World *world = lisp->world;
  EcsIter iter = {.n_columns = query->n_columns};
  for (size i = 0; i < kv_size(archetypes); ++i) {
    iter.archetype = get_archetype(world, kv_A(archetypes, i));
    /* Columns for all Archetypes are stored contiguously,
     * so apply a stride of query->n_columns. */
    iter.columns = &kv_A(query->columns, i * query->n_columns);
    func(lisp, &iter, data);
  }
}

static void add_archetype_to_cache(LispEnv *lisp, EcsIter *iter, void *data) {
  CachedQuery *cache = (CachedQuery *)data;
  if (iter->n_columns != cache->n_columns) {
    WRONG("Mismatched column counts of new archetype and cache",
          OBJ_IMM((i32)iter->n_columns));
  }
  kv_push(ArchetypeID, cache->archetypes, iter->archetype->id);
  for (size i = 0; i < iter->n_columns; ++i) {
    kv_push(size, cache->columns, iter->columns[i]);
  }
}

struct CachedQuery *ecs_query(LispEnv *lisp, Object query) {
  CachedQuery *q = malloc(sizeof(CachedQuery));
  kv_init(q->archetypes);
  kv_init(q->columns);
  q->n_columns = lisp_vector_length(LISP_CAR(lisp, query));
  ecs_do_query(lisp, query, add_archetype_to_cache, q);
  return q;
}

void ecs_destroy_cached_query(struct CachedQuery *query) {
  kv_destroy(query->archetypes);
  kv_destroy(query->columns);
  free(query);
}

EntityID *ecs_iter_ids(LispEnv *lisp, struct EcsIter *iter) {
  return &kv_A(iter->archetype->entities, 0);
}

size ecs_iter_count(struct EcsIter *iter) {
  return kv_size(iter->archetype->entities);
}

void *ecs_iter_get(LispEnv *lisp, struct EcsIter *iter, size index) {
  if (index == NOT_PRESENT) {
    return NULL;
  }
  return kv_A(iter->archetype->columns, index).elements;
}
