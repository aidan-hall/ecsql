#include "ecs/ecs.h"
#include "klib/kvec.h"
#include "lisp/lisp.h"
#include "lisp/object.h"
#include <ecs/ecs_internal.h>
#include <ecs/query.h>

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
    WRONG("Malformed query: unrecognised pair form", predicate);
    return false;
  }
  case OBJ_ENTITY_TAG:
  case OBJ_RELATION_TAG:
    return ecs_archetype_has(world, archetype, predicate);
  default:
    WRONG("Malformed query: invalid object type", predicate);
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
  foo.n_columns = lisp_length(lisp, components);
  foo.columns = calloc(foo.n_columns, sizeof(foo.columns[0]));

  for (size i = 0; i < kv_size(world->archetypes); ++i) {
    ArchetypeID id = kv_A(world->archetypes, i).id;
    if (ecs_query_matches(lisp, id, predicate)) {
      Archetype *archetype = get_archetype(world, id);
      foo.archetype = archetype;
      /* Produce the array of Component columns */
      for (size j = 0; j < foo.n_columns; ++j) {
        foo.columns[j] = ecs_archetype_component_column(
            world, archetype, LISP_CAR(lisp, components));
        components = LISP_CDR(lisp, components);
      }
      func(lisp, &foo, data);
    }
  }

  free(foo.columns);
}

void ecs_do_cached_query(LispEnv *lisp, CachedQueryID query_id, SystemFunc *func,
                         void *data) {
  CachedQuery *query = &kv_A(lisp->world->cached_queries, query_id.val);
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

CachedQueryID ecs_query(LispEnv *lisp, Object query) {
  CachedQuery q;
  q.id = (CachedQueryID){kv_size(lisp->world->cached_queries)};
  kv_init(q.archetypes);
  kv_init(q.columns);
  q.n_columns = lisp_length(lisp, LISP_CAR(lisp, query));
  ecs_do_query(lisp, query, add_archetype_to_cache, &q);
  kv_push(CachedQuery, lisp->world->cached_queries, q);
  return q.id;
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
