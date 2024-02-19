#include "ecs/ecs.h"
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
  size n_components = lisp_vector_length(components);
  Object predicate = LISP_CDR(lisp, query);
  EcsIter foo;
  foo.columns = calloc(n_components, sizeof(foo.columns[0]));

  for (size i = 0; i < kv_size(world->archetypes); ++i) {
    ArchetypeID id = kv_A(world->archetypes, i).id;
    if (ecs_query_matches(lisp, id, predicate)) {
      Archetype *archetype = get_archetype(world, id);
      foo.archetype = id;
      foo.count = ecs_archetype_size(world, archetype);
      /* Produce the array of Component columns */
      for (size j = 0; j < n_components; ++j) {
        foo.columns[j] = ecs_archetype_component_column(
            world, archetype, *lisp_get_vector_item(lisp, components, j));
      }
      func(lisp, &foo, data);
    }
  }

  free(foo.columns);
}

static void add_archetype_to_cache(LispEnv *lisp, EcsIter *iter, void *data) {
  CachedQuery *cache = (CachedQuery *)data;
  kv_push(typeof(kv_A(cache->matches, 0)), cache->matches, *iter);
  size bytes = sizeof(iter->columns[0]) * iter->n_columns;
  void **columns_copy = malloc(bytes);
  /* The columns vector is only valid while the Query is executing, so make a
   * copy of it. */
  memcpy(&columns_copy, &iter->columns, bytes);
}

CachedQuery ecs_query(LispEnv *lisp, Object query) {
  CachedQuery q;
  kv_init(q.matches);
  ecs_do_query(lisp, query, add_archetype_to_cache, &q);
  return q;
}
