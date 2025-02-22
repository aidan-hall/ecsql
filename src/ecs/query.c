#include "ecs/ecs.h"
#include "klib/kvec.h"
#include "lisp/lisp.h"
#include "lisp/object.h"
#include <ecs/ecs_internal.h>
#include <ecs/query.h>

/* Predicate: Component | Relation | (and ...) | (or ...) | (not Predicate)
 * Determines whether 'archetype' satisfies 'predicate'.
 */
static bool ecs_query_matches(LispEnv *lisp, ArchetypeID archetype,
                              Object predicate) {
  struct World *world = lisp->world;
  switch (OBJ_TYPE(predicate)) {
  case OBJ_PAIR_TAG: {
    Object car = LISP_CAR(lisp, predicate);
    if (EQ(car, lisp->keysyms.and)) {
      /* and: all child predicates must match */
      predicate = LISP_CDR(lisp, predicate);
      while (OBJ_TYPE(predicate) == OBJ_PAIR_TAG) {
        if (!ecs_query_matches(lisp, archetype, LISP_CAR(lisp, predicate))) {
          return false;
        }

        predicate = LISP_CDR(lisp, predicate);
      }
      return true;
    } else if (EQ(car, lisp->keysyms.not_k)) {
      /* not: child predicate must not match */
      return !ecs_query_matches(lisp, archetype,
                                LISP_CAR(lisp, LISP_CDR(lisp, predicate)));
    } else if (EQ(car, lisp->keysyms.or)) {
      /* or: any child predicate must match */
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
    /* Component: archetype must have it. */
    return ecs_archetype_has(world, archetype, predicate);
  default:
    WRONG("Malformed query: invalid object type", predicate);
    return false;
  }

  /* Did not match (this should probably be unreachable...) */
  return false;
}

void ecs_do_pairwise_query(LispEnv *lisp, Object query0, Object query1,
                           NWiseSystem system, void *data) {
  struct World *world = lisp->world;
  Object components0 = LISP_CAR(lisp, query0);
  Object predicate0 = LISP_CDR(lisp, query0);
  EcsIter *foo[2] = {malloc(sizeof(EcsIter)), malloc(sizeof(EcsIter))};
  foo[0]->n_columns = lisp_length(lisp, components0);
  foo[0]->columns = calloc(foo[0]->n_columns, sizeof(foo[0]->columns[0]));
  Object components1 = LISP_CAR(lisp, query1);
  Object predicate1 = LISP_CDR(lisp, query1);
  foo[1]->n_columns = lisp_length(lisp, components1);
  foo[1]->columns = calloc(foo[1]->n_columns, sizeof(foo[1]->columns[1]));

  for (size i = 0; i < kv_size(world->archetypes); ++i) {

    Archetype *archetype_i = &kv_A(world->archetypes, i);
    ArchetypeID id = archetype_i->id;
    if (kv_size(archetype_i->entities) == 0) {
      continue;
    }
    Object traversal_components = components0;
    if (ecs_query_matches(lisp, id, predicate0)) {
      foo[0]->archetype = archetype_i;
      /* Produce the array of Component columns */
      for (size j = 0; j < foo[0]->n_columns; ++j) {
        Object the_component = LISP_CAR(lisp, traversal_components);
        foo[0]->columns[j] =
            ecs_archetype_component_column(world, archetype_i, the_component);
        traversal_components = LISP_CDR(lisp, traversal_components);
      }

      /* Repeat for the partner archetype->
       * The initial value of j depends on the intended behaviour.
       *  0: ordered pairs of *archetypes*,
       *  i: "unordered" pairs: never do (a, b) and (b, a) for distinct a & b.
       *  i + 1: unordered pairs, also excluding (a, a) for any archetype a.
       *
       * Ultimately, the N-Wise Queries functionality isn't very important, so it doesn't matter.
       */
      size j;
      switch (system.behaviour) {
      case NWISE_ALL:
        j = 0;
        break;
      case NWISE_DISTINCT:
        j = i;
        break;
      }
      for (; j < kv_size(world->archetypes); ++j) {

        Archetype *archetype_j = &kv_A(world->archetypes, j);
        ArchetypeID id = archetype_j->id;
        if (kv_size(archetype_j->entities) == 0) {
          continue;
        }
        traversal_components = components1;
        if (ecs_query_matches(lisp, id, predicate1)) {
          foo[1]->archetype = archetype_j;
          /* Produce the array of Component columns */
          for (size j = 0; j < foo[1]->n_columns; ++j) {
            Object the_component = LISP_CAR(lisp, traversal_components);
            foo[1]->columns[j] = ecs_archetype_component_column(
                world, archetype_j, the_component);
            traversal_components = LISP_CDR(lisp, traversal_components);
          }
          system.func(lisp, foo, data);
        }
      }
    }
  }
  free(foo[0]->columns);
  free(foo[1]->columns);
  free(foo[0]);
  free(foo[1]);
}

void ecs_do_query(LispEnv *lisp, Object query, SystemFunc *func, void *data) {
  struct World *world = lisp->world;
  Object components = LISP_CAR(lisp, query);
  Object predicate = LISP_CDR(lisp, query);
  EcsIter foo;
  foo.n_columns = lisp_length(lisp, components);
  foo.columns = calloc(foo.n_columns, sizeof(foo.columns[0]));

  for (size i = 0; i < kv_size(world->archetypes); ++i) {

    Archetype *archetype = &kv_A(world->archetypes, i);
    ArchetypeID id = archetype->id;
    if (kv_size(archetype->entities) == 0) {
      continue;
    }
    Object traversal_components = components;
    if (ecs_query_matches(lisp, id, predicate)) {
      foo.archetype = archetype;
      /* Produce the array of Component Columns for use in the EcsIter(ator).
       * These provide the System access to the Component data bound by its Query. */
      for (size j = 0; j < foo.n_columns; ++j) {
        Object the_component = LISP_CAR(lisp, traversal_components);
        size column;
        switch (OBJ_TYPE(the_component)) {
        case OBJ_RELATION_TAG:
        case OBJ_ENTITY_TAG:
          /* Single Component: bind its column */
          column =
              ecs_archetype_component_column(world, archetype, the_component);
          break;
        case OBJ_PAIR_TAG: {
          Object car = LISP_CAR(lisp, the_component);
          Object options = LISP_CDR(lisp, the_component);
          /* or and opt forms: bind the column of the first matching Component, if any. */
          if (EQ(car, lisp->keysyms.or)) {
            for (column = NOT_PRESENT;
                 column == NOT_PRESENT && OBJ_TYPE(options) == OBJ_PAIR_TAG;
                 options = LISP_CDR(lisp, options)) {
              column = ecs_archetype_component_column(world, archetype,
                                                      LISP_CAR(lisp, options));
            }
          } else if (EQ(car, lisp->keysyms.opt)) {
            if (OBJ_TYPE(options) != OBJ_PAIR_TAG) {
              WRONG(
                  "An opt Component expression must have exactly one argument",
                  the_component);
              column = NOT_PRESENT;
            } else {
              column = ecs_archetype_component_column(world, archetype,
                                                      LISP_CAR(lisp, options));
            }
          } else {
            column = NOT_PRESENT;
            WRONG("Invalid Component expression in compiled Query",
                  the_component);
          }
          break;
        }
        default:
          column = NOT_PRESENT;
          break;
        }
        foo.columns[j] = column;
        traversal_components = LISP_CDR(lisp, traversal_components);
      }
      func(lisp, &foo, data);
    }
  }

  free(foo.columns);
}

/* Run a Cached Query on the Archetypes it stored. */
void ecs_do_cached_query(LispEnv *lisp, CachedQueryID query_id,
                         SystemFunc *func, void *data) {
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

/* A System Function that adds the matched Archetype to a cache passed via data. */
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

EntityID *ecs_iter_ids(struct EcsIter *iter) {
  return &kv_A(iter->archetype->entities, 0);
}

size ecs_iter_columns(struct EcsIter *iter) {
  return iter->n_columns;
}

size ecs_iter_count(struct EcsIter *iter) {
  return kv_size(iter->archetype->entities);
}

bool ecs_iter_same_archetype(struct EcsIter *a, struct EcsIter *b) {
  return a->archetype == b->archetype;
}

size ecs_iter_pairwise_inner_start(struct EcsIter *outer, struct EcsIter *inner,
                                   size outer_iter) {
  if (ecs_iter_same_archetype(outer, inner)) {
    return outer_iter + 1;
  } else {
    return 0;
  }
}

void *ecs_iter_get(struct EcsIter *iter, size index) {
  if (index == NOT_PRESENT) {
    return NULL;
  }
  size column = iter->columns[index];
  if (column == NOT_PRESENT) {
    return NULL;
  }
  return kv_A(iter->archetype->columns, column).elements;
}

Object ecs_iter_component(struct EcsIter *iter, size index) {
  if (index < 0 || index >= iter->n_columns) {
    return NIL;
  } else if (iter->columns[index] == NOT_PRESENT) {
    return NIL;
  } else {
    for (size i = 0; i < kv_size(iter->archetype->component_columns); ++i) {
      if (kv_A(iter->archetype->component_columns, i) == iter->columns[index]) {
        return kv_A(iter->archetype->type, i);
      }
    }
    return NIL;
  }
}

bool ecs_iter_has(LispEnv *lisp, struct EcsIter *iter, Object component) {
  return ecs_archetype_has(lisp->world, iter->archetype->id, component);
}

Object ecs_new_system(LispEnv *lisp, Object query, SystemFunc *func,
                      void *data) {
  struct World *world = lisp->world;
  Object system = ecs_new(world);
  WorldComponents *world_components = ecs_world_components(world);
  ecs_add(world, system, world_components->system);
  *(SystemFunc **)ecs_get(world, system, world_components->system) = func;
  ecs_add(world, system, world_components->system_data);
  *(void **)ecs_get(world, system, world_components->system_data) = data;
  ecs_add(world, system, world_components->query);
  *(Object *)ecs_get(world, system, world_components->query) = query;
  return system;
}

Object ecs_new_self_join_system(LispEnv *lisp, Object query,
                                NWiseSystem nwise_system, void *data) {
  struct World *world = lisp->world;
  Object entity = ecs_new(world);
  WorldComponents *world_components = ecs_world_components(world);
  ecs_add(world, entity, world_components->nwise_system);
  *(NWiseSystem *)ecs_get(world, entity, world_components->nwise_system) =
      nwise_system;
  ecs_add(world, entity, world_components->system_data);
  *(void **)ecs_get(world, entity, world_components->system_data) = data;
  ecs_add(world, entity, world_components->query);
  *(Object *)ecs_get(world, entity, world_components->query) = query;
  ecs_add(world, entity, world_components->self_join_system);
  return entity;
}
