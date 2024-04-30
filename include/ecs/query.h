#ifndef QUERY_H
#define QUERY_H

#include <ecs/ecs.h>
#include <klib/kvec.h>
#include <lisp/lisp.h>
#include <lisp/object.h>

struct EcsIter;

/* Get the Component data vector at the given index in the iterator. */
void *ecs_iter_get(struct EcsIter *iter, size index);
/* Test whether the given Component is bound in the iterator. */
bool ecs_iter_has(LispEnv *lisp, struct EcsIter *iter, Object component);
/* Gets the Component bound at the given index by the iterator */
Object ecs_iter_component(struct EcsIter *iter, size index);

/* Iterator accessors.
 * In retrospect, it might have made sense to make the definition of iter
 * public. */

/* IDs of Entities in the Archetype iter is bound to. */
EntityID *ecs_iter_ids(struct EcsIter *iter);
/* Number of Entities in iter. */
size ecs_iter_count(struct EcsIter *iter);
/* Number of columns in iter. */
size ecs_iter_columns(struct EcsIter *iter);
/* Test if a and b are iterators over the same Archetype */
bool ecs_iter_same_archetype(struct EcsIter *a, struct EcsIter *b);

/* Determine which at which index into the inner iterator to start,
 * based on the state of the outer iterator in a pairwise Query. */
size ecs_iter_pairwise_inner_start(struct EcsIter *outer, struct EcsIter *inner,
                                   size outer_iter);

typedef void(SystemFunc)(LispEnv *lisp, struct EcsIter *iter, void *data);
/* An N-wise System */
typedef void(NWiseSystemFunc)(LispEnv *lisp, struct EcsIter **iter, void *data);
enum NWiseBehaviour {
  /* Iterate over all pairs of matching Archetypes, i.e. including (a, b) and (b,
     a). */
  NWISE_ALL,
  /* Run on each pair of matching Archetypes only once, i.e. (a, b) but not (b,
     a). */
  NWISE_DISTINCT,
};

typedef struct NWiseSystem {
  NWiseSystemFunc *func;
  enum NWiseBehaviour behaviour;
} NWiseSystem;
/* Run func on every Archetype that satisfies query. */
void ecs_do_query(LispEnv *lisp, Object query, SystemFunc *func, void *data);
void ecs_do_pairwise_query(LispEnv *lisp, Object query0, Object query1,
                           NWiseSystem system, void *data);

/* Build a CachedQuery based on the normal Query 'query' */
CachedQueryID ecs_query(LispEnv *lisp, Object query);

void ecs_do_cached_query(LispEnv *lisp, CachedQueryID query, SystemFunc *func,
                         void *data);

/* Construct a new System Entity, with the given Query, System Function, and
   SystemData Components. */
Object ecs_new_system(LispEnv *lisp, Object query, SystemFunc *func,
                      void *data);
/* Construct a new self-join System.
 * This is a special case of n-wise Systems that runs for each distinct pair of Archetypes,
 * and ensures that each distinct pair of Entities is operated on exactly once,
 * even for Entities in the same Archetype. */
Object ecs_new_self_join_system(LispEnv *lisp, Object query,
                                NWiseSystem nwise_system, void *data);
#endif
