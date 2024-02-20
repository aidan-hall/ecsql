#ifndef QUERY_H
#define QUERY_H

#include <ecs/ecs.h>
#include <klib/kvec.h>
#include <lisp/lisp.h>
#include <lisp/object.h>

typedef struct EcsIter {
  ArchetypeID archetype;
  size n_columns;
  /* Column in 'archetype' where each relevant Component can be found. */
  size *columns;
} EcsIter;

static inline void *ecs_iter_get(LispEnv *lisp, struct EcsIter *iter,
                                 size index) {
  return ecs_archetype_get(lisp->world, iter->archetype, index);
}

EntityID *ecs_iter_ids(LispEnv *lisp, struct EcsIter *iter);

typedef void(SystemFunc)(LispEnv *lisp, struct EcsIter *iter, void *data);
void ecs_do_query(LispEnv *lisp, Object query, SystemFunc *func, void *data);
typedef struct CachedQuery {
  kvec_t(struct EcsIter) matches;
} CachedQuery;

CachedQuery ecs_query(LispEnv *lisp, Object query);
void ecs_destroy_cached_query(CachedQuery query);

void ecs_do_cached_query(LispEnv *lisp, CachedQuery *query, SystemFunc *func,
                         void *data);
#endif
