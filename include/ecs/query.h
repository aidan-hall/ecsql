#ifndef QUERY_H
#define QUERY_H

#include <lisp/object.h>
#include <lisp/lisp.h>
#include <ecs/ecs.h>
#include <klib/kvec.h>

typedef struct EcsIter {
  ArchetypeID archetype;
  /* Number of Entities in the matching archetype */
  size count;
  size n_columns;
  /* Column in 'archetype' where each relevant Component can be found. */
  size* columns;
} EcsIter;

static inline void *ecs_iter_get(LispEnv *lisp, struct EcsIter *iter, size index) {
  return ecs_archetype_get(lisp->world, iter->archetype, index);
}


typedef void (SystemFunc)(LispEnv *lisp, struct EcsIter *iter, void *data);
void ecs_run(LispEnv *lisp, Object query, SystemFunc *func, void *data);

typedef struct CachedQuery {
  kvec_t(struct EcsIter) matches;
} CachedQuery;

CachedQuery ecs_query(LispEnv *lisp, Object query);
#endif
