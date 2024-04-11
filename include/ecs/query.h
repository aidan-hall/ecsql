#ifndef QUERY_H
#define QUERY_H

#include <ecs/ecs.h>
#include <klib/kvec.h>
#include <lisp/lisp.h>
#include <lisp/object.h>

struct EcsIter;

void *ecs_iter_get(struct EcsIter *iter, size index);
bool ecs_iter_has(LispEnv *lisp, struct EcsIter *iter, Object component);
Object ecs_iter_component(struct EcsIter *iter, size index);

EntityID *ecs_iter_ids(struct EcsIter *iter);
size ecs_iter_count(struct EcsIter *iter);
size ecs_iter_columns(struct EcsIter *iter);
bool ecs_iter_same_archetype(struct EcsIter *a, struct EcsIter *b);
size ecs_iter_pairwise_inner_start(struct EcsIter *outer, struct EcsIter *inner, size outer_iter);

typedef void(SystemFunc)(LispEnv *lisp, struct EcsIter *iter, void *data);
typedef void(NWiseSystemFunc)(LispEnv *lisp, struct EcsIter **iter, void *data);
enum NWiseBehaviour {
  NWISE_ALL,
  NWISE_DISTINCT,
};

typedef struct NWiseSystem {
  NWiseSystemFunc *func;
  enum NWiseBehaviour behaviour;
} NWiseSystem;
void ecs_do_query(LispEnv *lisp, Object query, SystemFunc *func, void *data);
void ecs_do_pairwise_query(LispEnv *lisp, Object query0, Object query1,
                           NWiseSystem system, void *data);

CachedQueryID ecs_query(LispEnv *lisp, Object query);

void ecs_do_cached_query(LispEnv *lisp, CachedQueryID query, SystemFunc *func,
                         void *data);

Object ecs_new_system(LispEnv *lisp, Object query, SystemFunc *func,
                      void *data);
Object ecs_new_self_join_system(LispEnv *lisp, Object query,
                                NWiseSystem nwise_system, void *data);
#endif
