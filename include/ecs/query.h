#ifndef QUERY_H
#define QUERY_H

#include <ecs/ecs.h>
#include <klib/kvec.h>
#include <lisp/lisp.h>
#include <lisp/object.h>

struct EcsIter;

void *ecs_iter_get(LispEnv *lisp, struct EcsIter *iter, size index);

EntityID *ecs_iter_ids(struct EcsIter *iter);
size ecs_iter_count(struct EcsIter *iter);

typedef void(SystemFunc)(LispEnv *lisp, struct EcsIter *iter, void *data);
void ecs_do_query(LispEnv *lisp, Object query, SystemFunc *func, void *data);

CachedQueryID ecs_query(LispEnv *lisp, Object query);

void ecs_do_cached_query(LispEnv *lisp, CachedQueryID query, SystemFunc *func,
                         void *data);
#endif
