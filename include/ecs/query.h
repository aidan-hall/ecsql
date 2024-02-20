#ifndef QUERY_H
#define QUERY_H

#include <ecs/ecs.h>
#include <klib/kvec.h>
#include <lisp/lisp.h>
#include <lisp/object.h>

struct EcsIter;
struct CachedQuery;

void *ecs_iter_get(LispEnv *lisp, struct EcsIter *iter, size index);

EntityID *ecs_iter_ids(LispEnv *lisp, struct EcsIter *iter);
size ecs_iter_count(struct EcsIter *iter);

typedef void(SystemFunc)(LispEnv *lisp, struct EcsIter *iter, void *data);
void ecs_do_query(LispEnv *lisp, Object query, SystemFunc *func, void *data);

struct CachedQuery *ecs_query(LispEnv *lisp, Object query);
void ecs_destroy_cached_query(struct CachedQuery *query);

void ecs_do_cached_query(LispEnv *lisp, struct CachedQuery *query,
                         SystemFunc *func, void *data);
#endif
