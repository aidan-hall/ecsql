#ifndef SYSTEMS_H
#define SYSTEMS_H

#include <ecs/query.h>
#include <lisp/lisp.h>

/* Run the Lisp System supplied in 'data' for every Entity in iter. */
void lisp_run_system(LispEnv *lisp, struct EcsIter *iter, void *data);
/* Run each System in iter once. */
void run_matching_systems(LispEnv *lisp, struct EcsIter *iter, void *data);
/* Run each NWiseSystem in iter once, passing its Query Component as both Query
   arguments to ecs_do_pairwise_query. */
void run_matching_self_join_systems(LispEnv *lisp, struct EcsIter *iter,
                                    void *data);

#endif
