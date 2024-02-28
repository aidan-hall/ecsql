#ifndef SYSTEMS_H
#define SYSTEMS_H

#include <lisp/lisp.h>
#include <ecs/query.h>

void lisp_run_system(LispEnv *lisp, struct EcsIter *iter, void *data);
void run_matching_systems(LispEnv *lisp, struct EcsIter *iter, void *data);
void run_matching_self_join_systems(LispEnv *lisp, struct EcsIter *iter, void *data);


#endif
