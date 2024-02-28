#include <lisp/systems.h>


void lisp_run_system(LispEnv *lisp, struct EcsIter *iter, void *data) {
  Object system = *(Object *)data;
  Object args_form = lisp_cons(lisp, NIL, NIL);
  EntityID *ids = ecs_iter_ids(iter);
  size N = ecs_iter_count(iter);
  for (size i = 0; i < N; ++i) {
    *LISP_CAR_PLACE(lisp, args_form) = ecs_object_with_id(lisp->world, ids[i]);
    lisp_apply(lisp, system, args_form);
  }
}

void run_matching_systems(LispEnv *lisp, struct EcsIter *iter, void *data) {
  IGNORE(data);
  size N = ecs_iter_count(iter);
  SystemFunc **functions = ecs_iter_get(iter, 0);
  Object *queries = ecs_iter_get(iter, 1);
  void **datas = ecs_iter_get(iter, 2);
  for (size i = 0; i < N; ++i) {
    ecs_do_query(lisp, queries[i], functions[i], datas[i]);
  }
}

void run_matching_self_join_systems(LispEnv *lisp, struct EcsIter *iter,
                                    void *data) {
  IGNORE(data);
  size N = ecs_iter_count(iter);
  NWiseSystem *functions = ecs_iter_get(iter, 0);
  Object *queries = ecs_iter_get(iter, 1);
  void **datas = ecs_iter_get(iter, 2);
  for (size i = 0; i < N; ++i) {
    ecs_do_pairwise_query(lisp, queries[i], queries[i], functions[i], datas[i]);
  }
}
