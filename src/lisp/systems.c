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
