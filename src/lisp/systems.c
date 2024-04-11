#include "ecs/ecs.h"
#include "ecs/query.h"
#include "lisp/lisp.h"
#include "lisp/memory.h"
#include "lisp/object.h"
#include <lisp/systems.h>

void lisp_run_system(LispEnv *lisp, struct EcsIter *iter, void *data) {
  Object storage = ecs_world_components(lisp->world)->storage;
  Object lisp_storage = lisp->comp.lisp_component_storage;

  Object system = BIT_CAST(Object, data);

  size bindings_length = ecs_iter_columns(iter);

  size N = ecs_iter_count(iter);
  EntityID *ids = ecs_iter_ids(iter);

  if (bindings_length == 0) {
    /* Just pass the Entity. */
    Object args_form = lisp_cons(lisp, NIL, NIL);
    for (size i = 0; i < N; ++i) {
      *LISP_CAR_PLACE(lisp, args_form) =
          ecs_object_with_id(lisp->world, ids[i]);
      lisp_apply(lisp, system, args_form);
    }
    return;
  }

  /* The cells allocated for the Component values. */
  size arg_cells = lisp_allocate_cells(lisp, bindings_length);

  if (arg_cells == -1) {
    WRONG("Failed to allocate memory for Lisp System Component bindings.");
    return;
  }

  /* Set up the arguments list, and get each argument type's information. */
  Object *argument_places[bindings_length];
  u8 *columns[bindings_length];
  Object components[bindings_length];
  struct LispComponentStorage lisp_storages[bindings_length];
  struct Storage storages[bindings_length];

  Object args_form = NIL;
  for (size i = bindings_length - 1; i >= 0; --i) {
    /* Iterate backwards because we are building the argument list backwards. */
     
    args_form = lisp_cons(lisp, NIL, args_form);
    if ((columns[i] = ecs_iter_get(iter, i)) != NULL) {
      argument_places[i] = LISP_CAR_PLACE(lisp, args_form);
      components[i] = ecs_iter_component(iter, i);
      if (!EQ(components[i], NIL)) {
        void *tmp = ecs_get(lisp->world, components[i], lisp_storage);
        if (tmp == NULL) {
          WRONG("Bound Component has no LispComponentStorage", components[i]);
          return;
        }
        lisp_storages[i] = *(struct LispComponentStorage *)tmp;
        tmp = ecs_get(lisp->world, components[i], storage);
        if (tmp == NULL) {
          WRONG("Bound Component has no Storage", components[i]);
          return;
        }
        storages[i] = *(struct Storage *)tmp;
        if (lisp_storages[i].type == STORE_STRUCT) {
          *argument_places[i] =
              OBJ_BOX_INDEX(lisp_store_pointer(lisp, NULL),
                            lisp_storages[i].struct_id, STRUCT);
        }
      }
    } else {
      components[i] = NIL;
    }
  }
  /* Add one more cons cell for the Entity. */
  args_form = lisp_cons(lisp, NIL, args_form);

  /* Run the System */
  for (size i = 0; i < N; ++i) {
    /* Set the ID */
    *LISP_CAR_PLACE(lisp, args_form) = ecs_object_with_id(lisp->world, ids[i]);

    /* Set the Component arguments */
    for (size j = 0; j < bindings_length; ++j) {
      /* Skip NULL bindings */
      if (EQ(components[j], NIL))
        continue;

      switch (lisp_storages[j].type) {
      case STORE_OBJECT:
        *argument_places[j] = ((Object *)columns[j])[i];
        break;
      case STORE_UNBOXED: {
        u64 val = 0;
        memcpy(&val, &columns[j][i * storages[j].size], storages[j].size);
        *argument_places[j] = OBJ_BOX_RAWTAG(val, lisp_storages[j].object_type);
        break;
      }
      case STORE_STRUCT:
        *(void **)lisp_cell_at(lisp, -argument_places[j]->index) =
            &columns[j][i * storages[j].size];
        break;
      }
    }
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
