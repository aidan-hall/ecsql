#include "lisp/memory.h"
#include "lisp/object.h"
#include <common.h>
#include <ecs/ecs.h>
#include <ecs/query.h>
#include <lisp/lexer.h>
#include <lisp/lisp.h>
#include <lisp/print.h>
#include <lisp/reader.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void test_type_bsearch() {
  Type a;
  kv_init(a);
  Object foo = (Object){.bits = ~0};
  printf("foo: %lx, ENT_SIG(foo): %lx\n", foo.bits, ENT_SIG(foo));
  foo = ENT_BOX((EntityID){3}, 0);
  printf("foo: %lx, ENT_SIG(foo): %lu\n", foo.bits, ENT_SIG(foo));
  kv_push(Object, a, foo);
  kv_push(Object, a, (ENT_BOX((EntityID){49}, 0)));
  kv_push(Object, a, (ENT_BOX((EntityID){67}, 0)));
  kv_push(Object, a, (ENT_BOX((EntityID){93}, 0)));
  kv_push(Object, a, (ENT_BOX((EntityID){105}, 0)));
  kv_push(Object, a, (ENT_BOX((EntityID){241}, 0)));
  kv_push(Object, a, (ENT_BOX((EntityID){999}, 0)));
  kv_push(Object, a, (ENT_BOX((EntityID){7680}, 0)));
  fprintf(stderr, "Elements: ");
  for (size i = 0; i < kv_size(a); ++i) {
    fprintf(stderr, "%lx, ", kv_A(a, i).bits);
  }
  fputc('\n', stderr);

  printf("t: %ld\n", type_pos(a, ENT_BOX((EntityID){93}, 0)));
  printf("f: %ld\n", type_pos(a, ENT_BOX((EntityID){0}, 0)));
  printf("t: %ld\n", type_pos(a, ENT_BOX((EntityID){3}, 0)));
  printf("f: %ld\n", type_pos(a, ENT_BOX((EntityID){10000}, 0)));
  printf("t: %ld\n", type_pos(a, ENT_BOX((EntityID){999}, 0)));
  printf("t: %ld\n", type_pos(a, ENT_BOX((EntityID){7680}, 0)));
}

struct Vec3 {
  float x;
  float y;
  float z;
};

void apply_velocity(LispEnv *lisp, struct EcsIter *iter, void *data) {
  printf("Applying velocity...\n");
  IGNORE(data);
  struct Vec3 *poss = ecs_iter_get(lisp, iter, 0);
  struct Vec3 *vels = ecs_iter_get(lisp, iter, 1);
  size N = ecs_iter_count(iter);
  for (size i = 0; i < N; ++i) {
    poss[i].x += vels[i].x;
    poss[i].y += vels[i].y;
    poss[i].z += vels[i].z;
  }
}

void print_mover(LispEnv *lisp, struct EcsIter *iter, void *data) {
  printf("Printing some movers...\n");
  IGNORE(data);
  EntityID *ids = ecs_iter_ids(lisp, iter);
  struct Vec3 *poss = ecs_iter_get(lisp, iter, 0);
  struct Vec3 *vels = ecs_iter_get(lisp, iter, 1);
  size N = ecs_iter_count(iter);
  for (size i = 0; i < N; ++i) {
    printf("Entity %u: pos: (%f, %f, %f), vel: (%f, %f, %f)\n", ids[i].val,
           poss[i].x, poss[i].y, poss[i].z, vels[i].x, vels[i].y, vels[i].z);
  }
}

int main(int argc, char *argv[]) {
  LispEnv lisp_env = new_lisp_environment();
  LispEnv *lisp = &lisp_env;
  if (setjmp(lisp->error_loc) != 0) {
    fprintf(stderr, "Error in a file loaded at startup: no good!\n");
    exit(1);
  }

  {
    /* Allocate an object outside Lisp memory, and insert a pointer to it. */
    char outside[] = "fruitbar";
    size idx = lisp_allocate_cells(lisp, 1);
    *(char **)lisp_cell_at(lisp, idx) = outside;
    lisp_add_to_namespace(lisp, lisp->globals, SYM(lisp, "fruitptr"),
                          OBJ_BOX_INDEX(-idx, strlen(outside), STRING));
  }

  Object l = lisp_list(lisp, lisp->keysyms.quote, lisp->keysyms.t, OBJ_NIL_TAG);
  lisp_print(lisp, l, stdout);
  fputc('\n', stdout);

  test_type_bsearch();
  struct World *world = lisp->world;
  Object storage_comp = ecs_lookup_by_name(world, SYM(lisp, "Storage"));
  lisp_apply(lisp, lisp_eval(lisp, OBJS(lisp, "(function print)")),
             lisp_cons(lisp, storage_comp, NIL));
  Object pos = ecs_lookup_by_name(world, SYM(lisp, "Pos"));
  {
    struct Storage pos_storage =
        *(struct Storage *)ecs_get(world, pos, storage_comp);
    printf("pos_storage: .size = %lu, .alignment = %lu\n", pos_storage.size,
           pos_storage.alignment);
  }
  Object vel = ecs_lookup_by_name(world, SYM(lisp, "Vel"));
  Object fooable = ecs_new(world);
  Object apple = ecs_new(world);
  Object player = ecs_new(world);
  assert(ecs_set_name(world, player, SYM(lisp, "player")));
  ecs_add(world, player, pos);
  *(struct Vec3 *)ecs_get(world, player, pos) = (struct Vec3){3, 2, 3};
  Object pear = ecs_new(world);
  ecs_add(world, player, vel);
  *(struct Vec3 *)ecs_get(world, player, vel) = (struct Vec3){0, 0, 2};
  ecs_add(world, player, fooable);
  Object orange = ecs_new(world);

  Object eats = lisp_new_ecs_component(lisp, SYM(lisp, "i32"));
  assert(ecs_set_name(world, eats, SYM(lisp, "Eats")));
  ecs_add(world, player, ecs_pair(eats, apple));
  *(i32 *)ecs_get(world, player, ecs_pair(eats, apple)) = -4;
  assert(ecs_set_name(world, apple, SYM(lisp, "Apple")));
  ecs_add(world, player, ecs_pair(eats, orange));
  ecs_add(world, player, ecs_pair(eats, pear));
  Type type = ecs_type(world, player);
  printf("eats: %lx, eats.id: %d\n", eats.bits, eats.id.val);
  for (size i = 0; i < kv_size(type); ++i) {
    printf("type[%ld] = %lx, .relation = %d\n", i, kv_A(type, i).bits,
           kv_A(type, i).relation);
  }

  {
    for (int i = 0; i < 10; ++i) {
      Object e = ecs_new(world);
      ecs_add(world, e, pos);
      ecs_add(world, e, vel);
      *(struct Vec3 *)ecs_get(world, e, pos) = (struct Vec3){i, -i, 2 * i};
      *(struct Vec3 *)ecs_get(world, e, vel) = (struct Vec3){3 * i, 0, 69.0};
    }
    Object mover_query =
        lisp_eval(lisp, lisp_macroexpand(lisp, OBJS(lisp, "(select Pos Vel)")));
    struct CachedQuery *cached_mover_query = ecs_query(lisp, mover_query);
    ecs_do_cached_query(lisp, cached_mover_query, print_mover, NULL);
    ecs_do_cached_query(lisp, cached_mover_query, apply_velocity, NULL);
    ecs_do_cached_query(lisp, cached_mover_query, print_mover, NULL);
  }

  if (setjmp(lisp->error_loc) != 0) {
    fprintf(stderr, "Resuming from top level...\n");
  }
  lisp_eval(lisp, OBJS(lisp, "(repl)"));

  return 0;
}
