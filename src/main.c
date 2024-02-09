#include <common.h>
#include <ecs/ecs.h>
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
  Object foo = ENT_BOX(3, 0);
  printf("foo: %lx\n", foo.bits);
  kv_push(Object, a, foo);
  kv_push(Object, a, (ENT_BOX(49, 0)));
  kv_push(Object, a, (ENT_BOX(67, 0)));
  kv_push(Object, a, (ENT_BOX(93, 0)));
  kv_push(Object, a, (ENT_BOX(105, 0)));
  kv_push(Object, a, (ENT_BOX(241, 0)));
  kv_push(Object, a, (ENT_BOX(999, 0)));
  kv_push(Object, a, (ENT_BOX(7680, 0)));
  fprintf(stderr, "Elements: ");
  for (size i = 0; i < kv_size(a); ++i) {
    fprintf(stderr, "%lx, ", kv_A(a, i).bits);
  }
  fputc('\n', stderr);

  printf("t: %ld\n", type_pos(a, ENT_BOX(93, 0)));
  printf("f: %ld\n", type_pos(a, ENT_BOX(0, 0)));
  printf("t: %ld\n", type_pos(a, ENT_BOX(3, 0)));
  printf("f: %ld\n", type_pos(a, ENT_BOX(10000, 0)));
  printf("t: %ld\n", type_pos(a, ENT_BOX(999, 0)));
  printf("t: %ld\n", type_pos(a, ENT_BOX(7680, 0)));
}

int main(int argc, char *argv[]) {
  LispEnv lisp = new_lisp_environment();
  if (setjmp(lisp.error_loc) != 0) {
    fprintf(stderr, "Error in a file loaded at startup: no good!\n");
    exit(1);
  }
  Object l = lisp_list(&lisp, lisp.keysyms.quote, lisp.keysyms.t, OBJ_NIL_TAG);
  lisp_print(&lisp, l, stdout);
  fputc('\n', stdout);


  test_type_bsearch();
  struct World *world = init_world();
  Object pos = ECS_NEW_COMPONENT(world, struct Vec3);
  Object vel = ECS_NEW_COMPONENT(world, struct Vec3);
  Object fooable = ecs_new(world);
  Object player = ecs_new(world);
  ecs_add(world, player, pos);
  *(struct Vec3 *)ecs_get(world, player, pos) = (struct Vec3){1, 2, 3};
  ecs_add(world, player, vel);
  *(struct Vec3 *)ecs_get(world, player, vel) = (struct Vec3){0, 0, 2};
  ecs_remove(world, player, pos);
  ecs_add(world, player, fooable);
  ecs_remove(world, player, vel);
  ecs_remove(world, player, fooable);

  if (setjmp(lisp.error_loc) != 0) {
    fprintf(stderr, "Resuming from top level...\n");
  }
  lisp_eval(&lisp, OBJS(&lisp, "(repl)"));
  return 0;
}
