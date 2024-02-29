#include "lisp/memory.h"
#include "lisp/object.h"
#include <common.h>
#include <ecs/ecs.h>
#include <ecs/query.h>
#include <lisp/lexer.h>
#include <lisp/lisp.h>
#include <lisp/print.h>
#include <lisp/reader.h>
#include <lisp/systems.h>
#include <math.h>
#include <raylib.h>
#include <raymath.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <threads.h>

#define FPS (60)
#define FRAMELEN (1.0 / FPS)
#define SCREEN_HEIGHT (720)
#define SCREEN_WIDTH (1280)

typedef struct Vec4i {
  i32 x;
  i32 y;
  i32 z;
  i32 w;
} Vec4i;

static inline Color vec4i_to_colour(Vec4i colour) {
  return (Color){.r = colour.x, .g = colour.y, .b = colour.z, .a = colour.w};
}

int repl_thread_function(void *_lisp) {
  LispEnv *lisp = (LispEnv *)_lisp;
  if (setjmp(lisp->error_loc) != 0) {
    fprintf(stderr, "Resuming from top level...\n");
  }
  Object print_function = LISP_EVAL_STR(lisp, "(function prin1)");
  Object print_args = lisp_cons(lisp, NIL, NIL);
  Object *form_place = LISP_CAR_PLACE(lisp, print_args);
  while (!feof(stdin)) {
    fputs("* ", stdout);
    Object sexp =
        lisp_eval(lisp, lisp_macroexpand(lisp, lisp_read(lisp, stdin)));
    *form_place = sexp;
    lisp_apply(lisp, print_function, print_args);
    fputc('\n', stdout);
  }
  return thrd_success;
}

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

#define GRAVITY (200.0)

void mouse_gravity(LispEnv *lisp, struct EcsIter *iter, void *data) {
  IGNORE(data);

  if (IsMouseButtonDown(MOUSE_LEFT_BUTTON)) {
    Vector2 mouse_pos = (Vector2){GetMouseX(), GetMouseY()};
    size N = ecs_iter_count(iter);
    struct Vector2 *poss = ecs_iter_get(iter, 0);
    struct Vector2 *vels = ecs_iter_get(iter, 1);

    for (size i = 0; i < N; ++i) {
      Vector2 to_centre = Vector2Subtract(mouse_pos, poss[i]);
      /* It would be more realistic to normalise, but this produces a more
       * visible effect */
      /* Vector2 dir_to_centre = Vector2Normalize(to_centre); */
      float dist_to_centre2 = Vector2LengthSqr(to_centre);
      /* Prevent badness at the centre. TODO: Bounce off the mouse or something.
       */
      if (dist_to_centre2 <= 25.0)
        continue;
      vels[i].x += to_centre.x * GRAVITY / dist_to_centre2;
      vels[i].y += to_centre.y * GRAVITY / dist_to_centre2;
    }
  }
}

void do_bounce(LispEnv *lisp, struct EcsIter *iter, void *data) {
  size N = ecs_iter_count(iter);

  struct Vector2 *poss = ecs_iter_get(iter, 0);
  struct Vector2 *vels = ecs_iter_get(iter, 1);
  const float *bounce_damp = ecs_iter_get(iter, 2);

  for (size i = 0; i < N; ++i) {
    if (poss[i].y < 0) {
      poss[i].y = 0;
      vels[i].y *= -bounce_damp[i];
    } else if (poss[i].y > SCREEN_HEIGHT) {
      poss[i].y = SCREEN_HEIGHT;
      vels[i].y *= -bounce_damp[i];
    }
    if (poss[i].x < 0) {
      poss[i].x = 0;
      vels[i].x *= -bounce_damp[i];
    } else if (poss[i].x > SCREEN_WIDTH) {
      poss[i].x = SCREEN_WIDTH;
      vels[i].x *= -bounce_damp[i];
    }
  }
}

void collision_system(LispEnv *lisp, struct EcsIter **iter, void *data) {
  /* printf("Running collisions for archetypes %u and %u\n", archetype0.val,
   * archetype1.val); */
  float delta = GetFrameTime();

  size N = ecs_iter_count(iter[0]);
  struct Vector2 *possn = ecs_iter_get(iter[0], 0);
  struct Vector2 *velsn = ecs_iter_get(iter[0], 1);
  float *radiin = ecs_iter_get(iter[0], 2);
  float *bouncen = ecs_iter_get(iter[0], 3);

  size M = ecs_iter_count(iter[1]);
  struct Vector2 *possm = ecs_iter_get(iter[1], 0);
  struct Vector2 *velsm = ecs_iter_get(iter[1], 1);
  float *radiim = ecs_iter_get(iter[1], 2);
  float *bouncem = ecs_iter_get(iter[1], 3);

  for (size i = 0; i < N; ++i) {
    /* Skip indistinct pairs of Entities in the same archetype */
    for (size j = ecs_iter_same_archetype(iter[0], iter[1]) ? i + 1 : 0; j < M;
         ++j) {
      Vector2 diff = Vector2Subtract(possm[j], possn[i]);
      float dist2 = Vector2LengthSqr(diff);
      float threshold = powf(radiin[i], 2) + powf(radiim[j], 2);
      if (dist2 <= threshold) {
        /* printf("collision (%f, %f): %u in %u & %u in %u\n", diff.x, diff.y,
         */
        /*        idsn[i].val, archetype0.val, idsm[j].val, archetype1.val); */
        possn[i] = Vector2Subtract(possn[i], Vector2Scale(velsn[i], 2 * delta));
        possm[j] = Vector2Subtract(possm[j], Vector2Scale(velsm[j], 2 * delta));

        Vector2 dir = Vector2Normalize(diff);
        velsm[j] = Vector2Scale(Vector2Reflect(velsm[j], dir), bouncem[j]);
        velsn[i] = Vector2Scale(Vector2Reflect(velsn[i], dir), bouncen[i]);
      }
    }
  }
}

void draw_movers(LispEnv *lisp, struct EcsIter *iter, void *data) {
  struct Vector2 *poss = ecs_iter_get(iter, 0);
  struct Vec4i *colours = ecs_iter_get(iter, 1);
  float *radii = ecs_iter_get(iter, 2);
  size N = ecs_iter_count(iter);
  for (size i = 0; i < N; ++i) {
    Color colour = colours != NULL ? vec4i_to_colour(colours[i]) : WHITE;
    DrawCircle(poss[i].x, poss[i].y, radii[i], colour);
  }
}

void print_mover(LispEnv *lisp, struct EcsIter *iter, void *data) {
  printf("Printing some movers...\n");
  IGNORE(data);
  EntityID *ids = ecs_iter_ids(iter);
  struct Vector2 *poss = ecs_iter_get(iter, 0);
  struct Vector2 *vels = ecs_iter_get(iter, 1);
  size N = ecs_iter_count(iter);
  for (size i = 0; i < N; ++i) {
    printf("Entity %u: pos: (%f, %f), vel: (%f, %f)\n", ids[i].val, poss[i].x,
           poss[i].y, vels[i].x, vels[i].y);
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
  WorldComponents *world_components = ecs_world_components(lisp->world);
  Object storage_comp = world_components->storage;

  lisp_apply(lisp, lisp_lookup_function(lisp, SYM(lisp, "print")),
             lisp_cons(lisp, storage_comp, NIL));
  Object pos = ecs_lookup_by_name(world, SYM(lisp, "Pos"));
  {
    struct Storage pos_storage =
        *(struct Storage *)ecs_get(world, pos, storage_comp);
    printf("pos_storage: .size = %lu, .alignment = %lu\n", pos_storage.size,
           pos_storage.alignment);
  }

  Object physics_component = ecs_lookup_by_name(world, SYM(lisp, "Physics"));
  Object graphics_component = ecs_lookup_by_name(world, SYM(lisp, "Graphics"));

  ecs_add(world,
          ecs_new_system(lisp, LISP_EVAL_STR(lisp, "(select Pos Vel Colour)"),
                         mouse_gravity, NULL),
          physics_component);

  ecs_add(world,
          ecs_new_system(lisp,
                         LISP_EVAL_STR(lisp, "(select Pos Colour Radius)"),
                         draw_movers, NULL),
          graphics_component);
  ecs_add(world,
          ecs_new_system(lisp, LISP_EVAL_STR(lisp, "(select Pos Vel Bounce)"),
                         do_bounce, NULL),
          physics_component);
  ecs_add(world,
          ecs_new_self_join_system(
              lisp, LISP_EVAL_STR(lisp, "(select Pos Vel Radius Bounce)"),
              (NWiseSystem){collision_system, NWISE_DISTINCT}, NULL),
          physics_component);

  Object nwise_physics_query = LISP_EVAL_STR(
      lisp,
      "(select NWiseSystem Query SystemData (with SelfJoin) (with Physics))");
  Object physics_query =
      LISP_EVAL_STR(lisp, "(select System Query SystemData (with Physics))");
  Object graphics_query =
      LISP_EVAL_STR(lisp, "(select System Query SystemData (with Graphics))");

  /* Main game loop */
  thrd_t repl_thread;
  InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, "ECSQL Demo");
  thrd_create(&repl_thread, repl_thread_function, lisp);
  SetTargetFPS(FPS);
  while (!WindowShouldClose()) {

    ecs_do_query(lisp, nwise_physics_query, run_matching_self_join_systems,
                 NULL);
    ecs_do_query(lisp, physics_query, run_matching_systems, NULL);
    BeginDrawing();
    ClearBackground(BLACK);
    ecs_do_query(lisp, graphics_query, run_matching_systems, NULL);
    DrawFPS(1150, 10);
    EndDrawing();
  }
  CloseWindow();
  thrd_join(repl_thread, NULL);
  printf("goodbye\n");

  return 0;
}
