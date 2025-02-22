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
#define SCREEN_HEIGHT (480)
#define SCREEN_WIDTH (640)

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

#define GRAVITY (200.0)

void mouse_gravity(LispEnv *lisp, struct EcsIter *iter, void *data) {
  IGNORE(data);

  if (IsMouseButtonDown(MOUSE_MIDDLE_BUTTON)) {
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
      /* Prevent weird behaviour at the centre. */
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

void detect_collisions_and_bounce(LispEnv *lisp, struct EcsIter **iter,
                                  void *data) {
  size N = ecs_iter_count(iter[0]);
  struct Vector2 *possn = ecs_iter_get(iter[0], 0);
  struct Vector2 *velsn = ecs_iter_get(iter[0], 1);
  float *radiin = ecs_iter_get(iter[0], 2);
  float *bouncen = ecs_iter_get(iter[0], 3);
  float *massn = ecs_iter_get(iter[0], 4);

  size M = ecs_iter_count(iter[1]);
  struct Vector2 *possm = ecs_iter_get(iter[1], 0);
  struct Vector2 *velsm = ecs_iter_get(iter[1], 1);
  float *radiim = ecs_iter_get(iter[1], 2);
  float *bouncem = ecs_iter_get(iter[1], 3);
  float *massm = ecs_iter_get(iter[1], 4);

  for (size i = 0; i < N; ++i) {
    /* Skip indistinct pairs of Entities in the same archetype */
    for (size j = ecs_iter_pairwise_inner_start(iter[0], iter[1], i); j < M;
         ++j) {
      Vector2 diff = Vector2Subtract(possm[j], possn[i]);
      if (CheckCollisionCircles(possn[i], radiin[i], possm[j], radiim[j])) {
        Vector2 dir = Vector2Normalize(diff);
        float dist = Vector2Length(diff);
        Vector2 intersection = Vector2Scale(dir, radiim[j] + radiin[i] - dist);
        Vector2 half_collision = Vector2Scale(intersection, 0.5);
        Vector2 relative_velocity = Vector2Subtract(velsn[i], velsm[j]);
        /* printf("collision (%f, %f): %u in %u & %u in %u\n", diff.x, diff.y,
         */
        /*        idsn[i].val, archetype0.val, idsm[j].val, archetype1.val); */
        possn[i] = Vector2Subtract(possn[i], half_collision);
        possm[j] = Vector2Add(possm[j], half_collision);

        /* https://en.wikipedia.org/wiki/Elastic_collision */

        float total_mass = massn[i] + massm[j];

        velsm[j] = Vector2Subtract(
            velsm[j],
            Vector2Scale(dir, -(1 + bouncem[j]) *
                                  Vector2DotProduct(relative_velocity, dir) *
                                  massn[i] / total_mass));

        velsn[i] = Vector2Subtract(
            velsn[i],
            Vector2Scale(dir, (1 + bouncen[i]) *
                                  Vector2DotProduct(relative_velocity, dir) *
                                  massm[j] / total_mass));
      }
    }
  }
}

void draw_movers(LispEnv *lisp, struct EcsIter *iter, void *data) {
  /* (select Pos (opt Colour) Radius) */
  struct Vector2 *poss = ecs_iter_get(iter, 0);
  struct Vec4i *colours = ecs_iter_get(iter, 1);
  float *radii = ecs_iter_get(iter, 2);
  size N = ecs_iter_count(iter);
  if (colours == NULL) {
    for (size i = 0; i < N; ++i) {
      DrawCircle(poss[i].x, poss[i].y, radii[i], WHITE);
    }
  } else {
    for (size i = 0; i < N; ++i) {
      Color colour = vec4i_to_colour(colours[i]);
      DrawCircle(poss[i].x, poss[i].y, radii[i], colour);
    }
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

void move(LispEnv *lisp, struct EcsIter *iter, void *data) {
  IGNORE(data);
  struct Vector2 *poss = ecs_iter_get(iter, 0);
  struct Vector2 *vels = ecs_iter_get(iter, 1);
  size N = ecs_iter_count(iter);
  float delta = GetFrameTime();
  for (size i = 0; i < N; ++i) {
    poss[i].x += vels[i].x * delta;
    poss[i].y += vels[i].y * delta;
  }
}

int main(int argc, char *argv[]) {
  LispEnv lisp_env = new_lisp_environment();
  LispEnv *lisp = &lisp_env;
  if (setjmp(lisp->error_loc) != 0) {
    fprintf(stderr, "Error in a file loaded at startup: no good!\n");
    exit(1);
  }

  struct World *world = lisp->world;

  Object physics_component = ecs_lookup_by_name(world, SYM(lisp, "Physics"));
  Object graphics_component = ecs_lookup_by_name(world, SYM(lisp, "Graphics"));

  Object move_system = ecs_new_system(
      lisp, LISP_EVAL_STR(lisp, "(select (or RelPos Pos) Vel)"), move, NULL);
  ecs_add(world, move_system, physics_component);
  assert(ecs_set_name(world, move_system, SYM(lisp, "Move")));

  Object mouse_gravity_system =
      ecs_new_system(lisp, LISP_EVAL_STR(lisp, "(select Pos Vel (has Mass))"),
                     mouse_gravity, NULL);
  ecs_add(world, mouse_gravity_system, physics_component);
  assert(ecs_set_name(world, mouse_gravity_system, SYM(lisp, "MouseGravity")));

  Object draw_movers_system = ecs_new_system(
      lisp, LISP_EVAL_STR(lisp, "(select Pos (opt Colour) Radius)"),
      draw_movers, NULL);
  ecs_add(world, draw_movers_system, graphics_component);
  assert(ecs_set_name(world, draw_movers_system, SYM(lisp, "DrawMovers")));
  Object bounce_system = ecs_new_system(
      lisp, LISP_EVAL_STR(lisp, "(select Pos Vel Bounce)"), do_bounce, NULL);
  ecs_add(world, bounce_system, physics_component);
  assert(ecs_set_name(world, bounce_system, SYM(lisp, "DoBounce")));
  Object collision_system = ecs_new_self_join_system(
      lisp, LISP_EVAL_STR(lisp, "(select Pos Vel Radius Bounce Mass)"),
      (NWiseSystem){detect_collisions_and_bounce, NWISE_DISTINCT}, NULL);
  assert(ecs_set_name(world, collision_system, SYM(lisp, "DoCollision")));
  ecs_add(world, collision_system, physics_component);

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
    EndDrawing();
  }
  CloseWindow();
  thrd_join(repl_thread, NULL);
  printf("goodbye\n");

  return 0;
}
