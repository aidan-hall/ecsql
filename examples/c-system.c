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

// ...

Object physics_component =
  ecs_lookup_by_name(lisp->world, SYM(lisp, "Physics"));

ecs_add(lisp->world,
        ecs_new_system(lisp,
                       LISP_EVAL_STR(lisp,
                                     "(select Pos Vel)"),
                       move, NULL),
        physics_component);

