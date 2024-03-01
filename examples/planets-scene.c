/* This is the equivalent C code for planets-scene.lisp */
void planets_scene() {
  Object vel = ecs_lookup_by_name(world, SYM(lisp, "Vel"));
  Object bounce = ecs_lookup_by_name(world, SYM(lisp, "Bounce"));
  Object radius_component = ecs_lookup_by_name(world, SYM(lisp, "Radius"));
  Object fooable = ecs_new(world);
  Object apple = ecs_new(world);
  Object player = ecs_new(world);
  assert(ecs_set_name(world, player, SYM(lisp, "player")));
  ecs_add(world, player, bounce);
  *(f32 *)ecs_get(world, player, bounce) = 0.99;
  ecs_add(world, player, radius_component);
  *(f32 *)ecs_get(world, player, radius_component) = 10.0;
  ecs_add(world, player, pos);
  *(struct Vec2 *)ecs_get(world, player, pos) = (struct Vec2){300, 200};
  Object pear = ecs_new(world);
  ecs_add(world, player, vel);
  *(struct Vec2 *)ecs_get(world, player, vel) = (struct Vec2){0, 0};
  ecs_add(world, player, fooable);
  Object orange = ecs_new(world);

  Object colour = ecs_lookup_by_name(world, SYM(lisp, "Colour"));

  ecs_add(world, player, colour);
  *(struct Vec4i *)ecs_get(world, player, colour) = (struct Vec4i){
      .x = YELLOW.r, .y = YELLOW.g, .z = YELLOW.b, .w = YELLOW.a};
  Object species = ecs_new(world);
  assert(ecs_set_name(world, species, SYM(lisp, "Species")));
  {
    Object yellow_tag = ecs_new(world);
    assert(ecs_set_name(world, yellow_tag, SYM(lisp, "Human")));
    ecs_add(world, player, ecs_pair(species, yellow_tag));
  }

  Color species_colours[] = {RED, BLUE, GREEN};
  f32 species_sizes[] = {15.0, 10.0, 5.0};
  Object population_species[] = {ecs_new(world), ecs_new(world),
                                 ecs_new(world)};
  assert(ecs_set_name(world, population_species[0], SYM(lisp, "Dwarf")));
  assert(ecs_set_name(world, population_species[1], SYM(lisp, "Elf")));
  assert(ecs_set_name(world, population_species[2], SYM(lisp, "Goblin")));
  for (int i = 0; i < 20; ++i) {
    Object e = ecs_new(world);
    ecs_add(world, e, pos);
    ecs_add(world, e, vel);
    ecs_add(world, e, bounce);
    ecs_add(world, e, radius_component);
    ecs_add(world, e, colour);
    *(struct Vec2 *)ecs_get(world, e, pos) = (struct Vec2){50 * i, 20 * i};
    *(struct Vec2 *)ecs_get(world, e, vel) =
        (struct Vec2){20.0 * (1 + i), 15.0 * (1 + i)};
    *(f32 *)ecs_get(world, e, bounce) = 0.8;
    *(f32 *)ecs_get(world, e, radius_component) = species_sizes[i % 3];

    Color picked = species_colours[i % countof(species_colours)];
    *(struct Vec4i *)ecs_get(world, e, colour) = (struct Vec4i){
        .x = picked.r, .y = picked.g, .z = picked.b, .w = picked.a};
    ecs_add(
        world, e,
        ecs_pair(species, population_species[i % countof(population_species)]));
  }
  {
    Object wizard = ecs_new(world);
    assert(ecs_set_name(world, wizard, SYM(lisp, "Wizard")));
    Object the_wizard = ecs_new(world);
    ecs_add(world, the_wizard, ecs_pair(species, wizard));
    ecs_add(world, the_wizard, pos);
    ecs_add(world, the_wizard, vel);
    ecs_add(world, the_wizard, bounce);
    ecs_add(world, the_wizard, radius_component);
    ecs_add(world, the_wizard, colour);
    *(Vec4i *)ecs_get(world, the_wizard, colour) = colour_to_vec4i(ORANGE);
    *(f32 *)ecs_get(world, the_wizard, radius_component) = 20.0;
    *(f32 *)ecs_get(world, the_wizard, bounce) = 0.0;
    *(Vec2 *)ecs_get(world, the_wizard, vel) = (Vec2){0, 0};
    *(Vec2 *)ecs_get(world, the_wizard, pos) = (Vec2){100, 100};
  }
}
