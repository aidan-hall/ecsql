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
