#include <lisp/lisp.h>
#include <lisp/object.h>
#include <lisp/print.h>
#include <lisp/types.h>

/* list *must* be a pair or nil.  Any sub-structure should be handled correctly.
 */
static void lisp_print_list(LispEnv *lisp, Object list, FILE *stream) {
  fputc('(', stream);
  while (OBJ_TYPE(list) == OBJ_PAIR_TAG) {
    lisp_print(lisp, LISP_CAR(lisp, list), stream);
    list = LISP_CDR(lisp, list);
    if (OBJ_TYPE(list) == OBJ_PAIR_TAG)
      fputc(' ', stream);
  }

  if (!EQ(list, NIL)) {
    fputs(" . ", stream);
    lisp_print(lisp, list, stream);
  }
  fputc(')', stream);
}

void lisp_print(LispEnv *lisp, Object object, FILE *stream) {
  /* for (int i = 0; i < depth; i++) { */
  /*   fputc(' ', stream); */
  /* } */
  switch (OBJ_TYPE(object)) {
  case OBJ_FLOAT_TAG:
    fprintf(stream, "%f", lisp_unbox_float(object));
    return;
  case OBJ_INT_TAG:
    fprintf(stream, "%d", (i32)OBJ_UNBOX(object));
    return;
  case OBJ_STRING_TAG: {
    s8 s = lisp_string_to_s8(lisp, object);
    fputc('"', stream);
    print_s8(stream, s);
    fputc('"', stream);
    /* fprintf(stream, "\"%s\"", object, */
    /*         (char *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object))); */
  }
    return;
  case OBJ_SYMBOL_TAG:
    fprintf(stream, "%s", (char *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object)));
    return;
  case OBJ_CHAR_TAG:
    fputs("#\\", stream);
    fputc(OBJ_UNBOX(object), stream);
    return;
  case OBJ_PAIR_TAG:
    if (EQ(LISP_CAR(lisp, object), lisp->keysyms.quote)) {
      fputc('\'', stream);
      lisp_print(lisp, LISP_CAR(lisp, LISP_CDR(lisp, object)), stream);
      return;
    }
    [[fallthrough]];
  case OBJ_NIL_TAG:
    lisp_print_list(lisp, object, stream);
    return;
  case OBJ_FILE_PTR_TAG:
    fprintf(stream, "#s%ld", OBJ_UNBOX(object));
    return;
  case OBJ_UNDEFINED_TAG:
    fprintf(stream, "#!undefined");
    return;
  case OBJ_PRIMITIVE_TAG: {
    fputs("(function ", stream);
    khiter_t iter = kh_get(primitives, lisp->primitive_functions, object.bits);
    if (iter == kh_end(lisp->primitive_functions)) {
      WRONG("Attempted to print an invalid primitive function");
    }
    InterpreterPrimitive fn = kh_value(lisp->primitive_functions, iter);
    lisp_print(lisp, fn.id_symbol, stream);
    fputc(' ', stream);
    lisp_print(lisp, fn.argument_types, stream);
    fputs(")", stream);
    return;
  }
  case OBJ_CLOSURE_TAG:
    fputs("(lambda ", stream);
    lisp_print(lisp, LISP_CAR(lisp, LISP_CDR(lisp, object)), stream);
    fputs(")", stream);
    return;
  case OBJ_VECTOR_TAG: {
    fputs("(vector", stream);
    Object *cells = lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object));
    i16 length = OBJ_UNBOX_METADATA(object);
    for (i16 i = 0; i < length; ++i) {
      fputc(' ', stream);
      lisp_print(lisp, cells[i], stream);
    }
    fputs(")", stream);
  }
    return;
  case OBJ_STRUCT_TAG:
    /* TODO: Implement a clever way to call struct printer methods. */
    fprintf(stream, "(struct ");
    lisp_print(lisp, lisp_type_of(lisp, object), stream);
    fputs(")", stream);
    return;
  case OBJ_ENTITY_TAG:
    fprintf(stream, "#*entity(%d %d)", object.id.val, object.gen);
    return;
  case OBJ_RELATION_TAG:
    fprintf(stream, "#*relation(%d %d)", object.relation, object.entity.val);
    return;
  }
  /* Should be unreachable. */
  WRONG("Invalid object tag: ", OBJ_BOX(object.tag, INT));
}
