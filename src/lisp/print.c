#include <lisp/print.h>
#include <lisp/lisp.h>
#include <lisp/object.h>
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

  if (OBJ_TYPE(list) != OBJ_NIL_TAG) {
    fputs(" . ", stream);
    lisp_print(lisp, list, stream);
  }
  fputc(')', stream);
}

void lisp_print(LispEnv *lisp, Object object, FILE *stream) {
  /* for (int i = 0; i < depth; i++) { */
  /*   fputc(' ', stream); */
  /* } */
  switch ((enum ObjectTag)OBJ_TYPE(object)) {
  case OBJ_FLOAT_TAG:
    fprintf(stream, "%f", lisp_unbox_float(object));
    break;
  case OBJ_INT_TAG:
    fprintf(stream, "%d", (i32)OBJ_UNBOX(object));
    break;
  case OBJ_STRING_TAG: {
    s8 s = lisp_string_to_s8(lisp, object);
    fputc('"', stream);
    print_s8(stream, s);
    fputc('"', stream);
    /* fprintf(stream, "\"%s\"", object, */
    /*         (char *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object))); */
  } break;
  case OBJ_SYMBOL_TAG:
    fprintf(stream, "%s", (char *)lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object)));
    break;
  case OBJ_CHAR_TAG:
    fputs("#\\", stream);
    fputc(OBJ_UNBOX(object), stream);
    break;
  case OBJ_PAIR_TAG:
    if (EQ(LISP_CAR(lisp, object), lisp->keysyms.quote)) {
      fputc('\'', stream);
      lisp_print(lisp, LISP_CAR(lisp, LISP_CDR(lisp, object)), stream);
      break;
    }
    [[fallthrough]];
  case OBJ_NIL_TAG:
    lisp_print_list(lisp, object, stream);
    break;
  case OBJ_FILE_PTR_TAG:
    fprintf(stream, "#(stream %ld)", OBJ_UNBOX(object));
    break;
  case OBJ_UNDEFINED_TAG:
    fprintf(stream, "#undefined");
    break;
  case OBJ_PRIMITIVE_TAG:
    fputs("(function ", stream);
    lisp_print(lisp, OBJ_REINTERPRET(object, SYMBOL), stream);
    fputs(")", stream);
    break;
  case OBJ_CLOSURE_TAG:
    fputs("(lambda ", stream);
    lisp_print(lisp, OBJ_REINTERPRET(object, PAIR), stream);
    fputs(")", stream);
    break;
  case OBJ_VECTOR_TAG: {
    fputs("(vector", stream);
    Object *cells = lisp_cell_at(lisp, OBJ_UNBOX_INDEX(object));
    i16 length = OBJ_UNBOX_METADATA(object);
    for (i16 i = 0; i < length; ++i) {
      fputc(' ', stream);
      lisp_print(lisp, cells[i], stream);
    }
    fputs(")", stream);
  } break;
  case OBJ_STRUCT_TAG:
    /* TODO: Implement a clever way to call struct printer methods. */
    fprintf(stream, "(struct ");
    lisp_print(lisp, lisp_type_of(lisp, object), stream);
    fputs(")", stream);
    break;
  }
  /* Should be unreachable. */
}
