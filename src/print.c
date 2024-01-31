#include "print.h"
#include "lisp.h"

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
    fputc('?', stream);
    fputc(OBJ_UNBOX(object), stream);
    break;
  case OBJ_PAIR_TAG:
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
    lisp_print(lisp, LISP_CAR(lisp, LISP_CDR(lisp, object)), stream);
    fputs(")", stream);
    break;
  }
}
