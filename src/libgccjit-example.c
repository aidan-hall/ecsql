#include "lexer.h"
#include <libgccjit.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* int main(int argc, char *argv[]) { */
/*   if (argc <= 1) */
/*     return 1; */

/*   s8 stream = (s8){(u8 *)argv[1], strlen(argv[1])}; */
/*   for (Token tok = get_token(&stream); tok.t != TOK_ERROR && tok.t !=
 * TOK_END; */
/*        tok = get_token(&stream)) { */
/*     printf("Token, t=%c '", tok.t); */
/*     fwrite(tok.lexeme.data, sizeof(u8), tok.lexeme.len, stdout); */
/*     printf("'\n"); */
/*   } */

/*   return 0; */
/* } */

/* Smoketest example for libgccjit.so
   Copyright (C) 2014-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <libgccjit.h>

#include <stdio.h>
#include <stdlib.h>
/* Usage example for libgccjit.so
   Copyright (C) 2014-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <libgccjit.h>

#include <stdio.h>
#include <stdlib.h>

#define GCC_JIT_AS_OBJECT(X)                                                   \
  _Generic((X),                                                                \
      gcc_jit_location *: gcc_jit_location_as_object,                          \
      gcc_jit_type *: gcc_jit_type_as_object,                                  \
      gcc_jit_field *: gcc_jit_field_as_object,                                \
      gcc_jit_param *: gcc_jit_param_as_object,                                \
      gcc_jit_function *: gcc_jit_function_as_object,                          \
      gcc_jit_block *: gcc_jit_block_as_object,                                \
      gcc_jit_lvalue *: gcc_jit_lvalue_as_object,                              \
      gcc_jit_rvalue *: gcc_jit_rvalue_as_object,                              \
      gcc_jit_case *: gcc_jit_case_as_object,                                  \
      gcc_jit_extended_asm *: gcc_jit_extended_asm_as_object)(X)

#define GCC_DEBUG_LOG(X)                                                       \
  fprintf(stderr, #X ": %s\n",                                                 \
          gcc_jit_object_get_debug_string(GCC_JIT_AS_OBJECT(X)))

#define VEC_SIZE (4)
static void create_code(gcc_jit_context *ctxt) {
  /* Let's try to inject the equivalent of:

      int square (int i)
      {
        return i * i;
      }
   */
  gcc_jit_type *int_type = gcc_jit_context_get_type(ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *double_type =
      gcc_jit_context_get_type(ctxt, GCC_JIT_TYPE_DOUBLE);
  gcc_jit_type *int_vector_type = gcc_jit_type_get_vector(int_type, VEC_SIZE);
  gcc_jit_param *param_i =
      gcc_jit_context_new_param(ctxt, NULL, int_vector_type, "i");
  GCC_DEBUG_LOG(param_i);
  /* { */
  /*   gcc_jit_object *param_i_obj = GCC_JIT_AS_OBJECT(param_i); */
  /*   printf("param_i: %s\n", gcc_jit_object_get_debug_string(param_i_obj)); */
  /* } */
  gcc_jit_function *func =
      gcc_jit_context_new_function(ctxt, NULL, GCC_JIT_FUNCTION_EXPORTED,
                                   int_vector_type, "square", 1, &param_i, 0);
  gcc_jit_param *x_param =
    gcc_jit_context_new_param(ctxt, nullptr, double_type, "x");
  gcc_jit_param *y_param =
    gcc_jit_context_new_param(ctxt, nullptr, double_type, "y");
  gcc_jit_rvalue *x_rvalue = gcc_jit_param_as_rvalue(x_param);
  gcc_jit_function *normal_sin_func =
    gcc_jit_context_new_function(ctxt, nullptr, GCC_JIT_FUNCTION_IMPORTED, double_type, "sin", 1, &y_param, 0);
  fprintf(stderr, "normal sin: %p\n", (void *) normal_sin_func);
  gcc_jit_function *sin_func =
      gcc_jit_context_get_builtin_function(ctxt, "__builtin_sin");
  fprintf(stderr, "__builtin_sin, indirected: %p\n", (void *)sin_func);
  gcc_jit_function *my_sin_func =
      gcc_jit_context_new_function(ctxt, nullptr, GCC_JIT_FUNCTION_EXPORTED,
                                   double_type, "sine", 1, &x_param, 0);
  gcc_jit_block_end_with_return(gcc_jit_function_new_block(my_sin_func, nullptr), nullptr,
				/* normal_sin_func would also work here */
				gcc_jit_context_new_call(ctxt, nullptr, sin_func, 1, &x_rvalue));
  /* gcc_jit_context_new_function(ctxt, NULL, GCC_JIT_FUNCTION_IMPORTED, */
  /*                                double_type, "sin", 1, &x_param, 0); */
  GCC_DEBUG_LOG(func);
  GCC_DEBUG_LOG(sin_func);
  gcc_jit_block *block = gcc_jit_function_new_block(func, NULL);

  gcc_jit_rvalue *expr = gcc_jit_context_new_binary_op(
      ctxt, NULL, GCC_JIT_BINARY_OP_MULT, int_vector_type,
      gcc_jit_param_as_rvalue(param_i), gcc_jit_param_as_rvalue(param_i));
  GCC_DEBUG_LOG(expr);
  gcc_jit_block_end_with_return(block, NULL, expr);
}

static int example_main(int argc, char **argv) {
  gcc_jit_context *ctxt = NULL;
  gcc_jit_result *result = NULL;

  /* Get a "context" object for working with the library.  */
  ctxt = gcc_jit_context_acquire();
  if (!ctxt) {
    fprintf(stderr, "NULL ctxt");
    goto error;
  }

  /* Set some options on the context.
     Let's see the code being generated, in assembler form.  */
  gcc_jit_context_set_bool_option(ctxt, GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE,
                                  0);

  /* Populate the context.  */
  create_code(ctxt);

  /* Compile the code.  */
  result = gcc_jit_context_compile(ctxt);
  if (!result) {
    fprintf(stderr, "NULL result");
    goto error;
  }

  /* We're done with the context; we can release it: */
  gcc_jit_context_release(ctxt);
  ctxt = NULL;

  /* Extract the generated code from "result".  */
  void *fn_ptr = gcc_jit_result_get_code(result, "square");
  if (!fn_ptr) {
    fprintf(stderr, "NULL fn_ptr\n");
    goto error;
  }

  typedef int v4si __attribute((vector_size(sizeof(int) * VEC_SIZE)));
  typedef v4si (*fn_type)(v4si);
  fn_type square = (fn_type)fn_ptr;
  v4si x = {1, 2, 3, 99};
  v4si res = square(x);

  printf("result: ");
  for (int i = 0; i < VEC_SIZE; ++i) {
    printf("%d, ", res[i]);
  }
  printf("\n");

  fn_ptr = gcc_jit_result_get_code(result, "sine");
  if (!fn_ptr) {
    fprintf(stderr, "NULL fn_ptr (sine)\n");
    goto error;
  }

  double y = M_PI;
  typedef double (*trig_fn)(double);
  trig_fn my_sin_ptr = fn_ptr;
  double res2 = my_sin_ptr(y);
  printf("sin(%f): %f\n", y, res2);

error:
  if (ctxt)
    gcc_jit_context_release(ctxt);
  if (result)
    gcc_jit_result_release(result);
  return 0;
}
