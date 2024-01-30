#ifndef PRINT_H
#define PRINT_H

#include "types.h"
#include <stdio.h>

void lisp_print(LispEnv *lisp, Object object, FILE *stream);

#endif
