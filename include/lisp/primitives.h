#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include "types.h"

/* Add a set of primitive functions to the Lisp environment.
 * This API would be easy to use with shared libraries,
 * that would make it possible to add new primitives without recompiling ECSQL
 * itself. There's currently no reason to do that, however. */
void lisp_install_primitives(struct LispEnv *lisp);
#endif
