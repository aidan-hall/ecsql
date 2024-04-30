#ifndef READER_H
#define READER_H

#include "types.h"
#include <stdio.h>

/* Read one Lisp expression as text from 'stream'. */
Object lisp_read(struct LispEnv *lisp, FILE *stream);
/* Read one Lisp Object from string str. */
Object lisp_read_from_string(struct LispEnv *lisp, s8 str);

#define OBJS(LISP, S) (lisp_read_from_string(LISP, s8(S)))

#define LISP_PROMPT "* "
#endif
