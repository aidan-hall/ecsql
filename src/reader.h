#ifndef READER_H
#define READER_H

#include "lisp.h"
#include <stdio.h>

/* Read one Lisp expression as text from 'stream'. */
Object lisp_read(LispEnv *lisp, FILE *stream);
Object lisp_read_from_string(LispEnv *lisp, s8 str);

#define LISP_PROMPT "* "
#endif
