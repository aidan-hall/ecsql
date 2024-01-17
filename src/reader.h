#ifndef READER_H
#define READER_H

#include "lisp.h"
#include <stdio.h>

/* Read one Lisp expression as text from 'stream'. */
Object lisp_read(FILE *stream);

#endif
