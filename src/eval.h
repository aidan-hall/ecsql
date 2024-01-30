#ifndef EVAL_H
#define EVAL_H

#include "types.h"

Object lisp_bind(struct LispEnv *lisp, Object parameters, Object arguments,
                 Object context);
Object lisp_evaluate(struct LispEnv *lisp, Object expression, Object context);
Object lisp_eval(struct LispEnv *lisp, Object expression);
Object lisp_apply(struct LispEnv *lisp, Object function, Object arguments);
Object lisp_evaluate_sequence(struct LispEnv *lisp, Object sequence, Object context);


#endif
