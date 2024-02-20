;;; Basic definers for macros, functions and globals.
(defname 'macro 'defmacro
         (lambda (name params . body)
           (list
            'defname ''macro (list 'quote name) (cons 'lambda (cons params body)))))

(defmacro defun (name params . body)
  (list
   'defname ''function (list 'quote name) (cons 'lambda (cons params body))))

(defmacro defvar (name value)
  (list 'defname ''global (list 'quote name) value))

;;; Common shorthands for accessing the contents of
(defun cadr (x)
  (car (cdr x)))

(defun caddr (x)
  (car (cdr (cdr x))))

(defun cadddr (x)
  (car (cdr (cdr (cdr x)))))
(defun cddr (x)
  (cdr (cdr x)))

;;; Type Predicates

(defmacro def-type-predicate (name type)
  ;; Generate a function that tests if a value has the specified type.
  (list 'defun name (list 'v)
        (list 'eq '(type-of v) (list 'quote type))))
(def-type-predicate consp pair)
(def-type-predicate integerp i32)
(def-type-predicate floatp f32)
(def-type-predicate symbolp symbol)
(def-type-predicate relationp relation)
(def-type-predicate entityp entity)

(defun not (v)
  (eq v nil))

(defun listp (v)
  (or (consp v) (not v)))

(defun type-spec-matches (value spec)
  ;; Logically equivalent to lisp_type_spec_matches in types.c.
  (or (eq spec t)
      (and (consp value) (consp spec)
           (type-spec-matches (car value) (car spec))
           (type-spec-matches (cdr value) (cdr spec)))
      (and (not value) (not spec))
      (eq (type-of value) spec)))

;;; Essential Utility Functions

(defun mapcar (f l)
  (if l
      (cons (funcall f (car l))
            (mapcar f (cdr l)))
    nil))

(defun memql (elt list)
  (if (consp list)
      (if (eql elt (car list))
          list
        (memql elt (cdr list)))))

;;; Common Macros
(defmacro let (binds . body)
  ;; Generate a set of lexical bindings that cannot refer to one another,
  ;; and are accessible within the `body' code.
  ;; `binds' takes the form: ((name value) (name value) ...)
  ((lambda (names values)
     (cons
      (cons 'lambda (cons names body))
      values))
   (mapcar (lambda (bind) (if (consp bind) (car bind) bind)) binds)
   (mapcar (lambda (bind) (and (consp bind) (cadr bind))) binds)))

(defmacro let* (binds . body)
  ;; Generate a set of lexical bindings where each binding can refer to the previous ones.
  (if binds
      (list 'let (list (car binds))
            (cons 'let* (cons (cdr binds) body)))
    (cons 'progn body)))

(defmacro cond clauses
  ;; Evaluate the first expression in each clause until one is true,
  ;; then evaluate the rest of that clause.
  (if (consp clauses)
      (let ((current (car clauses)))
        (list 'if (car current) (cons 'progn (cdr current))
              (cons 'cond (cdr clauses))))
    clauses))

(defun qq-list-form (list)
  ;; Whether a quasiquoted expression can be translated into a call to `list'.
  (or (not list)
      (and (consp list) (not (eq (car list) 'unquote)) (qq-list-form (cdr list)))))

(defun quasiquote-rec (form level)
  ;; Expand a quasiquoted form at the given level of nested quasiquotation.
  ;; One backtick increases `level' by 1, one comma decreases it by 1.
  ;; Data at level 0 is inserted as code.
  (cond
   ((eq level 0)
    ;; At level 0, the form is unquoted.
    form)

   ((not (consp form))
    ;; Non-cons forms don't have sub-structure, so we can simply quote them and stop.
    (list 'quote form))

   (t
    (let ((head (car form)))
      (cond
       ((eq head 'unquote)
        ;; Decrease quasiquotation level by 1 for the sub-structure.
        (let ((unquoted (quasiquote-rec (cadr form) (- level 1))))
          ;; Only remove the innermost unquote (that reaches level 0).
          ;; Repeated macroexpansion will successively remove the rest of the unquotes.
          (if (> level 1)
              (list 'unquote unquoted)
            unquoted)))

       ((eq head 'quasiquote)
        ;; Increase quasiquotation level by 1 for the sub-structure.
        (list 'quasiquote (quasiquote-rec (cadr form) (+ level 1))))

       (t
        (if (qq-list-form form)
            ;; Generating calls to `list' instead of `cons' makes the output shorter.
            (cons 'list (mapcar (lambda (form) (quasiquote-rec form level)) form))
          (list 'cons
                (quasiquote-rec head level)
                (quasiquote-rec (cdr form) level)))))))))

(defmacro quasiquote (form)
  (quasiquote-rec form 1))

(defmacro unless (cond . body)
  `(if ,cond
       nil
     . ,body))
(defmacro when (cond . body)
  `(if ,cond
       (progn . ,body)))

(defmacro case (expr . clauses)
  (let ((gname (gensym)))
    `(let ((,gname ,expr))
       (cond . ,(mapcar
                 (lambda (clause)
                   ;; Treat t and otherwise as default clauses.
                   (if (memql (car clause) '(t otherwise))
                       (cons t (cdr clause))
                     ;; Use `eql' directly if there is only one item in the clause.
                     `(,(if (not (cdr (car clause)))
                            `(eql ,gname ',(car (car clause)))
                          `(memql ,gname ',(car clause)))
                       . ,(cdr clause))))
                 clauses)))))

;;; Less Essential Utilities (Not Needed by any Common Macros)
(defun max (a . as)
  (let ((m a))
    (while (consp as)
      (let ((current (car as)))
        (if (> current m)
            (setq m current)))
      (setq as (cdr as)))
    m))

(defvar gensym-counter 0)
(defun gensym ()
  (setq gensym-counter (+ gensym-counter 1))
  (make-symbol (concat "#g" (to-string gensym-counter))))

(defun filter (f l)
  (if l
      (let ((rest (filter f (cdr l))))
        (if (funcall f (car l))
            (cons (car l) rest)
          rest))
    nil))

(defun reduce (f start elements)
  (while (consp elements)
    (setq start (funcall f start (car elements)))
    (setq elements (cdr elements)))
  start)

(defun last (l)
  (if (and (consp l) (consp (cdr l)))
      (last (cdr l))
    l))

(defun nconc lists
  (while (and lists (not (car lists)))
    (setq lists (cdr lists)))
  (when (consp lists)
    (reduce
     (lambda (front next)
       (if next (setcdr (last front) next)))
     (car lists)
     (cdr lists))
    (car lists)))

(defun negated (f)
  (lambda (x)
    (not (funcall f x))))

(defun union (a b)
  (reduce
   (lambda (acc elem)
     (if (memql elem acc)
         acc
         (cons elem acc)))
   a
   b))

(defun prin1-list (file list)
  (fputc #\( file)
  (while (and (consp list) (consp (cdr list)))
    (prin1-to file (car list))
    (fputc #\  file)
    (setq list (cdr list)))
  (when (consp list)
    (prin1-to file (car list))
    (when (cdr list)
      (fputs " . " file)
      (prin1-to file (cdr list))))
  (fputc #\) file))

(defun prin1-to (stream form)
  (if (structp form)
      (prin1-struct-to stream form)
    (case (type-of form)
      ((pair)
       (if (eq (car form) 'quote)
        (progn
          (fputc #\' stream)
          (prin1-to stream (cadr form)))
        (prin1-list stream form)))
      ((nil)
       (fputs "()" stream))
      ((character)                    ; #\c escape
       (fputc #\# stream)
       (fputc #\\ stream)
       (fputc form stream))
      ((entity)
       (fputs "#*entity" stream)
       (prin1-to stream (list (ecs-id form) (ecs-gen form))))
      ((relation)
       (fputs "#*relation" stream)
       (prin1-to stream (list (ecs-relation form) (ecs-target form))))
      (t
       (prin1-to* stream form)))))

(defun prin1 (form)
  (prin1-to stdout form))

(defun print (form)
  (fputc #\ stdout)
  (prin1 form)
  (fputs "
" stdout))

(defun equal (a b)
  ;; Return t if `a' and `b' are the same, traversing the sub-structure of `cons' pairs.
  ;; TODO: Do the right thing for vectors and structs.
  (if (and (consp a) (consp b))
      (and (equal (car a) (car b))
           (equal (cdr a) (cdr b)))
    (eql a b)))

(defmacro assert (spec)
  `(unless ,spec
     (wrong "ASSERTION FAILURE" ',spec)))

(defmacro incq (var . rest)
  (assert (symbolp var))
  `(setq ,var (+ ,var ,(if rest (car rest) 1))))

(defun puts (string)
  ;; Write `string' to `stdout'.
  (fputs string stdout))

(defun repl ()
  ;; Run a Read-Eval-Print loop
  (while (not (feof stdin))
    (puts "* ")
    (let ((form (read-stream stdin)))
      (if (eq form eof)
          (puts "End of file reached. Goodbye.
")
        (prin1 (eval form))
        (puts "
")
        )))
  (puts "End of file reached. Goodbye.
"))
