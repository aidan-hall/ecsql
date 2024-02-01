(defname 'macro 'defmacro
         (lambda (name params . body)
           (list
            'defname ''macro (list 'quote name) (cons 'lambda (cons params body)))))

(defmacro defun (name params . body)
  (list
   'defname ''function (list 'quote name) (cons 'lambda (cons params body))))

(defmacro define (name value)
  (list 'defname ''global (list 'quote name) value))

(defun cadr (x)
  (car (cdr x)))

(defun caddr (x)
  (car (cdr (cdr x))))

(defun cadddr (x)
  (car (cdr (cdr (cdr x)))))


(defmacro def-type-predicate (name type)
  (list 'defun name (list 'v)
     (list 'eq '(type-of v) (list 'quote type))))
(def-type-predicate consp pair)
(def-type-predicate integerp i32)
(def-type-predicate floatp f32)

(defun not (v)
  (eq v nil))
(defun listp (v)
  (or (consp v) (not v)))
(defun type-spec-matches (value spec)
  (or (eq spec t)
      (and (consp value) (consp spec)
           (type-spec-matches (car value) (car spec))
           (type-spec-matches (cdr value) (cdr spec)))
      (and (not value) (not spec))
      (eq (type-of value) spec)))
(defun mapcar (f l)
  (if (not l)
      nil
    (cons (funcall f (car l))
          (mapcar f (cdr l)))))

(defmacro let (binds . body)
  ((lambda (names values)
     (cons
      (cons 'lambda (cons names body))
       values))
   (mapcar (lambda (bind) (if (consp bind) (car bind) bind)) binds)
   (mapcar (lambda (bind) (and (consp bind) (cadr bind))) binds)))

(defmacro let* (binds . body)
  (if binds
      (list 'let (list (car binds))
         (cons 'let* (cons (cdr binds) body)))
    (cons 'progn body)))

(defmacro cond clauses
  (if (consp clauses)
      (let ((current (car clauses)))
        (list 'if (car current) (cons 'progn (cdr current))
              (cons 'cond (cdr clauses))))
    clauses))

(defun qq-list-form (list)
  (or (not list)
      (and (consp list) (not (eq (car list) 'unquote)) (qq-list-form (cdr list)))))
(defun quasiquote-rec (form level)
  (cond
   ((eq level 0)
    form)
   ((not (consp form))
    (list 'quote form))
   (t
    (let ((head (car form)))
      (cond
       ((eq head 'unquote)
        (let ((unquoted (quasiquote-rec (cadr form) (- level 1))))
          (print (list "unquote at level:" level))
          (if (> level 1)
              (list 'unquote unquoted)
            unquoted)))
       ((eq head 'quasiquote)
        (list 'quasiquote (quasiquote-rec (cadr form) (+ level 1))))
       (t
        (if (qq-list-form form)
            (cons 'list (mapcar (lambda (form) (quasiquote-rec form level)) form))
          (list 'cons
           (quasiquote-rec head level)
           (quasiquote-rec (cdr form) level)))))))))

(defmacro quasiquote (form)
  (quasiquote-rec form 1))

(defun equal (a b)
  (if (and (consp a) (consp b))
      (and (equal (car a) (car b))
           (equal (cdr a) (cdr b)))
    (eql a b)))

(defun puts (string)
  (fputs string stdout))

(defun repl ()
  (while (not (feof stdin))
    (puts "* ")
    (let ((form (read-stream stdin)))
      (if (eq form eof)
          (puts "End of file reached. Goodbye.
")
        (print (eval form)))))
  (puts "End of file reached. Goodbye."))
