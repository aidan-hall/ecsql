;;; Basic definers for macros, functions and globals.
(defname 'macro 'defmacro*
  (lambda (name params . body)
    (list
     'defname ''macro (list 'quote name) (cons 'lambda (cons params body)))))

(defmacro* defun* (name params . body)
  (list
   'defname ''function (list 'quote name) (cons 'lambda (cons params body))))

(defmacro* defvar* (name value)
  (list 'defname ''global (list 'quote name) value))

;;; Doc string-enabled definers.

(defun* not (object)
  (eq object nil))

(defvar* *doc-strings* nil)
(defun* add-doc-string (object string)
  (if (eq (type-of string) 'string)
      (setq *doc-strings* (cons (cons object string) *doc-strings*))
      (wrong "Attempted to pass non-string as docstring" string))
  string)

(defmacro* defmacro (name params . body)
  (if (and (eq (type-of body) 'pair)
           (eq (type-of (cdr body)) 'pair)
           (eq (type-of (car body)) 'string))
      (list 'progn
            (list 'add-doc-string
                  (list 'quote name)
                  (concat
                   (car body)
                   "

Macro arguments: "
                   (to-string params)))
            (cons 'defmacro* (cons name (cons params (cdr body)))))
      (cons 'defmacro* (cons name (cons params body)))))

;; Doc strings for that things that had to be defined before the docstring-enabled definers.
(add-doc-string (function add-doc-string)
                "Add a documentation STRING for the given name.")
(add-doc-string (function not)
                "Returns t iff OBJECT is nil.")
(add-doc-string
 'defmacro
 "Define a macro with the given NAME, PARAMS and BODY in global scope.
If the first form of BODY is a string, it will be used as the docstring for this macro.")

(defmacro defun (name params . body)
  "Define a function with the given NAME, PARAMS and BODY in global scope.
If the first form of BODY is a string,
it will be used as the docstring for this function."
  (if (and (eq (type-of body) 'pair)
           (eq (type-of (cdr body)) 'pair)
           (eq (type-of (car body)) 'string))
      (list 'progn
            (list 'add-doc-string
                  (list 'quote name)
                  (concat
                   (car body)
                   "

Function arguments: "
                   (to-string params)))
            (cons 'defun* (cons name (cons params (cdr body)))))
      (cons 'defun* (cons name (cons params body)))))

(defmacro defvar (name value . docstring?)
  "Define a variable with the given NAME and VALUE.
Use the optional third argument to document the variable."
  (list 'progn
        (list 'defvar* name value)
        (list 'add-doc-string
              (list 'quote name)
              (if (and (consp docstring?) (stringp (car docstring?)))
                  (car docstring?)
                  "Undocumented special/global variable."))
        name))

(defun describe (object)
  "Prints some information about the supplied OBJECT.
This comprises its value, its type, and its docstring if it has one."
  (print object)
  (print (type-of object))
  ((lambda (docform)
     (when docform
       (puts (cdr docform))
       (puts "
")))
   (assoc object *doc-strings*))
  nil)




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
  "Define function NAME that returns t iff its argument has type TYPE."
  (list 'defun name (list 'object)
        (concat "Returns t iff OBJECT is a " (symbol-name type) ".")
        (list 'eq '(type-of object) (list 'quote type))))

(def-type-predicate consp pair)
(def-type-predicate integerp i32)
(def-type-predicate floatp f32)
(def-type-predicate symbolp symbol)
(def-type-predicate relationp relation)
(def-type-predicate entityp entity)
(def-type-predicate stringp string)
(def-type-predicate vectorp vector)

(defun listp (v)
  (or (consp v) (not v)))

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
  "Generate a set of lexical bindings that cannot refer to one another,
and are accessible within the BODY code.
BINDS takes the form: ((name value) (name value) ...)"
  ((lambda (names values)
     (cons
      (cons 'lambda (cons names body))
      values))
   (mapcar (lambda (bind) (if (consp bind) (car bind) bind)) binds)
   (mapcar (lambda (bind) (and (consp bind) (cadr bind))) binds)))

(defmacro let* (binds . body)
  "Generate a set of lexical bindings where each binding can refer to the previous ones."
  (if binds
      (list 'let (list (car binds))
            (cons 'let* (cons (cdr binds) body)))
      (cons 'progn body)))

(defmacro cond clauses
  "Evaluate the first expression in each clause until one is true,
then evaluate the rest of that clause."
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

;;; Less Essential Utilities (Not Needed by any Common Macros)

(defvar gensym-counter 0
  "Added to the name of each gensym so they all appear distinct.
Incremented upon each call to gensym.")
(defun gensym ()
  "Generate a new, uninterned symbol."
  (setq gensym-counter (+ gensym-counter 1))
  (make-symbol (concat "#g" (to-string gensym-counter))))

(defun max (a . as)
  "Obtain the maximum value of list (A . AS)."
  (let ((m a))
    (while (consp as)
      (let ((current (car as)))
        (if (> current m)
            (setq m current)))
      (setq as (cdr as)))
    m))

(defun negated (f)
  (lambda (x)
    (not (funcall f x))))

;;; Control Flow macros
(defmacro unless (cond . body)
  `(if ,cond
       nil
       . ,body))
(defmacro when (cond . body)
  `(if ,cond
       (progn . ,body)))

(defmacro case (expr . clauses)
  "Evaluate EXPR. Evaluate the first matching clause in CLAUSES.
A clause has the form ((forms...) body...).
A clause matches if the result of evaluating EXPR is eql to one of the forms in its car."
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

(defmacro dotimes (iterator . body)
  "Evaluate BODY a given number of times in sequence.
ITERATOR takes the form (var n).
Var is bound to 0, 1, ..., n-1 on the corresponding iteration."
  (let ((varname (car iterator)))
    `(let ((,varname 0))
       (while (< ,varname ,(cadr iterator))
         (progn . ,body)
         (incq ,varname)))))

;;; Functional/List Processing Macros

(defun filter (f l)
  (if l
      (let ((rest (filter f (cdr l))))
        (if (funcall f (car l))
            (cons (car l) rest)
            rest))
      nil))

(defun reduce (f start elements)
  "Reduce ELEMENTS to a single value with function F, starting with START.
Performs a left fold."
  (while (consp elements)
    (setq start (funcall f start (car elements)))
    (setq elements (cdr elements)))
  start)

(defun last (l)
  "Returns the last cons cell in list L, or nil if L is nil."
  (if (and (consp l) (consp (cdr l)))
      (last (cdr l))
      l))

(defun nconc lists
  "Concatenate LISTS by modifying them. The last list is not modified."
  (while (and lists (not (car lists)))
    (setq lists (cdr lists)))
  (when (consp lists)
    (reduce
     (lambda (front next)
       (if next (setcdr (last front) next)))
     (car lists)
     (cdr lists))
    (car lists)))

(defun zip (a b)
  "Zip lists A and B together."
  (when (and (consp a) (consp b))
    (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))

(defun union (a b)
  "Compute the union of lists A & B.
Elements are compared with eql."
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
        ((vector)
         (let ((len (length form))
               (i 0))
           (fputs "(vector" stream)
           (while (< i len)
             (fputc #\  stream)
             (prin1-to stream (aref form i))
             (incq i))
           (fputs ")" stream)))
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
  "Return t if `a' and `b' are the same, traversing the sub-structure of `cons' pairs."
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
  "Write STRING to STDOUT."
  (fputs string stdout))

(defun repl ()
  "Run a Read-Eval-Print loop."
  (while (not (feof stdin))
    (puts "* ")
    (let ((form (read-stream stdin)))
      (if (eq form eof)
          (puts "End of file reached. Goodbye.
")
          (prin1 (eval form))
          (puts "
"))))
  (puts "End of file reached. Goodbye.
"))

;; Doc strings for primitives

(defmacro generate-primitive-docstrings (entries)
  "Generate doc strings for the given primitive functions.
ENTRIES is a list, with items of the form (name docstring)."
  (cons
   'progn
   (mapcar
    (lambda (entry)
      `(add-doc-string
        ',(car entry)
        (concat ,(cadr entry)
                "

Primitive function.
"
                ,(if (cddr entry)
                     (concat "Arguments: " (to-string (caddr entry)) ".
")
                     "")
                "Typespec: "
                (to-string (function ,(car entry))) ".")))
    entries)))

(generate-primitive-docstrings
 ((/ "Divide the first argument by each of the remaining arguments.
With one argument, divide 1 by it.")
  (- "Subtract from the first argument all remaining arguments.
With one argument, negate it.")
  (quit "Quit Lisp.")
  (symbol-name "Get the name of the supplied symbol as a string.")
  (intern "Obtain the canonical symbol with the given name name.
I.e. (eq 'a (intern (make-string 1 #\a))) => t")
  (make-symbol "Produce a new, uninterned symbol with the given name.
Calling make-symbol twice with the same argument will produce two symbols that are distinct from
each other, and the canonical symbol with that name.")
  (make-string "Given arguments (n c), produce a string length N, with every character being C.")
  (make-vector "Given arguments (n v), produce a vector length N, with every element being V.")
  (vector "Produce a vector containing the arguments.")
  (aref "Get the nth element of the vector.")
  (aset "Set the nth element of the vector to the supplied value.")
  (eq "Return t iff the arguments are bit-for-bit the same.")
  (eql "Return t iff the arguments are equal.
• Numbers: Numerically equal.
• Strings: Same length, and all characters the same.")
  (length "Returns the length of the given list, vector or string.")
  (to-string "Returns the printed representation of the argument as a string.")
  (type-of "Returns the symbol representing the type of the argument.")
  (type-tag "Returns the Object type tag of the argument.
Note that all structs have the same type tag.")
  (funcall "Apply the first argument to the remaining arguments.")
  (apply "Apply the first argument to the remaining arguments.
The last argument is a list of arguments to pass to the function.
E.g. (apply #'+ 4 '(1 2 3)) => 10.")
  (eval "Evaluate the argument form.
It is preferable to use macros or other language features over this function where possible.")
  (macroexpand-1 "Expand the top-level macro in the argument form, if there is one.")
  (macroexpand "Recursively expand out all macros in the argument form.")
  (wrong "Signal an error, displaying a message and the value of the second argument.")
  (size-of "Return the number of Bytes necessary to store elements of the argument type in a struct.")
  (type-spec-matches "Returns t iff the supplied form matches the supplied type spec." (form spec))
  (structp "Returns t iff the argument is a struct.")
  (struct-metadata "Returns reflection data about the given struct type.")
  (ecs-new "Create and return a new ECS entity.")
  (make-entity "Produce an Entity object with the given id and generation.
Not guaranteed to be a live entity.")
  (ecs-pair "Produce a Relation object with the given RELATION and TARGET.
Not guaranteed to be a valid Relation (wherein RELATION and TARGET are both alive."
                 (relation target))
  (make-relation "Produce a Relation object with the given RELATION and TARGET.
Not guaranteed to be a valid Relation (wherein RELATION and TARGET are both alive."
                 (relation target))
  (ecs-entity "Returns the Entity with the argument ID, if alive. Otherwise, nil.")
  (ecs-destroy "Destroy the supplied Entity.")
  (ecs-get "Obtain the value of COMPONENT for ENTITY.
ENTITY must have COMPONENT, and COMPONENT must have LispStorage,
or an error is raised." (entity component))
  (ecs-set "Set the value of COMPONENT for ENTITY.
ENTITY must already have COMPONENT, and COMPONENT must have LispStorage.
VALUE must be of COMPONENT's LispStorage type (see ecs-storage-type)." (entity component value))
  (ecs-set-name "Set the name of ENTITY.
Names are not Components, and are used to find Entities with ecs-lookup." (entity name))
  (ecs-lookup "Obtain the Entity with the given name, if it exists." (name))
  (ecs-has "Returns t iff ENTITY has COMPONENT." (entity component))
  (ecs-add "Add COMPONENT to ENTITY." (entity component))
  (ecs-id "Returns the ID of ENTITY." (entity))
  (ecs-gen "Returns the generation of ENTITY." (entity))
  (ecs-relation "Returns the Relation type of RELATION" (relation))
  (ecs-target "Returns the target Entity of RELATION" (relation))
  (ecs-new-component "Creates a new Component that stores values of type TYPE.")
  (ecs-do-query "Run FUNCTION on every Entity matching QUERY.
This is the backend to ecsql, which you should probably use instead." (function query))
  (ecs-register-system "Create a new System (Entity) with the given FUNCTION and QUERY added.
This is the backend to ecs-new-system, which you should probably use instead." (function query))
  (ecs-storage-type "Obtain the type of Lisp Object stored by COMPONENT.
If COMPONENT is a Relation, this will be the same as (ecs-storage-type (ecs-relation COMPONENT))." (component))
  (get-mouse-x "Get the X coordinate of the mouse cursor on the game window.")
  (get-mouse-y "Get the Y coordinate of the mouse cursor on the game window.")
  (get-screen-width "Get the width of the game window in pixels.")
  (get-screen-height "Get the height of the game window in pixels.")
  (get-delta "Get the duration of the last frame.")
  (draw-text "Draw TEXT at the given X and Y coordinates, at the given SIZE."
             (text x y size))
  (is-mouse-down "Returns t iff mouse button BUTTON is currently held down.
Allowed values of BUTTON: left, right." (button))
  (is-mouse-pressed "Returns t iff mouse button BUTTON was just pressed.
Allowed values of BUTTON: left, right." (button))))
