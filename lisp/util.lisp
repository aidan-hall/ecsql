;;; Basic definers for macros, functions and globals.
(defname 'macro 'defmacro*
  (lambda (name params . body)
    (list
     ;; The macro symbol must be quoted in the output, so double-quote it.
     'defname ''macro (list 'quote name) (cons 'lambda (cons params body)))))

(defmacro* defun* (name params . body)
  (list
   'defname ''function (list 'quote name) (cons 'lambda (cons params body))))

(defmacro* defvar* (name value)
  (list 'defname ''global (list 'quote name) value))


;;; Doc (documentation) string facility.

(defvar* *doc-strings* nil)
(defun* add-doc-string (object string)
  (if (eq (type-of string) 'string)
      (setq *doc-strings* (cons (cons object string) *doc-strings*))
      (wrong "Attempted to pass non-string as docstring" string))
  string)

(add-doc-string 'add-doc-string
                "Function, arguments: (name string)

Add documentation STRING for the given NAME (which should probably be a symbol).
When referring to function or macro parameters in a documentation string,
we capitalise their names.")
(add-doc-string '*doc-strings*
                "An alist mapping names to documentation strings.
See (describe 'add-doc-string).")

;;; Definers with support for doc strings.

(defmacro* defmacro (name params . body)
  ;; If the first element of body is a string, use it as the doc string.
  (if (and (eq (type-of body) 'pair)
           (eq (type-of (cdr body)) 'pair)
           (eq (type-of (car body)) 'string))
      (list 'progn
            (list 'add-doc-string
                  ;; We need the *value* of name to be quoted in the output.
                  (list 'quote name)
                  (concat
                   "Macro, arguments: "
                   (to-string params)
                   "

"
                   (car body)))
            ;; Defer to defun* for creating the definition in either case.
            (cons 'defmacro* (cons name (cons params (cdr body)))))
      (cons 'defmacro* (cons name (cons params body)))))

(add-doc-string
 'defmacro
 "Macro, arguments: (name params . body)

Define a macro with the given NAME, PARAMS and BODY in global scope.
If the first form of BODY is a string, it will be used as the docstring for this macro.")

(defmacro defun (name params . body)
  "Define a function with the given NAME, PARAMS and BODY in global scope.
If the first form of BODY is a string,
it will be used as the docstring for this function."
  ;; See defmacro.
  (if (and (eq (type-of body) 'pair)
           (eq (type-of (cdr body)) 'pair)
           (eq (type-of (car body)) 'string))
      (list 'progn
            (list 'add-doc-string
                  (list 'quote name)
                  (concat
                   "Function, arguments: "
                   (to-string params)
                   "

 "
                   (car body)))
            (cons 'defun* (cons name (cons params (cdr body)))))
      (cons 'defun* (cons name (cons params body)))))

(defmacro defvar (name value . docstring?)
  "Define a variable with the given NAME and VALUE.
Use the optional third argument to document the variable."
  ;; See defmacro.
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
     (if docform
         (progn
           (puts (cdr docform))
           (puts "
"))))
   (assoc object *doc-strings*))
  nil)

;;; Common shorthands for accessing the contents of lists

(defun cadr (x)
  (car (cdr x)))

(defun cdar (x)
  (cdr (car x)))
(defun caar (x)
  (car (car x)))
(defun cddr (x)
  (cdr (cdr x)))

(defun caddr (x)
  (car (cdr (cdr x))))

(defun cadddr (x)
  (car (cdr (cdr (cdr x)))))

(defun caadr (x)
  (car (car (cdr x))))

(defun cdadr (x)
  (cdr (car (cdr x))))

;;; Type Predicates

(defmacro def-type-predicate (name type)
  "Define function NAME that returns t iff its argument has type TYPE."
  (list 'defun name (list 'object)
        (concat "Returns t iff OBJECT is of type " (symbol-name type) ".")
        (list 'eq '(type-of object) (list 'quote type))))

(def-type-predicate consp pair)
(def-type-predicate integerp i32)
(def-type-predicate floatp f32)
(def-type-predicate symbolp symbol)
(def-type-predicate relationp relation)
(def-type-predicate entityp entity)
(def-type-predicate stringp string)
(def-type-predicate vectorp vector)
(def-type-predicate not nil)

(defun listp (v)
  "Returns t iff V is a list (i.e. a pair or nil)."
  (or (consp v) (not v)))

;;; Essential Utility Functions

(defun mapcar (f l)
  "Apply F to each element of list L, and return the results in a list."
  (if l
      (cons (funcall f (car l))
            (mapcar f (cdr l)))
      nil))

(defun memql (elt list)
  "Test if ELT is eql to any member of LIST.
See (describe 'eql)."
  (if (consp list)
      (if (eql elt (car list))
          list
          (memql elt (cdr list)))))

;;; Common Macros

(defmacro let (binds . body)
  "Generate a set of lexical bindings that cannot refer to one another,
and are accessible within the BODY code.
BINDS takes the form: ((name value) (name value) ...).
Alternatively, a member of BINDS can just be a symbol, to initialise it to nil."
  ((lambda (names values)
     (cons
      (cons 'lambda (cons names body))
      values))
   ;; Get the list of names.
   (mapcar (lambda (bind)
             (if (consp bind) (car bind) bind))
           binds)
   ;; Get the list of value forms.
   (mapcar (lambda (bind)
             (and (consp bind) (cadr bind)))
           binds)))

(defmacro let* (binds . body)
  "Generate a set of lexical bindings where each binding can refer to the previous ones.

See (describe 'let) for the meanings of BINDS and BODY."
  (if binds
      (list 'let (list (car binds))
            (cons 'let* (cons (cdr binds) body)))
      (cons 'progn body)))

(defmacro cond clauses
  "Evaluate the first expression in each clause until one is true,
then evaluate the rest of that clause in sequence."
  (if (consp clauses)
      (let ((current (car clauses)))
        ;; Build an if-else chain with *recursive macro-expansion*.
        (list 'if (car current)
              ;; Implicit progn
              (cons 'progn (cdr current))
              (cons 'cond (cdr clauses))))
      clauses))

(defun qq-list-form (list)
  "Determine whether quasiquoted expression LIST can be translated into a call to the list function.
Using list instead of cons makes the result of quasiquotation shorter."
  (or
   ;; Yes: base case, and we could represent this with the code (list).
   (not list)
   ;; Yes: Not an unquote form, and the cdr is also qq-list-form.
   (and (consp list) (not (eq (car list) 'unquote)) (qq-list-form (cdr list)))))

(defun quasiquote-rec (form level)
  "Expand a quasiquoted FORM at the given level of nested quasiquotation.
One backtick increases LEVEL by 1, one comma decreases it by 1.
Data at level 0 is inserted as code."
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
  "Apply 1 level of quasiquotation to FORM."
  (quasiquote-rec form 1))

(defmacro assert (spec)
  `(if (not ,spec)
       (wrong "ASSERTION FAILURE" ',spec)))

(defmacro incq (var . rest)
  "Increment VAR by 1, or the second argument if any."
  (assert (symbolp var))
  `(setq ,var (+ ,var ,(if (consp rest) (car rest) 1))))

;;; Less Essential Utilities (not needed by any common macros)

(defvar gensym-counter 0
  "Added to the name of each gensym so they all appear distinct.
Incremented upon each call to gensym.")
(defun gensym ()
  "Generate a new, uninterned symbol."
  (setq gensym-counter (+ gensym-counter 1))
  (make-symbol (concat "#g" (to-string gensym-counter))))

(defun max (a . as)
  "Obtain the maximum argument value."
  (let ((m a))
    (while (consp as)
      (let ((current (car as)))
        (if (> current m)
            (setq m current)))
      (setq as (cdr as)))
    m))

(defun float (x)
  "Convert integer X into an f32."
  (+ 0.0 x))

(defun negated (f)
  "Create a function that returns nil when F doesn't, and vice versa."
  (lambda (x)
    (not (funcall f x))))


;;; Control Flow macros

(defmacro unless (cond . body)
  "Evaluate BODY in sequence if COND evaluates to nil."
  `(if ,cond
       nil
       . ,body))

(defmacro when (cond . body)
  "Evaluate BODY in sequence if COND does not evaluate to nil."
  `(if ,cond
       ;; Implicit progn
       (progn . ,body)))

(defmacro case (expr . clauses)
  "Evaluate EXPR, then evaluate the first matching clause in CLAUSES.
A clause has the form ((forms...) body...).
A clause matches if the result of evaluating EXPR is eql to one of the forms in its car,
or if one of those forms is t or otherwise.

See (describe 'eql)."
  ;; Use a gensym to ensure EXPR is only evaluated once.
  (let ((gname (gensym)))
    `(let ((,gname ,expr))
       ;; case is a special case of cond.
       (cond . ,(mapcar
                 (lambda (clause)
                   ;; Treat t and otherwise as default clauses.
                   (if (memql (car clause) '(t otherwise))
                       (cons t (cdr clause))
                       ;; Use `eql' directly if there is only one item in the clause.
                       (cons
                        (if (not (cdar clause))
                            `(eql ,gname ',(caar clause))
                            `(memql ,gname ',(car clause)))
                        ;; Insert the body of the clause.
                        (cdr clause))))
                 clauses)))))

(defmacro dotimes (iterator . body)
  "Evaluate BODY a given number of times in sequence.
ITERATOR has the form (VAR N).
VAR is bound to each of 0, 1, ..., N-1 on the corresponding iteration."
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
      ;; Here, L is either the last cons cell or nil, so simply return it.
      l))

(defun nconc lists
  "Concatenate LISTS by modifying them. The last list is not modified."
  ;; Skip any nil lists at the start.
  (while (and lists (not (car lists)))
    (setq lists (cdr lists)))
  ;; If all lists were nil, return nil.
  (when (consp lists)
    (reduce
     (lambda (front next)
       ;; Add a pointer to the start of the next list at the end of the current one.
       (if next (setcdr (last front) next)))
     (car lists)
     (cdr lists))
    ;; We modified the lists, so we can now access the result via the first element.
    (car lists)))

(defun zip (a b)
  "Zip lists A and B together.

E.g. (zip '(1 2 3) '(a b c)) = '((1 . a) (2 . b) (3 . c))."
  (when (and (consp a) (consp b))
    (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))

(defun union (a b)
  "Compute the union of lists A & B.
Elements are compared with eql."
  ;; O(n^2) linked list traversal. Don't tell Marcin!
  (reduce
   (lambda (acc elem)
     (if (memql elem acc)
         acc
         (cons elem acc)))
   a
   b))

(defun reverse (l)
  "Reverse the list L."
  (reduce (lambda (acc elem) (cons elem acc)) nil l))

(defun prin1-list (file list)
  "Print a textual representation of LIST to FILE."
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

(defun prin1-to (file form)
  "Print a textual representation of FORM to FILE."
  (if (structp form)
      (prin1-struct-to file form)
      (case (type-of form)
        ((pair)
         (let ((rmacro (assoc (car form)
                              '((quote . #\')
                                (quasiquote . #\`)
                                (unquote . #\,)))))
           (if rmacro
               (progn
                 (fputc (cdr rmacro) file)
                 (prin1-to file (cadr form)))
               (prin1-list file form))))
        ((nil)
         (fputs "()" file))
        ((character)                    ; #\c escape
         (fputc #\# file)
         (fputc #\\ file)
         (fputc form file))
        ((entity)
         (fputs "#*entity" file)
         (prin1-to file (list (ecs-id form) (ecs-gen form))))
        ((relation)
         (fputs "#*relation" file)
         (prin1-to file (list (ecs-relation form) (ecs-target form))))
        ((vector)
         (let ((len (length form))
               (i 0))
           (fputs "(vector" file)
           (while (< i len)
             (fputc #\  file)
             (prin1-to file (aref form i))
             (incq i))
           (fputs ")" file)))
        (t
         (prin1-to* file form)))))

(defun prin1 (form)
  "Print a textual representation of FORM to stdout."
  (prin1-to stdout form))

(defun print (form)
  "Print a textual representation of FORM to stdout, surrounded by new-line characters."
  (fputs "
" stdout)
  (prin1 form)
  (fputs "
" stdout))

(defun equal (a b)
  "Return t if A and B have the same structure, traversing the sub-structure of `cons' cells.
Leaves are compared with eql."
  (if (and (consp a) (consp b))
      (and (equal (car a) (car b))
           (equal (cdr a) (cdr b)))
      (eql a b)))

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

(defmacro generate-primitive-docstrings entries
  "Generate doc strings for the given primitive functions.
ENTRIES is a list, with items of the form (name docstring . ?arguments).
The optional ?arguments form represents the arguments that the function should take."
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
                ;; Include the arguments in the doc string if applicable.
                ,(if (cddr entry)
                     (concat "Arguments: " (to-string (caddr entry)) ".
")
                     "")
                "Typespec: "
                (to-string (function ,(car entry))) ".")))
    entries)))

(generate-primitive-docstrings
 (* "Multiply a list of numbers.")
 (+ "Add a list of numbers.")
 (% "Compute X mod Y." (x y))
 (/ "Divide the first argument by each of the remaining arguments.
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
 (assoc "Returns first key-value pair in LIST whose car is eq to KEY, if any, else nil."
        (key list))
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
It is preferable to use macros or other language features over this function where possible."
       (form))
 (read-stream "Read one Lisp object from the supplied FILE." (file))
 (fopen "Open a file with the given r/w setting. See fopen(3)." (file r/w))
 (getc "See getc(3)." (file))
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
Allowed values of BUTTON: left, right." (button)))
