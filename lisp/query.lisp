(defun translate-predicate (predicate)
  "Translate PREDICATE from user-facing to internal query representation.
PREDICATE takes the form described in (describe 'ecsql).

Returns a pair: (BINDINGS . CONDITION),
where BINDINGS is the list of Components whose values are automatically bound in Lisp queries
and CONDITION is the predicate that must satisfy src/ecs/query.c:ecs_query_matches
for a System to run on a given Archetype.

Most values in BINDINGS will be single Components, with the following exceptions:
• (opt COMPONENT ...): Each COMPONENT is bound if an Entity has it, otherwise nil is used.
• (or COMPONENT ...): The first of these Components that an Entity has will be bound."
  (case (type-of predicate)
    ((nil) (cons nil nil))
    ((pair)
     (case (car predicate)
       ((not)
        (cons nil
              ;; Discard the 'and at the front of the translated query structure
              ;; and replace it with 'not.
              (cons 'not (cddr
                          ;; Treat a not as "none of the supplied arguments"
                          (translate-predicate
                           (cons 'and (cdr predicate)))))))
       ((and)
        (let ((children
               (mapcar (function translate-predicate)
                       (cdr predicate))))
          (cons
           (reduce
            (function union)
            nil
            (mapcar (function car) children))
           (cons 'and (mapcar (function cdr) children)))))
       ((or)
        ;; Assume a single level of Components in a binding or expression.
        ;; Supporting arbitrary sub-structure would be extremely complex and inefficient,
        ;; and would deliver little value.
        (let ((children
               (mapcar #'translate-predicate
                       (cdr predicate))))
          (cons
           (list (cons 'or (apply #'nconc (mapcar #'car children))))
           (cons 'or (mapcar #'cdr children)))))
       ;; Make a Component bound but not required.
       ((opt)
        (let ((children (mapcar #'translate-predicate (cdr predicate))))
          (cons (mapcar (lambda (child) (cons 'opt (car child))) children)
                ;; An opt clause does not require matches.
                ;; (and) is a no-op predicate.
                '(and))))
       ;; Allow Components to be required but not fetched.
       ((with has)
        (cons nil (cdr (translate-predicate (cadr predicate)))))
       ((rel)
        (let ((component (ecs-pair (ecs-resolve (cadr predicate))
                                   (ecs-resolve (caddr predicate)))))
          (cons (list component) component)))
       (t
        (wrong "Invalid predicate form" predicate))))
    ;; Resolve Component names
    ((symbol i32)
     (let ((component (ecs-resolve predicate)))
       (if component
           (cons (list component) component)
           (wrong "Nonexistent Component name" predicate))))
    ((entity relation)
     (cons (list predicate) predicate))
    ((t) (wrong "Invalid form of query" predicate))))
(defun reverse (l)
  "Reverse the list L."
  (reduce (lambda (acc elem) (cons elem acc)) nil l))
(defun fixup-predicate (predicate)
  (let ((res (translate-predicate predicate)))
    (cons (reverse (car res)) (cdr res))))

(defmacro select predicate
  "Convert PREDICATE to a form usable by the Query engine.
Run (describe 'ecsql) for detail on the form of PREDICATE."
  ;; Implicit and form at top level.
  (let ((res (fixup-predicate (cons 'and predicate))))
    `',res))

(defun create-system-function (names body)
  "Generate a function with BODY, and (entity . NAMES) as the parameter list."
  `(lambda (entity . ,names)
     . ,body))

(defmacro ecsql (predicate names . body)
  "Evaluate BODY for each Entity that matches PREDICATE,
with NAMES bound to the values of Components in PREDICATE.

PREDICATE is an expression in the following grammar:

PREDICATE ::= (not PREDICATE)
                → Predicate must not match.
           |  (and PREDICATES...)
                → All predicates must match.
           |  (or PREDICATES...)
                → At least one predicate must match.
           |  (opt PREDICATE)
                → Bind Components in predicate, but they do not have to match.
           |  (with PREDICATE) | (has PREDICATE)
                → Predicate must match, but does not bind Components.
           |  (rel ENTITY ENTITY)
                → Matches Entities that have given Relation. Binds it.
           |  ENTITY
                → Matches Entities with the given Component. Binds it.

ENTITY expressions are evaluated with ecs-resolve.

The value of each Component in the PREDICATE that has storage is bound
to a name in NAMES, in the order they appear, when BODY is evaluated.
Components inside a (with PREDICATE) form must match, but do not get bound.

E.g. Make all Entities with Vel and Colour stop moving:
(ecsql (and (with Colour) Vel) (vel) (set-v2 vel 0. 0.))"
  (let* ((query (fixup-predicate predicate))
         (code (create-system-function names body)))
    `(ecs-do-query ',query ,code)))

(defmacro ecs-new-system (components predicate names . body)
  "Produces a System with the supplied COMPONENTS.
The effect of the System is equivalent to calling ecsql with the remaining arguments periodically,
typically every frame.

The COMPONENTS are added using ecs-add*, so any forms that work there work here.

E.g. A system to move Entities with Pos and Vel:
(defvar move-system
  (ecs-new-system
   (Physics)
   (and Pos Vel)
   (pos vel)
   (let ((delta (get-delta)))
     (set-v2 pos
             (+ (v2-x pos) (* (v2-x vel) delta))
             (+ (v2-y pos) (* (v2-y vel) delta))))))"
  (let* ((query (fixup-predicate predicate))
         (code (create-system-function names body)))
    `(ecs-add*
      (ecs-register-system ',query ,code)
      . ,components)))

;;; Example:
;;; (select Pos Vel) → ((vector Pos Vel) . (and Pos Vel))
