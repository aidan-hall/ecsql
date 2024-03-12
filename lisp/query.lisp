(defun translate-predicate (predicate)
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
       ((and or)
        (let ((children
               (mapcar (function translate-predicate)
                       (cdr predicate))))
          (cons
           (reduce
            (function union)
            nil
            (mapcar (function car) children))
           (cons (car predicate) (mapcar (function cdr) children)))))
       ;; Make a Component bound but not required.
       ;; TODO: Make this work.
       ((opt)
        (cons (car (translate-predicate (cadr predicate))) '(and)))
       ;; Allow Components to be required but not fetched.
       ((with)
        (cons nil (cdr (translate-predicate (cadr predicate)))))
       ((rel)
        (let ((component (ecs-pair (ecs-lookup (cadr predicate))
                                   (ecs-lookup (caddr predicate)))))
          (cons (list component) component)))
       (t
        (wrong "Invalid predicate form" predicate))))
    ;; Resolve Component names
    ((symbol)
     (let ((component (ecs-lookup predicate)))
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
See (describe (macro ecsql)) for detail on the form of PREDICATE."
  ;; Implicit and form at top level.
  (let ((res (fixup-predicate (cons 'and predicate))))
    `',res))

(defun create-system-function (names body components)
  "Generate a function that evaluates BODY on an Entity, with the given COMPONENTS bound to NAMES."
  `(lambda (entity)
     ((lambda ,names
        . ,body)
      . ,(mapcar (lambda (component)
                   `(ecs-get entity ',component))
                 components))))

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
           |  (with PREDICATE)
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
         (code (create-system-function names body (car query))))
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
         (code (create-system-function names body (car query))))
    `(ecs-add*
      (ecs-register-system ',query ,code)
      . ,components)))

;;; Example:
;;; (select Pos Vel) → ((vector Pos Vel) . (and Pos Vel))
