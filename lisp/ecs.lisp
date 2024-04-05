(defun ecs-resolve (thing)
  "Attempt to resolve THING into an Entity or Relation as follows:

  Entity or Relation  → THING,
  i32                 → Live Entity with that ID, if any,
  symbol              → Live Entity with that name, if any,
  (rel X Y) | (X . Y) → Relation with ecs-relation value X and ecs-target Y."
  (case (type-of thing)
    ((relation entity) thing)
    ((i32) (ecs-entity thing))
    ((symbol) (ecs-lookup thing))
    ((pair)
     (case (car thing)
       ((rel)
        ;; Relation: (rel Entity1 Entity2)
        (assert (>= (length thing) 3))
        ;; Treat this the same as (Entity1 . Entity2)
        (ecs-resolve (cons (cadr thing) (caddr thing))))
       (t
        ;; Relation: (Entity . Entity)
        (let ((first (ecs-resolve (car thing)))
              (second (ecs-resolve (cdr thing))))
          (if (and (eq (type-of first) 'entity)
                   (eq (type-of second) 'entity))
              (ecs-pair first second)
              (wrong "Failed to resolve list form to Entity or Relation" thing))))))
    (t (wrong "Unable to resolve to an Entity or Relation" thing))))

(defmacro ecs-add* (entity . components)
  "For each form in COMPONENTS, generate code to add an appropriate Component, possibly
initialised, to the supplied ENTITY.

Each form of components may take one of the following forms, with the following effects:
  symbol or (symbol) → Adds (ecs-lookup symbol).
  (name symbol) → (ecs-set-name entity symbol).
  (rel *1 *2) → Adds Relation (*1 . *2), after evaluating *1 and *2
  (* . *) → Adds (ecs-resolve form), almost certainly a Relation.
  (* ...) → Adds the Component specified in the car (derived with ecs-resolve),
            and initialises it using the remaining arguments.
            If the Component is stored as a struct, the constructor is called.
  (* = *) → Adds the Component specified in the car,
            and assigns it the value of the expression after the = sign.
  (* : *) → Adds the Component specified in the car,
            and copies the value of that Component from the Entity after the = sign.

Example:
(ecs-add*
 entity
 (Pos 12. 13.) (Bounce : Dwarf) Vel
 (Colour = red) (Species . Wizard))"

  ;; Use a gensym to ensure the entity argument is only evaluated once.
  ;; A common pattern is (ecs-add* (ecs-new) ...), so this is essential.
  (let ((esym (gensym)))
    `(let ((,esym ,entity))
       (progn
         . ,(mapcar
             (lambda (component-form)
               (if (or (not (eq (type-of component-form) 'pair))
                       ;; (Component): Treat this the same as a plain Component by unwrapping it.
                       (and (not (cdr component-form))
                            (setq component-form (car component-form))
                            component-form))
                   ;; If there's no sub-structure, defer to ecs-resolve.
                   `(ecs-add ,esym ',(ecs-resolve component-form))

                   (case (car component-form)
                     ((name)
                      ;; Set the name of an Entity.
                      `(ecs-set-name ,esym ,(cadr component-form)))
                     ((rel)
                      ;; Add a Relation, evaluating the pair members & resolving at runtime.
                      `(ecs-add ,esym (ecs-resolve (cons ,(cadr component-form)
                                                         ,(caddr component-form)))))
                     ;; Add a Component.
                     (t (if (not (consp (cdr component-form)))
                            `(ecs-add ,esym
                                      ;; component-form is a cons pair with a non-pair cdr:
                                      ;; (something . something)
                                      ;; attempt to resolve it (as a Relation).
                                      ',(ecs-resolve component-form))
                            (let ((component
                                   ;; Resolve the Component to add.
                                   (ecs-resolve (car component-form))))
                              `(progn
                                 ;; Add the Component.
                                 (ecs-add ,esym ',component)
                                 ;; Additional elements in the list hold initial values for Components.
                                 ,(let ((rest (cdr component-form)))
                                    ;; We know `rest' is a pair, so we can safely take its car.
                                    (let ((type (ecs-storage-type component)))
                                      `(ecs-set ,esym ',component
                                                ;; Produce the initial value for the Component.
                                                ,(case (car rest)
                                                   ;; '= means the cadr is an expression that will
                                                   ;; evaluate to the Component value.
                                                   ((=) (cadr rest))
                                                   ;; ': means inherit from the specified Entity.
                                                   ((:) `(ecs-get ,(cadr rest) ',component))
                                                   (t
                                                    (if (struct-metadata type)
                                                        ;; If the Component is represented by a struct,
                                                        ;; use its constructor.
                                                        (cons (intern
                                                               (concat "make-"
                                                                       (symbol-name type)))
                                                              (cdr component-form))
                                                        (case type
                                                          ((vector)
                                                           ;; Store the cdr elements in a vector.
                                                           (cons 'vector (cdr component-form)))
                                                          (t
                                                           ;; Other types just need the one value.
                                                           (car rest))))))))))))))))
             components))
       ,esym)))

(defmacro defcomponent (name type)
  "Define an Entity with the given NAME and LispStorage TYPE.
Also define a variable bound to that Entity."
  `(if (ecs-lookup ',name)
       (wrong "An entity with the given name already exists" ',name)
       (let ((comp (ecs-new-component ',type)))
         ;; Attempt to set the name inside the ECS first, since this could fail.
         (if (ecs-set-name comp ',name)
             (defvar ,name comp
               ,(if type
                    (concat "A Component. Stores data of type " (to-string type) ".")
                    "A tag Component. Does not store any data."))
             (puts "Couldn't set entity name. Cleaning up....")
             ;; Clean up the Entity if we failed to assign the name.
             (ecs-destroy comp)))))

(defcomponent Pos v2)
(defcomponent Vel v2)
(defcomponent RelPos v2)
(defcomponent Parent entity)
(defcomponent Orientation v3)
(defcomponent Health i32)
(defcomponent Mass f32)
(defcomponent Bounce f32)
(defcomponent Radius f32)
(defcomponent Physics nil)
(defcomponent Graphics nil)
(defstruct colour
  (r i32)
  (g i32)
  (b i32)
  (a i32))
(defcomponent Colour colour)

(let ((delta (/ 1.0 120)))
  (defun move-system (e)
    (let ((pos (ecs-get e Pos)) (vel (ecs-get e Vel)))
      (set-v2-x pos (+ (v2-x pos) (* (v2-x vel) delta)))
      (set-v2-y pos (+ (v2-y pos) (* (v2-y vel) delta))))))
