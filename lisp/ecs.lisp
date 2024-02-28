(defmacro defcomponent (name type)
  `(if (ecs-lookup ',name)
       (wrong "An entity with the given name already exists" ',name)
       (let ((comp (ecs-new-component ',type)))
         ;; Attempt to set the name inside the ECS first, since this could fail.
         (if (ecs-set-name comp ',name)
             (defvar ,name comp)
             (puts "Couldn't set entity name. Cleaning up....")
             ;; Clean up the Entity if we failed to assign the name.
             (ecs-destroy comp)))))

(defcomponent Pos v2)
(defcomponent Vel v2)
(defcomponent Orientation v3)
(defcomponent Health i32)
(defcomponent Mass f32)
(defcomponent Bounce nil)
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
