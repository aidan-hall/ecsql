(defmacro defcomponent (name type)
  `(if (ecs-get-by-name ',name)
       (wrong "An entity with the given name already exists" ',name)
     (let ((comp (ecs-new-component ',type)))
       ;; Attempt to set the name inside the ECS first, since this could fail.
       (if (ecs-set-name comp ',name)
           (defvar ,name comp)
         (puts "Couldn't set entity name. Cleaning up....")
         ;; Clean up the Entity if we failed to assign the name.
         (ecs-destroy comp)))))

(defcomponent Pos v3)
(defcomponent Vel v3)
(defcomponent Orientation v3)
(defcomponent Health i32)
(defcomponent Mass f32)
