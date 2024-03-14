(load "examples/planets-components.lisp")

;; Create an Entity and poke it about

(load "examples/planets-scene.lisp")

;; Make all moving Entities move towards (200,200)
(ecsql (and Pos Vel)
       (pos vel)
       (set-v2 vel
               (- 200 (v2-x pos))
               (- 200 (v2-y pos))))

;; Colour all Elves and Dwarves yellow
(ecsql (and Colour
            (with (or (rel Species Dwarf)
                      (rel Species Elf))))
       (colour)
       (set-colour colour
                   255 255 0 255))

;; See the definition of the Move system in main.c
(ecs-lookup 'Move)
(ecs-destroy (ecs-lookup 'Move))

(defvar move-system
  (ecs-new-system
   (Physics
    (name Move))
   (and Pos Vel)
   (pos vel)
   (let ((delta (get-delta)))
     (set-v2 pos
             (+ (v2-x pos) (* (v2-x vel) delta))
             (+ (v2-y pos) (* (v2-y vel) delta))))))


;; Start up some of the systems in examples/systems.lisp and show them off

