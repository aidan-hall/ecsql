(defun converge (centre)
  "Make all moving Entities start moving towards CENTRE."
 (ecsql (and Pos Vel) (pos vel)
        (set-v2 vel
                (- (v2-x centre) (v2-x pos))
                (- (v2-y centre) (v2-y pos)))))

(defmacro recolour-species (species new-colour)
  "Re-colour all Entities with Species SPECIES to NEW-COLOUR."
  `(ecsql (and (has ,species) Colour) (colour)
          (copy-colour colour ,new-colour)))

;;; Make all entities of a given species stop moving
(defmacro halt-species (species)
  `(ecsql (and Vel (has ,species)) (vel)
          (set-v2 vel 0.0 0.0)))

;; Interactive versions
;; Make Entities move towards (200, 200)
(ecsql (and Pos Vel)
       (pos vel)
       (set-v2 vel
               (- 200 (v2-x pos))
               (- 200 (v2-y pos))))
;; Colour all Elves and Dwarves yellow
(ecsql (and Colour
            (has (or Elf Dwarf)))
       (colour)
       (set-colour colour
                   255 255 0 255))
;; Move Entities with Pos and Vel every frame
(defvar move-system
  (ecs-new-system
   (Physics)
   (and Pos Vel)
   (pos vel)
   (let ((delta (get-delta)))
     (set-v2 pos
             (+ (v2-x pos) (* (v2-x vel) delta))
             (+ (v2-y pos) (* (v2-y vel) delta))))))
