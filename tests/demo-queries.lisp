(defun converge (centre)
  "Make all moving Entities start moving towards CENTRE."
 (ecsql (and Pos Vel) (pos vel)
        (set-v2 vel
                (- (v2-x centre) (v2-x pos))
                (- (v2-y centre) (v2-y pos)))))

(defmacro recolour-species (species new-colour)
  "Re-colour all Entities with Species SPECIES to NEW-COLOUR."
  `(ecsql (and (with (rel Species ,species)) Colour) (colour)
          (copy-colour colour ,new-colour)))

;;; Make all entities of a given species stop moving
(defmacro halt-species (species)
  `(ecsql (and Vel (with (rel Species ,species))) (vel)
          (set-v2 vel 0.0 0.0)))

;; Interactive versions
;; Make all moving Entities converge towards (200, 200)
(ecsql (and Pos Vel)
       (pos vel)
       (set-v2 vel
               (- 200 (v2-x pos))
               (- 200 (v2-y pos))))

;; Change the colour of all Elves and Dwarves to yellow
(ecsql (and Colour
            (with (or (rel Species Dwarf)
                      (rel Species Elf))))
       (colour)
       (set-colour colour
                   255 255 0 255))
