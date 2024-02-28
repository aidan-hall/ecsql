;;; Make all points converge towards a centre point
(defun converge (centre)
 (ecsql (and Pos Vel) (pos vel)
        (set-v2 vel (- (v2-x centre) (v2-x pos)) (- (v2-y centre) (v2-y pos)))))
;;; Make all entities of a given species stop moving
(defmacro halt-species (species)
  `(ecsql (and Vel (with (rel Species ,species))) (vel)
          (set-v2 vel 0.0 0.0)))

;;; Change the colour of a species
(defmacro recolour-species (species new-colour)
  `(ecsql (and (with (rel Species ,species)) Colour) (colour)
          (copy-colour colour ,new-colour)))

