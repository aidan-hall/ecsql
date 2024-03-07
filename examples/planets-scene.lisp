(defvar raylib-orange (make-colour 255 161 0 255))
(ecs-add* (ecs-new)
          (name 'Gandalf)
          (Species . Wizard)
          (Colour = raylib-orange)
          (Radius 20.0)
          (Pos 100. 100.)
          (Bounce 0.5)
          Vel)

(let ((species-vec (vector Dwarf Elf Goblin)))
  (dotimes (i 100)
    (let ((species (aref species-vec (% i (length species-vec)))))
      (ecs-add* (ecs-new)
                (Pos (+ 0.(% (* 20 i) 1280)) (+ 0. (% (* 30 i) 720)))
                (Vel (* 20. (+ 1 i)) (* 15. (+ 1 i)))
                (Bounce 0.8)
                (Radius : species)
                (Colour : species)
                (rel 'Species species)))))
