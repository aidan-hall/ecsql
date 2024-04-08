(defvar raylib-orange (make-colour 255 161 0 255))
(ecs-add* (ecs-new)
          (name 'Gandalf)
          Wizard
          (Colour = raylib-orange)
          (Radius 20.0)
          (Pos 100. 100.)
          (Bounce 0.5)
          (Mass 2.0)
          Vel)

(let ((species-vec (vector Dwarf Elf Goblin)))
  (dotimes (i 100)
    (let ((species (aref species-vec (% i (length species-vec))))
          (screen-width (get-screen-width))
          (screen-height (get-screen-height)))
      (ecs-add
       (ecs-add* (ecs-new)
                 (Pos (+ 0.(% (* 20 i) screen-width))
                      (+ 0. (% (* 30 i) screen-height)))
                 (Vel (* 20. (+ 1 i)) (* 15. (+ 1 i)))
                 (Bounce 0.8)
                 (Radius : species)
                 (Colour : species)
                 (Mass : species))
       species))))

;;; A child Entity
(defun v2-add (a b)
  (make-v2 (+ (v2-x a) (v2-x b))
           (+ (v2-y a) (v2-y b))))
(defun hierarchy-pos (e)
    (if (and (ecs-has e Parent) (ecs-has e RelPos))
        (let ((pos (ecs-get e RelPos)))
          (v2-add pos (hierarchy-pos (ecs-get e Parent))))
      (ecs-get e Pos)))
(defvar pos-from-relpos-system
    (ecs-new-system
     (Physics (name 'Pos2RelPos))
     (and Pos (has RelPos))
     (abs)
     (copy-v2 abs (hierarchy-pos entity))))

(ecs-add* (ecs-new)
          (Parent = (ecs-resolve 'Gandalf))
          Pos
          (RelPos 20. 20.)
          (Radius 5.))
