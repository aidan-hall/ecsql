;; Define some Components for demo.
(load "examples/planets-components.lisp")




;; Create an Entity and poke it about
(defvar e
  (ecs-add* (ecs-new)
            (Pos 200.0 200.0)
            (Vel 100. 200.)
            (Radius 10.0)
            (Colour 255 0 0 255)))


(ecs-add* e (Bounce 0.8))



;; Load a complex pre-defined scene.
(load "examples/planets-scene.lisp")



;; Make all moving Entities move towards (200,200)
(ecsql (and Pos Vel)
       (pos vel)
       (set-v2 vel
               (- 200 (v2-x pos))
               (- 200 (v2-y pos))))



;; Colour all Elves and Dwarves yellow
(ecsql (and Colour
            (has (or Dwarf Elf)))
       (colour)
       (set-colour colour
                   255 255 0 255))



;; Define the same System in C and Lisp
(ecs-resolve 'Move)
(ecs-destroy (ecs-resolve 'Move))


(defvar move-system
  (ecs-new-system
   (Physics (name 'Move))
   (and Pos Vel)
   (pos vel)
   (let ((delta (get-delta)))
     (set-v2 pos
             (+ (v2-x pos) (* (v2-x vel) delta))
             (+ (v2-y pos) (* (v2-y vel) delta))))))


;; More Lisp Systems

(defvar wizard-follow
  (ecs-new-system
   (Physics (name 'WizardFollow))
   (and (has Wizard) Pos) (pos)
   (set-v2 pos
           (+ (get-mouse-x) 30.0)
           (float (get-mouse-y)))))


(defvar pos-label
  (ecs-new-system
   (Graphics)
   (and Pos Radius (has Colour) (has Vel))
   (pos radius)
   (draw-text (to-string
               (list (floor (v2-x pos))
                     (floor (v2-y pos))))
              (+ (v2-x pos) radius)
              (- (v2-y pos) radius) 8)))


(defun point-in-circle (centre radius point)
  "Test whether POINT is inside a circle."
  (let* ((radius2 (* radius radius))
         (diff-x (- (v2-x centre) (v2-x point)))
         (diff-x2 (* diff-x diff-x))
         (diff-y (- (v2-y centre) (v2-y point)))
         (diff-y2 (* diff-y diff-y)))
    (and (< diff-x2 radius2) (< diff-y2 radius2))))


(defvar clicked-me
  (ecs-new-system
   (Graphics)
   (and Pos Radius (has Colour) (has Vel))
   (pos radius)
   (when (and (is-mouse-down 'left)
              (point-in-circle
               pos radius
               (make-v2 (float (get-mouse-x))
                        (float (get-mouse-y)))))
     (draw-text (concat "Clicked: "
                        (to-string (ecs-id entity)))
                10. 10. 20))))

(defvar kill-click
  (ecs-new-system
   (Physics)
   (and Pos Radius (has Colour) (has Vel)) (pos radius)
   (when (and (is-mouse-pressed 'right)
              (point-in-circle
               pos radius
               (make-v2 (float (get-mouse-x))
                        (float (get-mouse-y)))))
     (ecs-destroy entity))))
