(defvar wizard-follow
  (ecs-new-system
   (Physics
    (name 'WizardFollow))
   (and (with (rel Species Wizard)) Pos) (pos)
   (set-v2 pos (+ (get-mouse-x) 30.0) (get-mouse-y))))
(defvar human-follow
  (ecs-new-system
   (Physics) (and (with (rel Species Human)) Pos Vel) (pos vel)
   (set-v2 pos (- (get-mouse-x) 30.0) (get-mouse-y))
   (set-v2 vel 0. 0.)))
(defvar pos-label
  (ecs-new-system
   (Graphics) (and Pos Radius (with Colour) (with Vel)) (pos radius)
   (draw-text (to-string (list (floor (v2-x pos)) (floor (v2-y pos))))
              (+ (v2-x pos) radius)
              (- (v2-y pos) radius) 8)))

(defun point-in-circle (centre radius point)
  (let* ((radius2 (* radius radius))
         (diff-x (- (v2-x centre) (v2-x point)))
         (diff-x2 (* diff-x diff-x))
         (diff-y (- (v2-y centre) (v2-y point)))
         (diff-y2 (* diff-y diff-y)))
    (and (< diff-x2 radius2) (< diff-y2 radius2))))

(defvar clicked-me
  (ecs-new-system
   (Graphics) (and Pos Radius (with Colour) (with Vel)) (pos radius)
   (when (and (is-mouse-down 'left)
              (point-in-circle pos radius (make-v2 (get-mouse-x) (get-mouse-y))))
     (draw-text (concat "Clicked: " (to-string (ecs-id entity)))
                10. 10. 20))))

(defvar kill-click
  (ecs-new-system
   (Physics) (and Pos Radius (with Colour) (with Vel)) (pos radius)
   (when (and (is-mouse-pressed 'right)
              (point-in-circle pos radius (make-v2 (get-mouse-x) (get-mouse-y))))
     (ecs-destroy entity))))
