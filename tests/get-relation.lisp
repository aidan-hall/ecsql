(defvar p (ecs-lookup 'player))
(defvar e (ecs-lookup 'Eats))
(defvar a (ecs-lookup 'Apple))
(defvar ea (ecs-pair e a))
(assert (ecs-has p ea))
(assert (eq (type-of (ecs-get p ea)) 'i32))
(assert (eql (ecs-get p ea) -4))
(ecs-set p ea -3)
(assert (eql (ecs-get p ea) -3))
