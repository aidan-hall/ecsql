(mapcar
 (function macroexpand-1)
 '((ecs-add* entity Pos)
   (ecs-add* entity (Vel 12. 34.))
   (ecs-add* entity (rel Species Wizard))
   (ecs-add* entity (Species . Wizard))
   (ecs-add* entity ((rel Bounce Wizard) 23.))
   (ecs-add* entity ((Bounce . Wizard) 23.))))
