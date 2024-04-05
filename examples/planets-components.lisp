(defvar col-red (make-colour 230 41 55 255)) ; Raylib red
(defvar col-green (make-colour 0 255 0 255))
(defcomponent Species nil)
(defcomponent Wizard nil)
(defcomponent Human nil)

(ecs-add* (defcomponent Dwarf nil)
          (Colour = col-red)
          (Radius 15.0)
          (Mass 1.5))
(ecs-add* (defcomponent Elf nil)
          (Colour 0 0 255 255)          ; Can also use literal values.
          (Radius 10.0)
          (Mass 1.0))
(ecs-add* (defcomponent Goblin nil)
          (Colour = col-green)
          (Radius 5.0)
          (Mass 0.5))
