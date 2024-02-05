(defstruct v3
  (x f32)
  (y f32)
  (z f32))
(defvar av3 (make-v3 1. 2. 3.))

(defstruct player
  (pos v3)
  (vel v3)
  (health i32))

(defvar p (make-player (make-v3 0. 0. 0.) av3 10))
(puts "type of p: ")
(print (type-of p))
(puts "p: ")
(print-player-to stdout p)
