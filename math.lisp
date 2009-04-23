
(in-package :cipht/games/geometry-2d)

(defun half (x) (/ x 2))

(defun sink (x v)
  (if (< (abs v) (abs x))
      (- x (* (signum x) (signum v) v))
      0))
(define-modify-macro sinkf (amount) sink)

