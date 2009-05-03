
(in-package :cipht/games/geometry-2d)

(declaim (inline half square ilerp))
(defun half (x) (/ x 2))
(defun square (x) (expt x 2))
(defun ilerp (x a b) (/ (- x a) (- b a)))

(defun sink (x v)
  (if (< (abs v) (abs x))
      (- x (* (signum x) (signum v) v))
      0))
(define-modify-macro sinkf (amount) sink)

;;; (atan 0 0) is an error under CCL, though not SBCL
(defun mouse-arc (mx my r)
  (let ((u (/ my r)) (v (/ mx r))) (mod (atan u (if (= 0 u v) 1 v)) +2pi+)))
(defun sum-of-squares (&rest xs) (reduce #'+ xs :key #'square))

