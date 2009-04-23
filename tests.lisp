
(defpackage :cipht/games/geometry-2d-tests
  (:use #:cl #:rt))

(in-package :cipht/games/geometry-2d-tests)

;;;; CONSTANTS

(deftest constants.0
    (<= (* 2 3.14159) (- geom:+2pi+ short-float-epsilon) (+ geom:+2pi+ short-float-epsilon) (* 2 3.1416))
  t)

;;;; MATH

(deftest sinkf.0
    (let ((x 2))
      (geom:sinkf x 1.1)
      (geom:sinkf x -1.1)
      x)
  0)

(deftest sinkf.1
    (let ((x 3))
      (geom:sinkf x -1)
      (geom:sinkf x 1)
      x)
  1)

(deftest sinkf.2
    (let ((x 3))
      (geom:sinkf x 2)
      x)
  1)

;;;; POINTS

(deftest point.0
    (let ((p (geom:point 100 200)))
      (values (geom:x-of p) (geom:y-of p)))
  100 200)

(deftest point.1
    (handler-case
	(geom:point nil 0)
      (type-error () t))
  t)

(deftest translate-point.0
    (let ((p (geom:point 42 10))
	  (q (geom:point 10 10)))
      (let ((r (geom:translate-point p q)))
	(values (geom:x-of p) (geom:y-of p)
		(geom:x-of q) (geom:y-of q)
		(geom:x-of r) (geom:y-of r))))
  42 10
  10 10
  52 20)

(deftest translate-point!.0
    (let ((p (geom:point 42 10))
	  (q (geom:point 10 10)))
      (let ((r (geom:translate-point! p q)))
	(values (geom:x-of p) (geom:y-of p)
		(geom:x-of q) (geom:y-of q)
		(geom:x-of r) (geom:y-of r)
		(eq p r))))
  52 20
  10 10
  52 20
  t)

(deftest translate-point*.0
    (let ((p (geom:point 42 10)))
      (let ((r (geom:translate-point* p :x 10 :y 10)))
	(values (geom:x-of p) (geom:y-of p)
		(geom:x-of r) (geom:y-of r))))
  52 20
  52 20)

(deftest point<-point.0
    (let ((p (geom:point 42 10))
	  (q (geom:point 10 20)))
      (let ((r (geom:point<-point! p q)))
	(values (geom:x-of p) (geom:y-of p)
		(geom:x-of q) (geom:y-of q)
		(geom:x-of r) (geom:y-of r)
		(eq p r) (eq p q))))
  10 20
  10 20
  10 20
  t nil)

(deftest untranslate-point.0
    (let ((p (geom:point 42 10))
	  (q (geom:point 10 10)))
      (let ((r (geom:untranslate-point p q)))
	(values (geom:x-of p) (geom:y-of p)
		(geom:x-of q) (geom:y-of q)
		(geom:x-of r) (geom:y-of r))))
  42 10
  10 10
  32 0)


;;;; BOXES

(deftest box.0
    (let ((b (geom:box 1 2 3 4)))
      (values (geom:x-of (geom:bl-corner b)) (geom:y-of (geom:bl-corner b))
	      (geom:x-of (geom:ur-corner b)) (geom:y-of (geom:ur-corner b))))
  1 2 3 4)

(deftest box.1
    (let ((b (geom:box 1 2 3 4 t)))
      (values (geom:x-of (geom:bl-corner b)) (geom:y-of (geom:bl-corner b))
	      (geom:x-of (geom:ur-corner b)) (geom:y-of (geom:ur-corner b))))
  1 2 4 6)

(deftest box.2
    (let ((b (geom:box 1 2 3 4 nil)))
      (values (geom:x-of (geom:bl-corner b)) (geom:y-of (geom:bl-corner b))
	      (geom:x-of (geom:ur-corner b)) (geom:y-of (geom:ur-corner b))))
  1 2 3 4)

(deftest box.3
    (let ((b (geom:box (geom:point 1 2) (geom:point 3 4))))
      (values (geom:x-of (geom:bl-corner b)) (geom:y-of (geom:bl-corner b))
	      (geom:x-of (geom:ur-corner b)) (geom:y-of (geom:ur-corner b))))
  1 2 3 4)

(deftest box.4
    (let ((b (geom:box (geom:point 3 4) (geom:point 1 2))))
      (values (geom:x-of (geom:bl-corner b)) (geom:y-of (geom:bl-corner b))
	      (geom:x-of (geom:ur-corner b)) (geom:y-of (geom:ur-corner b))))
  1 2 3 4)

(deftest box.5
    (let ((b (geom:box 3 4 1 2)))
      (values (geom:x-of (geom:bl-corner b)) (geom:y-of (geom:bl-corner b))
	      (geom:x-of (geom:ur-corner b)) (geom:y-of (geom:ur-corner b))))
  1 2 3 4)

(deftest box-width.0
    (let ((b (geom:box 1 2 3 4))
	  (c (geom:box 1 2 3 4 t)))
      (values (geom:box-width b) (geom:box-width c)))
  2 3)

(deftest box-height.0
    (let ((b (geom:box 1 2 3 4))
	  (c (geom:box 1 2 3 4 t)))
      (values (geom:box-height b) (geom:box-height c)))
  2 4)
