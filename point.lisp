(in-package :cipht/games/geometry-2d)

(deftype point () '(complex single-float))
(declaim (inline point copy-point x-of y-of))
(defun point (x y) (the point (complex (float x) (float y))))
(defun x-of (p) (declare (type point p)) (the single-float (realpart p)))
(defun y-of (p) (declare (type point p)) (the single-float (imagpart p)))
(defun copy-point (p) (declare (type point p)) (complex (x-of p) (y-of p)))

(declaim (inline on-point point+))
(defun on-point (op p q)
  (declare (type point p q))
  (point (funcall op (x-of p) (x-of q)) (funcall op (y-of p) (y-of q))))
(defun point+ (p q) (declare (type point p q)) (point (+ (x-of p) (x-of q)) (+ (y-of p) (y-of q))))
(define-compiler-macro point+ (&whole form p q)
  (cond ((and (listp q) (eql (first q) 'point))
	 `(point (+ (x-of ,p) ,(second q)) (+ (y-of ,p) ,(third q))))
	((and (listp p) (eql (first p) 'point))
	 `(point (+ (x-of ,q) ,(second p)) (+ (y-of ,q) ,(third p))))
	(t form)))
