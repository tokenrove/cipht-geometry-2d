(in-package :cipht/games/geometry-2d)

(defun point (x y)
  (declare (real x y))
  (make-array 2 :element-type 'real :initial-contents (list x y)))
(defun x-of (v) (aref v 0))
(defun (setf x-of) (x p) (setf (aref p 0) x))
(defun y-of (v) (aref v 1))
(defun (setf y-of) (y p) (setf (aref p 1) y))

(defun copy-point (p) (copy-array p))

(defmacro define-symmetric-geometric-fn (name type &rest attributes)
  "Constructs the destructive function NAME!, the spread-argument
variant NAME*, and the copying version NAME."
  `(progn
     (defun ,(symbolicate name '!) (p q)
       (setf ,@(loop for (slot op) in attributes
		  for accessor = (symbolicate slot '-OF)
		  collect `(,accessor p)
		  collect `(,op (,accessor p) (,accessor q))))
       p)
     (defun ,(symbolicate name '*) (p &key ,@(loop for (slot nil default) in attributes
						collect `(,slot ,default)))
       (setf ,@(loop for (slot op) in attributes
		  for accessor = (symbolicate slot '-OF)
		  collect `(,accessor p)
		  collect `(,op (,accessor p) ,slot)))
       p)
     (defun ,name (p q)
       (,(symbolicate name '!) (,(symbolicate 'copy- type) p) q))))

(define-symmetric-geometric-fn translate-point point (x + 0) (y + 0))
(define-symmetric-geometric-fn untranslate-point point (x - 0) (y - 0))
(define-symmetric-geometric-fn point<-point point (x progn 0) (y progn 0))
(define-symmetric-geometric-fn scale-point point (x * 1) (y * 1))

(defun point<-complex (c) (point (realpart c) (imagpart c)))

(defmacro apply-point (point &body body)
  (let (prefix)
    (when (listp point)
      (setf prefix (first point) point (second point)))
    (once-only ((point point))
      (let ((x (if prefix (symbolicate prefix '-x) 'x))
	    (y (if prefix (symbolicate prefix '-y) 'y)))
	`(let ((,x (x-of ,point)) (,y (y-of ,point)))
	   ,@body)))))

(defmacro apply-points (points &body body)
  `(progn
     ,@(loop for point in points
	     collect `(apply-point ,point ,@body))))


