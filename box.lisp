(in-package :cipht/games/geometry-2d)

(deftype box () '(simple-array point (2)))

(declaim (inline %box box))
(defun %box (a b)
  (declare (type point a b) (optimize speed))
  (when (< (x-of b) (x-of a)) (psetf a (point (x-of b) (y-of a)) b (point (x-of a) (y-of b))))
  (when (< (y-of b) (y-of a)) (psetf a (point (x-of a) (y-of b)) b (point (x-of b) (y-of a))))
  (the box (make-array 2 :element-type 'point :initial-contents (list a b))))
(defun box (a b &optional x y relative?)
  "If only A and B are specified, they are assumed to be points;
otherwise the box is specified by (A,B) and (X,Y).  If RELATIVE? is
non-NIL, X and Y are treated as width and height."
  (when x
    (when relative? (incf x a) (incf y b))
    (setf a (point a b) b (point x y)))
  (%box a b))

(declaim (inline bl-corner ur-corner br-corner ul-corner box-width box-height))
(defun bl-corner (box) (aref box 0))
(defun ur-corner (box) (aref box 1))
(defun br-corner (box) (point (x-of (ur-corner box)) (y-of (bl-corner box))))
(defun ul-corner (box) (point (x-of (bl-corner box)) (y-of (ur-corner box))))

(defun box-width  (box) (declare (type box box)) (- (x-of (ur-corner box)) (x-of (bl-corner box))))
(defun box-height (box) (declare (type box box)) (- (y-of (ur-corner box)) (y-of (bl-corner box))))

(declaim (inline translate-box))
(defun translate-box (box point)
  (box (point+ (bl-corner box) point) (point+ (ur-corner box) point)))
(defun translate-box! (box point)
  (declare (type box box) (type point point))
  (setf (aref box 0) (point+ (aref box 0) point)
	(aref box 1) (point+ (aref box 1) point))
  box)

(defun copy-box (box) (copy-array box))

(declaim (inline box-contains-point? box-contains-box?))
(defun box-contains-point? (box point)
  (and (<= (x-of (bl-corner box)) (x-of point) (x-of (ur-corner box)))
       (<= (y-of (bl-corner box)) (y-of point) (y-of (ur-corner box)))))

(defun box-contains-box? (a b)
  (and (or (<= (x-of (bl-corner a)) (x-of (bl-corner b)) (x-of (ur-corner a)))
	   (<= (x-of (bl-corner a)) (x-of (ur-corner b)) (x-of (ur-corner a))))
       (or (<= (y-of (bl-corner a)) (y-of (bl-corner b)) (y-of (ur-corner a)))
	   (<= (y-of (bl-corner a)) (y-of (ur-corner b)) (y-of (ur-corner a))))))

(defun polygon<-box (box)
  (make-array 4 :element-type 'point
	      :initial-contents (list (bl-corner box) (br-corner box) (ur-corner box) (ul-corner box))))

(declaim (inline clamp-point))
(defun clamp-point (point box)
  "Clamp POINT to be within bounds of BOX."
  (declare (type point point) (type box box))
  (point (clamp (x-of point) (x-of (bl-corner box)) (x-of (ur-corner box)))
	 (clamp (y-of point) (y-of (bl-corner box)) (y-of (ur-corner box)))))
