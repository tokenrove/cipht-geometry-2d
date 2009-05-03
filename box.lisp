(in-package :cipht/games/geometry-2d)

(defun box (x y &optional x2 y2 relative?)
  "If only X and Y are specified, they are assumed to be points;
otherwise the box is specified by (X,Y) and (X2,Y2).  If RELATIVE? is
non-NIL, X2 and Y2 are treated as width and height."
  (unless x2 (setf x2 (x-of y) y2 (y-of y) y (y-of x) x (x-of x)))
  (when relative? (incf x2 x) (incf y2 y))
  (when (< x2 x) (rotatef x x2))
  (when (< y2 y) (rotatef y y2))
  (make-array 4 :element-type 'number :initial-contents (list x y x2 y2)))

(defun displace-point (box offset)
  (make-array 2 :element-type (array-element-type box) :displaced-to box :displaced-index-offset offset))

(defun bl-corner (box) (displace-point box 0))
(defun ur-corner (box) (displace-point box 2))
(defun br-corner (box) (point (aref box 2) (aref box 1)))
(defun ul-corner (box) (point (aref box 0) (aref box 3)))

(defun box-width (box) (- (x-of (ur-corner box)) (x-of (bl-corner box))))
(defun box-height (box) (- (y-of (ur-corner box)) (y-of (bl-corner box))))

(defun translate-box (box point)
  (box (translate-point (bl-corner box) point)
       (translate-point (ur-corner box) point)))
(defun untranslate-box (box point)
  (box (untranslate-point (bl-corner box) point)
       (untranslate-point (ur-corner box) point)))

(defun copy-box (box) (copy-array box))

(defun translate-box-to (box bl-corner)
  (aprog1 (copy-box box)
    (setf (bl-corner it) bl-corner)))

(defun translate-box-to! (box bl-corner)
  (prog1 box
    (setf (bl-corner box) bl-corner)))

(defun (setf bl-corner) (point box)
  (psetf (aref box 0) (x-of point)
	 (aref box 1) (y-of point)
	 (aref box 2) (+ (x-of point) (box-width box))
	 (aref box 3) (+ (y-of point) (box-height box))))

(defun scale-box-by-point (b p)
  (box (* (x-of p) (aref b 0)) (* (y-of p) (aref b 1))
       (* (x-of p) (aref b 2)) (* (y-of p) (aref b 3))))

(defun box-contains-point? (box point)
  (and (<= (aref box 0) (x-of point) (aref box 2))
       (<= (aref box 1) (y-of point) (aref box 3))))

(defun box-contains-box? (a b)
  (and (or (<= (aref a 0) (aref b 0) (aref a 2))
	   (<= (aref a 0) (aref b 2) (aref a 2)))
       (or (<= (aref a 1) (aref b 1) (aref a 3))
	   (<= (aref a 1) (aref b 3) (aref a 3)))))

(defun center-box-on-point! (box point)
  (translate-box-to! box (translate-point point (point (- (half (box-width box))) (- (half (box-height box)))))))

(defmacro apply-box-corners-ccw ((&rest boxes) &body body)
  `(progn
     ,@(loop for point in '(bl-corner br-corner ur-corner ul-corner)
	     collect `(let ,(loop for (prefix box) in boxes
				  nconc (let ((x (symbolicate prefix '-x))
					      (y (symbolicate prefix '-y)))
					  `((,x (x-of (,point ,box))) (,y (y-of (,point ,box))))))
			,@body))))


(defun polygon<-box (box)
  (make-array '(4 2) :initial-contents (list (bl-corner box) (br-corner box) (ur-corner box) (ul-corner box))))

(defun clamp-point (point box)
  (point (clamp (x-of point) (aref box 0) (aref box 2))
	 (clamp (y-of point) (aref box 1) (aref box 3))))

