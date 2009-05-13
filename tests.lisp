
(defpackage :cipht/games/geometry-2d-tests
  (:use #:cl #:fiveam)
  (:export #:geometry-2d))

(in-package :cipht/games/geometry-2d-tests)

(def-suite geometry-2d)
(in-suite geometry-2d)

;;;; CONSTANTS

(test constants.0
  (is (<= (* 2 3.14159) (- geom:+2pi+ short-float-epsilon) (+ geom:+2pi+ short-float-epsilon) (* 2 3.1416))))

;;;; MATH

(test sinkf.0
  (for-all ((x (gen-integer))
	    (m (gen-integer) (> (abs m) (abs x))))
    (geom:sinkf x m)
    (is (= 0 x))))

(test sinkf.1
  (for-all ((x (gen-integer))
	    (m (gen-integer :max 0) (< (abs m) (abs x))))
    (let ((y x))
      (geom:sinkf y m)
      (is (< (abs y) (abs x))))))

(test sinkf.2
  (is (= 1 (let ((x 3))
	     (geom:sinkf x 2)
	     x))))

;;;; POINTS

(test point.0
  (let ((p (geom:point 100 200)))
    (is (= 100 (geom:x-of p)))
    (is (= 200 (geom:y-of p)))))

(test point.1
  (signals type-error (geom:point nil 0)))

(test translate-point.0
  (for-all ((px (gen-integer)) (py (gen-integer))
	    (qx (gen-integer)) (qy (gen-integer)))
    (let ((p (geom:point px py))
	  (q (geom:point qx qy)))
      (let ((r (geom:translate-point p q)))
	(is (= px (geom:x-of p)))
	(is (= py (geom:y-of p)))
	(is (= qx (geom:x-of q)))
	(is (= qy (geom:y-of q)))
	(is (= (+ px qx)) (geom:x-of r))
	(is (= (+ py qy)) (geom:y-of r))))))

(test translate-point!.0
  (for-all ((px (gen-integer)) (py (gen-integer))
	    (qx (gen-integer)) (qy (gen-integer)))
    (let ((p (geom:point px py))
	  (q (geom:point qx qy)))
      (let ((r (geom:translate-point! p q)))
	(is (= qx (geom:x-of q)))
	(is (= qy (geom:y-of q)))
	(is (= (+ px qx)) (geom:x-of r))
	(is (= (+ py qy)) (geom:y-of r))
	(is (eq p r))))))

(test translate-point*.0
  (for-all ((px (gen-integer)) (py (gen-integer))
	    (qx (gen-integer)) (qy (gen-integer)))
    (let ((p (geom:point px py)))
      (let ((r (geom:translate-point* p :x qx :y qy)))
	(is (= (+ px qx)) (geom:x-of r))
	(is (= (+ py qy)) (geom:y-of r))
	(is (eq p r))))))

(test point<-point.0
  (for-all ((px (gen-integer)) (py (gen-integer))
	    (qx (gen-integer)) (qy (gen-integer)))
    (let ((p (geom:point px py))
	  (q (geom:point qx qy)))
      (let ((r (geom:point<-point! p q)))
	(is (= qx (geom:x-of p)))
	(is (= qy (geom:y-of p)))
	(is (= qx (geom:x-of q)))
	(is (= qy (geom:y-of q)))
	(is (= qx (geom:x-of r)))
	(is (= qy (geom:y-of r)))
	(is (eq p r))
	(is-false (eq p q))))))

(test untranslate-point.0
  (for-all ((px (gen-integer)) (py (gen-integer))
	    (qx (gen-integer)) (qy (gen-integer)))
    (let ((p (geom:point px py))
	  (q (geom:point qx qy)))
      (let ((r (geom:untranslate-point p q)))
	(is (= px (geom:x-of p)))
	(is (= py (geom:y-of p)))
	(is (= qx (geom:x-of q)))
	(is (= qy (geom:y-of q)))
	(is (= (- px qx)) (geom:x-of r))
	(is (= (- py qy)) (geom:y-of r))))))

;;;; BOXES

(test box.0
  (for-all ((blx (gen-integer)) (bly (gen-integer))
	    (urx (gen-integer) (> urx blx)) (ury (gen-integer) (> ury bly)))
    (let ((b (geom:box blx bly urx ury)))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= urx (geom:x-of (geom:ur-corner b))))
      (is (= ury (geom:y-of (geom:ur-corner b)))))))

(test box.1
  (for-all ((blx (gen-integer)) (bly (gen-integer))
	    (w (gen-integer :min 0)) (h (gen-integer :min 0)))
    (let ((b (geom:box blx bly w h t)))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= (+ blx w) (geom:x-of (geom:ur-corner b))))
      (is (= (+ bly h) (geom:y-of (geom:ur-corner b)))))))

(test box.2
  (for-all ((blx (gen-integer)) (bly (gen-integer))
	    (urx (gen-integer) (> urx blx)) (ury (gen-integer) (> ury bly)))
    (let ((b (geom:box (geom:point blx bly) (geom:point urx ury))))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= urx (geom:x-of (geom:ur-corner b))))
      (is (= ury (geom:y-of (geom:ur-corner b)))))))

(test box.3
  (for-all ((blx (gen-integer)) (bly (gen-integer))
	    (urx (gen-integer) (> urx blx)) (ury (gen-integer) (> ury bly)))
    (let ((b (geom:box (geom:point urx ury) (geom:point blx bly))))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= urx (geom:x-of (geom:ur-corner b))))
      (is (= ury (geom:y-of (geom:ur-corner b)))))))

(test box.4
  (for-all ((blx (gen-integer)) (bly (gen-integer))
	    (urx (gen-integer) (> urx blx)) (ury (gen-integer) (> ury bly)))
    (let ((b (geom:box urx ury blx bly)))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= urx (geom:x-of (geom:ur-corner b))))
      (is (= ury (geom:y-of (geom:ur-corner b)))))))

(test box-width.0
  (for-all ((blx (gen-integer)) (bly (gen-integer))
	    (urx (gen-integer) (> urx blx)) (ury (gen-integer) (> ury bly)))
    (let ((b (geom:box blx bly urx ury))
	  (c (geom:box blx bly urx ury t)))
      (is (= (abs (- urx blx)) (geom:box-width b)))
      (is (= (abs urx) (geom:box-width c))))))

(test box-height.0
  (for-all ((blx (gen-integer)) (bly (gen-integer))
	    (urx (gen-integer) (> urx blx)) (ury (gen-integer) (> ury bly)))
    (let ((b (geom:box blx bly urx ury))
	  (c (geom:box blx bly urx ury t)))
      (is (= (abs (- ury bly)) (geom:box-height b)))
      (is (= (abs ury) (geom:box-height c))))))
