
(defpackage :net.cipht/games/geometry-2d/external-tests
  (:use #:cl #:fiveam))
(in-package :net.cipht/games/geometry-2d/external-tests)

(in-suite geom:unit)

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
  (for-all ((px (gen-float)) (py (gen-float))
	    (qx (gen-float)) (qy (gen-float)))
    (let ((p (geom:point px py))
	  (q (geom:point qx qy)))
      (let ((r (geom:point+ p q)))
	(is (= px (geom:x-of p)))
	(is (= py (geom:y-of p)))
	(is (= qx (geom:x-of q)))
	(is (= qy (geom:y-of q)))
	(is (= (+ px qx)) (geom:x-of r))
	(is (= (+ py qy)) (geom:y-of r))))))

;;;; BOXES

(test box.0
  (for-all ((blx (gen-float)) (bly (gen-float))
	    (urx (gen-float) (> urx blx)) (ury (gen-float) (> ury bly)))
    (let ((b (geom:box blx bly urx ury)))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= urx (geom:x-of (geom:ur-corner b))))
      (is (= ury (geom:y-of (geom:ur-corner b)))))))

(test box.1
  (for-all ((blx (gen-float)) (bly (gen-float))
            (w (gen-float)) (h (gen-float)))
    (let ((b (geom:box blx bly (abs w) (abs h) t)))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= (+ blx (abs w)) (geom:x-of (geom:ur-corner b))))
      (is (= (+ bly (abs h)) (geom:y-of (geom:ur-corner b)))))))

(test box.2
  (for-all ((blx (gen-float)) (bly (gen-float))
	    (urx (gen-float) (> urx blx)) (ury (gen-float) (> ury bly)))
    (let ((b (geom:box (geom:point blx bly) (geom:point urx ury))))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= urx (geom:x-of (geom:ur-corner b))))
      (is (= ury (geom:y-of (geom:ur-corner b)))))))

(test box.3
  (for-all ((blx (gen-float)) (bly (gen-float))
	    (urx (gen-float) (> urx blx)) (ury (gen-float) (> ury bly)))
    (let ((b (geom:box (geom:point urx ury) (geom:point blx bly))))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= urx (geom:x-of (geom:ur-corner b))))
      (is (= ury (geom:y-of (geom:ur-corner b)))))))

(test box.4
  (for-all ((blx (gen-float)) (bly (gen-float))
	    (urx (gen-float) (> urx blx)) (ury (gen-float) (> ury bly)))
    (let ((b (geom:box urx ury blx bly)))
      (is (= blx (geom:x-of (geom:bl-corner b))))
      (is (= bly (geom:y-of (geom:bl-corner b))))
      (is (= urx (geom:x-of (geom:ur-corner b))))
      (is (= ury (geom:y-of (geom:ur-corner b)))))))

(test box-width.0
  (for-all ((blx (gen-float)) (bly (gen-float))
	    (urx (gen-float) (> urx blx)) (ury (gen-float) (> ury bly)))
    (let ((b (geom:box blx bly urx ury))
	  (c (geom:box blx bly urx ury t)))
      (is (= (abs (- urx blx)) (geom:box-width b)))
      (is (= (abs urx) (geom:box-width c))))))

(test box-height.0
  (for-all ((blx (gen-float)) (bly (gen-float))
	    (urx (gen-float) (> urx blx)) (ury (gen-float) (> ury bly)))
    (let ((b (geom:box blx bly urx ury))
	  (c (geom:box blx bly urx ury t)))
      (is (= (abs (- ury bly)) (geom:box-height b)))
      (is (= (abs ury) (geom:box-height c))))))
