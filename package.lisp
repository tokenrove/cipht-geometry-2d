
(defpackage #:cipht/games/geometry-2d
  (:nicknames #:geom-2d #:geom)
  (:use #:cl #:alexandria #:anaphora)
  (:export
   #:+2PI+
   #:point
   #:x-of
   #:y-of
   #:copy-point
   #:on-point
   #:point+
   #:box
   #:bl-corner
   #:ur-corner
   #:br-corner
   #:ul-corner
   #:box-width
   #:box-height
   ;; math
   #:sink
   #:sinkf
   #:ilerp
   #:mouse-arc
   #:half
   #:square
   #:in-modulo-range-p
   #:sum-of-squares
   ))
