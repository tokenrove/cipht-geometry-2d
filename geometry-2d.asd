;; -*- Lisp -*-

(defsystem geometry-2d
  :depends-on (:anaphora :alexandria)
  :serial t
  :components
  ((:file "package")
   (:file "constants")
   (:file "math")
   ;;(:file "macros")
   ;;(:file "scalar")
   (:file "point")
   (:file "box")
   ;;(:file "polygon")
   ))
