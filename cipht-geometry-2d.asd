;; -*- Lisp -*-

(in-package :asdf-user)

(defsystem cipht-geometry-2d
  :description "Ancient 2D geometry support library; not recommended."
  :author "Julian Squires <julian@cipht.net>"
  :license "GPL-3; assets under CC-BY-SA"
  :depends-on (:anaphora :alexandria)
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :unit :net.cipht/games/geometry-2d)))
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
   (:file "tests")))
