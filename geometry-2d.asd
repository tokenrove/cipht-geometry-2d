;; -*- Lisp -*-

(defpackage #:geometry-2d-system (:use #:cl #:asdf))
(in-package #:geometry-2d-system)

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

(defsystem geometry-2d-tests
  :depends-on (:geometry-2d :fiveam)
  :serial t
  :components
  ((:file "tests")))