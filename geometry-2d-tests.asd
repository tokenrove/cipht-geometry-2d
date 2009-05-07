;; -*- Lisp -*-

(defpackage #:geometry-2d-tests-system (:use #:cl #:asdf))
(in-package #:geometry-2d-tests-system)

(defsystem geometry-2d-tests
  :depends-on (:geometry-2d :fiveam)
  :serial t
  :components
  ((:file "tests")))
