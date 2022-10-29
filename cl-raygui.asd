(in-package #:cl-user)

#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(defpackage #:cl-raygui-asd
  (:use :cl :asdf))

(in-package :cl-raygui-asd)

(defsystem #:cl-raygui
  :version "0.0.1"
  :author "natefusion"
  :license "MIT"
  :description "Common Lisp bindings of raygui"
  :depends-on (#:cffi
               #:alexandria
               #:cl-raylib)
  :serial t
  :pathname "src"
  :components
  ((:file "package")
   (:file "library")
   (:file "util")
   (:file "raygui"))
  :in-order-to ((test-op (test-op cl-raylib-test))))
