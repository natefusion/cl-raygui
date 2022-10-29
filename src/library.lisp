(in-package #:cl-raygui)
(define-foreign-library raygui
  (:darwin "raygui.dynlib")
  (:unix "raygui.so")
  (:windows "raygui.dll")
  (t (:default "raygui")))

(let ((cffi:*foreign-library-directories* '("/usr/local/lib/")))
  (unless (foreign-library-loaded-p 'raygui)
    (use-foreign-library raygui)))



