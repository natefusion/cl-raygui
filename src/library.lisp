(in-package #:cl-raygui)
(define-foreign-library raygui
  (:darwin "raygui.dynlib")
  ;; doesn't work without full path for some reason
  (:unix "/usr/local/lib/raygui.so")
  (:windows "raygui.dll")
  (t (:default "raygui")))

(unless (foreign-library-loaded-p 'raygui)
  (use-foreign-library raygui))

