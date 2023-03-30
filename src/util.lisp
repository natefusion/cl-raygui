(in-package #:cl-raygui)

(defmethod translate-name-from-foreign ((spec string) (package (eql *package*)) &optional varp)
 (let ((name (translate-camelcase-name spec :upper-initial-p t :special-words '("2D" "3D" "FPS" "HSV" "POT" "RES" "TTF" "BRDF" "URL"))))
  (if varp (intern (format nil "*~a" name)) name)))

(defmethod translate-name-to-foreign ((spec symbol) (package (eql *package*)) &optional varp)
 (let ((name (translate-camelcase-name spec :upper-initial-p t :special-words '("2D" "3D" "FPS" "HSV" "POT" "RES" "TTF" "BRDF" "URL"))))
  (if varp (subseq name 1 (1- (length name))) name)))

(defun reset-state ()
  (gui-enable)
  (gui-unlock)
  (gui-fade 1.0)
  (gui-set-icon-scale 1)
  (gui-load-style-default))

(defmacro int-bool (n)
  `(if (eql ,n 1) t nil))

(defmacro bool-int (n)
  `(if ,n 1 0))

