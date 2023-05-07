(in-package #:cl-raygui)

(defmethod translate-name-from-foreign ((spec string) (package (eql *package*)) &optional varp)
 (let ((name (translate-camelcase-name spec :upper-initial-p t :special-words '("2D" "3D" "FPS" "HSV" "POT" "RES" "TTF" "BRDF" "URL"))))
  (if varp (intern (format nil "*~a" name)) name)))

(defmethod translate-name-to-foreign ((spec symbol) (package (eql *package*)) &optional varp)
 (let ((name (translate-camelcase-name spec :upper-initial-p t :special-words '("2D" "3D" "FPS" "HSV" "POT" "RES" "TTF" "BRDF" "URL"))))
  (if varp (subseq name 1 (1- (length name))) name)))

(defun reset-state ()
  (gui-set-state :state-normal)
  (gui-set-font
   (make-font :base-size 0
              :glyph-count 0
              :glyph-padding 0
              :texture (make-texture
                        :id 1 ; gui-set-font will do nothing if id <= 0
                        :width 0
                        :height 0
                        :mipmaps 0
                        :format 0)
              :recs (cffi:null-pointer)
              :glyphs (cffi:null-pointer)))
  (gui-enable)
  (gui-unlock)
  (gui-fade 1.0)
  (gui-set-icon-scale 1)
  (gui-disable-tooltip)
  (gui-set-tooltip "")
  (gui-load-style-default))

(defmacro int-bool (n)
  `(if (eql ,n 1) t nil))

(defmacro bool-int (n)
  `(if ,n 1 0))

