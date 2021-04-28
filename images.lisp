;;;; images.lisp

(in-package #:apex7tkl)

(defun format-for-oled (image-path)
  (let ((image-data (cl-gd:create-image-from-file "/home/hoagie/common-lisp/apex7tkl_lisp/grimm.png"))
        (translated nil))
    (print image-path)
    (cl-gd:do-pixels (image-data)
      (push (if (zerop (cl-gd:raw-pixel))
                0
                1)
            translated))
    translated))

(defun image-for-text (text)
  (print text)
  )
