;;;; images.lisp

(in-package #:apex7tkl)

(defun format-for-oled (image-data &optional (threshold 1))
  "Formats the IMAGE-DATA for tramission to the oled screen.
Optional argument THRESHOLD indicates the maximum value a pixel must have to be painted
white or black.
This function is not very lispy. It is a rough conversion of the one in the original Python
code. I will probably revisit this later..."
  (let ((byte-data (make-array 8))
        (index 0)
        (translated nil))
    (cl-gd:do-pixels (image-data)
      (format nil "~a " (cl-gd:raw-pixel))
      (setf (elt byte-data index) (if (< threshold (cl-gd:raw-pixel))
                                      #\0
                                      #\1))
      (incf index)
      (when (= index 8)
        ;; compute and start over
        (push (parse-integer (concatenate 'string byte-data) :radix 2) translated)
        (setf index 0)))
    (format nil "~%")
    ;; The original code goes over the pixels backwards, reversing the output...
    (nreverse translated)))

(defun detect-threshold (image-data)
  "Go over the image's raw pixels and get the average value.
This is a good value to adjust use as threshold for `format-for-oled'."
  (let ((counter 0)
        (total 0))
    (cl-gd:do-pixels (image-data)
      (setf total (+ total (cl-gd:raw-pixel)))
      (incf counter))
    (ceiling (/ total counter))))

(defun image-for-text (lines)
  "Writes LINES in an image."
  ;; Using cl-gd sample code as a starting point:
  (cl-gd:with-image* (128 40 t) ; create 128x40 pixel image
    (cl-gd:allocate-color 0 0 0)
    (let ((white (cl-gd:allocate-color 250 250 250)))
      (cl-gd:with-default-color (white)
        (cl-gd:with-default-font (:small)
          (cl-gd:draw-string 1 0 "This is a test")
          (cl-gd:draw-string 1 11 "Trying out font sizes")
          (cl-gd:draw-string 1 22 "15%")
          (cl-gd:draw-string 1 33 "15%"))))
    ;; (cl-gd:write-image-to-file "text.png"
    ;;                            :compression-level 6 :if-exists :supersede)
    ;; (print (detect-threshold cl-gd:*default-image*))
    (format-for-oled cl-gd:*default-image* 0)))
