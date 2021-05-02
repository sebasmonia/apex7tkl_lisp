;;;; images.lisp

(in-package #:apex7tkl)

(defparameter +max-image-width+ 128 "Horizontal pixel size of the OLED screen.")
(defparameter +max-image-height+ 40 "Vertical pixel size of the OLED screen.")

(defun format-for-oled (image-data &optional (threshold 1))
  "Formats the IMAGE-DATA for tramission to the oled screen.
Optional argument THRESHOLD indicates the maximum value a pixel must have to be painted
white or black. Defaults to 1, ie paint everything not strictly black in the original.
This function is not very lispy. It is a rough conversion of the one in the original Python
code. I will probably revisit this later..."
  (labels ((zero-or-one (pixel)
             (if (> threshold pixel)
                 #\0
                 #\1)))
    (let ((byte-data (make-array 8))
          (index 0)
          (translated nil))
      (loop for y from 0 below (min (cl-gd:image-height image-data) +max-image-height+)
            do (loop for x from 0 below (min (cl-gd:image-width image-data) +max-image-width+)
                     do
                        (setf (elt byte-data index) (zero-or-one
                                                     (cl-gd:get-pixel x y :image image-data)))
                        (incf index)
                        (when (= index 8)
                          ;; compute and start over
                          (push (parse-integer (concatenate 'string byte-data) :radix 2) translated)
                          (setf index 0))))
      ;; The original code goes over the pixels backwards, reversing the output...
      (nreverse translated))))

(defun detect-threshold (image-data)
  "Go over the image's raw pixels and get the average value.
The average is good value to adjust use as threshold for `format-for-oled'."
  (let ((counter 0)
        (total 0))
    (cl-gd:do-pixels (image-data)
      (setf total (+ total (cl-gd:raw-pixel)))
      (incf counter))
    (ceiling (/ total counter))))


(defun image-for-text (lines)
  "Writes LINES list of strings in the oled screen."
  ;; Used the first cl-gd sample code as a starting point:
  (cl-gd:with-image* (+max-image-width+ +max-image-height+)
    (cl-gd:allocate-color 0 0 0) ; First color is the background (black)
    (let ((white (cl-gd:allocate-color 255 255 255)))
      (cl-gd:with-default-color (white)
        (cl-gd:with-default-font (:small)
          (cl-gd:draw-string 1 0 (or (first lines) ""))
          (cl-gd:draw-string 1 11 (or (second lines) ""))
          (cl-gd:draw-string 1 22 (or (third lines) "")))))
    (format-for-oled cl-gd:*default-image*)))
