;;;; images.lisp

(in-package #:apex7tkl)

(defun format-for-oled (image-path)
  "Opens IMAGE-PATH and formats the data in the image for tramission to the old screen.
This function is not very lispy. It is a rough conversion of the one in the original Python
code. I will probably revisit this later..."
  (let ((image-data (cl-gd:create-image-from-file image-path))
        (byte-data (make-array 8))
        (index 0)
        (translated nil))
    (cl-gd:do-pixels (image-data)
      (setf (elt byte-data index) (if (zerop (cl-gd:raw-pixel))
                                      #\0
                                      #\1))
      (incf index)
      (when (= index 8)
        ;; compute and start over
        (push (parse-integer (concatenate 'string byte-data) :radix 2) translated)
        (setf index 0)))
    ;; The original code goes over the pixels backwards, reversing the output...
    (nreverse translated)))

(defun image-for-text (text)
  (print text)
  )
