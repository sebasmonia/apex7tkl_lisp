;;;; apex7tkl.lisp

(in-package #:apex7tkl)

(defparameter *debug* t "Set to t to print debug messages when runnning, and enable extra debugging in libusb.")

(defun debug-message (template &rest values)
  "Print the message in TEMPLATE using VALUES to standard output. A testing/debugging aid."
  (when *debug*
    (apply #'format `(t ,template ,@values))))

(defun initialize ()
  "Load libusb 1.0 via cffi and initialize other package elements.
This is meant to be called before any operation."
  (cffi:load-foreign-library "libusb-1.0.so")
  (build-key-codes))

;; TODO: w-value seems to be ok with either #x200 and #x300.
;; Need more testing, for now I will copy the values from
;; the Python version of the code
(defun send-control-message (data &optional (w-value #x300))
  "Sends DATA in vector to the Apex 7 TKL device, using W-VALUE (default #x300).
This function is the main only interaction point with the keyboard, and the only one
that depends on libusb and cffi.
The return value can be interpreted as an exit code."
  (let ((ctx (cffi:null-pointer))
        (vendor-id 4152)
        (product-id 5656)
        (return-value 0)
        (keyb nil))
    (unless (= (%usb:init ctx) 0)
      (error "Failed to initialize libusb"))
    (when *debug*
      (%usb:set-debug ctx 3)
      (debug-message "Enabled extra libusb output.~%"))
    (setf keyb (%usb:open-device-with-vid-pid ctx vendor-id product-id))
    (if (cffi:null-pointer-p keyb)
        (setf return-value 19) ;; allegedly, exit code for "No such device"
        ;; else, start interacting with the device:
        ;; Track return code of each call
        (let* ((auto-detach (%usb:set-auto-detach-kernel-driver keyb 1))
               (claim (%usb:claim-interface keyb 1))
               (bytes-written
                 (cffi::with-foreign-array (foreing-vect data `(:array %usb:uint8-t ,(array-total-size data)))
                   (%usb:control-transfer keyb #x21 #x9 w-value #x1 foreing-vect (array-total-size data) 3000)))
               (release (%usb:claim-interface keyb 1)))
          (unless (= bytes-written (array-total-size data))
            (setf return-value 1))
          (debug-message
           "Output of device communication: auto-detach: ~a claim interface: ~a bytes sent: ~a release interface: ~a~%"
           auto-detach
           claim
           bytes-written
           release)
          (%usb:close keyb)))
    ;; Device present or not, close the context
    (%usb:exit ctx)
    return-value))

(defun set-color-region (region-name red green blue)
  "Changes the color of the keys in REGION-NAME with RED GREEN BLUE values."
  ;; TODO: validate red green blue 0~255
  (let ((region-codes (get-region-codes region-name))
        (vect (make-array 642 :initial-element 0 :element-type 'integer)))
    (debug-message "Key codes to color: ~a~%RGB: ~a ~a ~a~%" region-codes red green blue)
    ;; hardcoded values for "set color"
    (setf (elt vect 0) #x3a)
    (setf (elt vect 1) #x69)
    ;; start adding the 3 color values per key. We'll run the "position"
    ;; in step of 4 since we'll add key-code + r + g +b
    (loop for key-code in region-codes
          for position from 2 below 1000 by 4
          do (setf (elt vect position) key-code)
             (setf (elt vect (+ 1 position)) red)
             (setf (elt vect (+ 2 position)) green)
             (setf (elt vect (+ 3 position)) blue))
    (a7t:send-control-message vect #x300)))

(defun set-color-keys (keys-colors-alist)
  "Changes the color KEYS-COLORS-ALIST, an alist with the format (keyname . (r g b))."
  (let ((vect (make-array 642 :initial-element 0 :element-type 'integer)))
    ;; hardcoded values for "set color"
    (setf (elt vect 0) #x3a)
    (setf (elt vect 1) #x69)
    ;; start adding the 3 color values per key. We'll run the "position"
    ;; in step of 4 since we'll add key-code + r + g +b
    ;; unlike `set-color-region', here the rgb values can change
    ;; also, translate the key name to its code to add it to the vector
    (loop for (key . (red green blue)) in keys-colors-alist
          for position from 2 below 1000 by 4
          for key-code = (get-key-code-by-name key)
          do (debug-message "Adding key: ~a (~a) RGB: ~a ~a ~a~%" key key-code red green blue)
             (setf (elt vect position) key-code)
             (setf (elt vect (+ 1 position)) red)
             (setf (elt vect (+ 2 position)) green)
             (setf (elt vect (+ 3 position)) blue))
    (a7t:send-control-message vect #x300)))

(defun set-config (config-number)
  "Set the keyboard to CONFIG-NUMBER configuration."
  (unless (member config-number '(1 2 3 4 5))
    (error "Invalid config-number. Configurations are in the range 1-5."))
  (let ((vect (make-array 20 :initial-element 0 :element-type 'integer)))
    ;; hardcoded value for "set config"
    (setf (elt vect 0) #x89)
    ;; the config number
    (setf (elt vect 1) config-number)
    (a7t:send-control-message vect #x200)))

(defun display-image (image-path &optional (convert nil))
  "Show IMAGE-PATH in the oled screen. Pixels over the 128x40 max limit are ignored.
The image must use only 2 colors (black/ white), but with the optional CONVERT argument,
an attempt is made to display a color image.
If the image is smaller than 128x40, it will be shown in the top left corner and the rest
of the screen stays black."
  (cl-gd:with-image-from-file* (image-path)
    (let ((converted-image (format-for-oled cl-gd:*default-image*
                                            (if convert
                                                (detect-threshold cl-gd:*default-image*)
                                                1)))
          (vect (make-array 642 :initial-element 0 :element-type 'integer)))
      (debug-message "Image data: ~a~%" converted-image)
      ;; hardcoded value for "OLED image"
      (setf (elt vect 0) #x65)
      ;; Add the image data
      (loop for pixel in converted-image
            for ndx from 1 below 643
            do (setf (elt vect ndx) pixel))
      (a7t:send-control-message vect #x300))))

(defun display-text (lines)
  "Write LINES to the display."
  (let* ((converted-image (image-for-text lines))
         (vect (make-array 642 :initial-element 0 :element-type 'integer)))
    ;; hardcoded value for "OLED image"
    ;; (debug-message "Image data: ~a~%" converted-image)
    (setf (elt vect 0) #x65)
    ;; Add the image data
    (loop for pixel in converted-image
          for ndx from 1 below 643
          do (setf (elt vect ndx) pixel))
    (a7t:send-control-message vect #x300)))

;; https://lisptips.com/post/44370032877/literal-syntax-for-integers

;; libusb_error {
;; LIBUSB_SUCCESS = 0,
;; LIBUSB_ERROR_IO = -1,
;; LIBUSB_ERROR_INVALID_PARAM = -2,
;; LIBUSB_ERROR_ACCESS = -3,
;; LIBUSB_ERROR_NO_DEVICE = -4,
;; LIBUSB_ERROR_NOT_FOUND = -5,
;; LIBUSB_ERROR_BUSY = -6,
;; LIBUSB_ERROR_TIMEOUT = -7,
;; LIBUSB_ERROR_OVERFLOW = -8,
;; LIBUSB_ERROR_PIPE = -9,
;; LIBUSB_ERROR_INTERRUPTED = -10,
;; LIBUSB_ERROR_NO_MEM = -11,
;; LIBUSB_ERROR_NOT_SUPPORTED = -12,
;; LIBUSB_ERROR_OTHER = -99
  ;; }
