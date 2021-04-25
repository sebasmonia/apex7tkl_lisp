;;;; apex7tkl.lisp

(in-package #:apex7tkl)

(defparameter *debug* t "Print debug messages when runnning.")

(defun initialize ()
  (cffi:load-foreign-library "libusb-1.0.so")
  )

(defun debug-message (template &rest values)
  (when *debug*
    (apply #'format `(t ,template ,@values))))

(defun send-control-message (data &optional (w-value #x300))
  "Sends DATA in vector to the Apex 7 TKL device, using W-VALUE (default #x300)."
  (let ((ctx (cffi:null-pointer))
        (vendor-id 4152)
        (product-id 5656))
    (unless (= (%usb:init ctx) 0)
      (error "Failed to initialize libusb"))
    (%usb:set-debug ctx 3)
    ;; Track return code of each call
    (let* ((keyb (%usb:open-device-with-vid-pid ctx vendor-id product-id))
           (auto-detach (%usb:set-auto-detach-kernel-driver keyb 1))
           (claim (%usb:claim-interface keyb 1))
           (bytes-written
             (cffi::with-foreign-array (foreing-vect data `(:array %usb:uint8-t ,(array-total-size data)))
               (%usb:control-transfer keyb #x21 #x9 w-value #x1 foreing-vect (array-total-size data) 3000)))
           (release (%usb:claim-interface keyb 1)))
      (debug-message
       "auto-detach: ~a~%claim: ~a~%bytes: ~a~%release: ~a~%"
       auto-detach
       claim
       bytes-written
       release)
      (%usb:close keyb))
    (%usb:exit ctx)))


;; Function to drop in the REPL to test
(defun test ()
           (let ((vect (make-array 642 :initial-element 0 :element-type 'integer)))
             (setf (elt vect 0) #x3a)
             (setf (elt vect 1) #x69)
             (setf (elt vect 2) #x29)
             (setf (elt vect 3) #xff)
             (setf (elt vect 4) #x00)
             (setf (elt vect 5) #x00)
             (a7t:send-control-message vect #x200)))



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
