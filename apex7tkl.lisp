;;;; apex7tkl.lisp

(in-package #:apex7tkl)

(defparameter *keyb* nil "Local reference to the keyboard. Set during (initialize)")
(defparameter *bytes-last-message* -1 "Temporary test variable")

(defun initialize ()
  (cffi:load-foreign-library "libusb-1.0.so")
  (get-apex7tkl-device)
  )

(defun get-apex7tkl-device ()
  "Return a reference to the apex7tkl device."
  (let ((ctx (cffi:null-pointer))
        (vendor-id 4152)
        (product-id 5656))
    (unless (= (%usb:init ctx) 0)
      (error "Failed to initialize libusb"))
    (setf *keyb*
          (%usb:open-device-with-vid-pid ctx vendor-id product-id))
    (%usb:exit ctx)))

;; https://lisptips.com/post/44370032877/literal-syntax-for-integers

;; Can change the Esc (#x29) key color correctly, but needs a call to detach the kernel and then
;; the keyboard stops working until (libusb-ffi:usb-reset (cl-libusb::usb-handle-pointer a7t::*keyb*))

(defun test ()
  (let ((ctx (cffi:null-pointer))
        (bytes-written -1)
        (vect (make-array 642 :initial-element 0 :element-type 'integer)))
    (unless (= (%usb:init ctx) 0)
      (error "Failed to initialize libusb"))
    (cffi::with-foreign-array (foreing-vect vect '(:array %usb:uint8-t 642))
      (setf (elt vect 0) #x3a)
      (setf (elt vect 1) #x69)
      (setf (elt vect 2) #x29)
      (setf (elt vect 3) #x00)
      (setf (elt vect 4) #x00)
      (setf (elt vect 5) #x00)
      (claim)
      (setf bytes-written
            (%usb:control-transfer *keyb* #x21 #x9 #x300 #x1 foreing-vect 642 3000))
      (release))
  (%usb:exit ctx)
  bytes-written))


(defun claim ()
  (%usb:claim-interface *keyb* 0)
  (%usb:claim-interface *keyb* 1)
  (%usb:claim-interface *keyb* 2)
  (%usb:claim-interface *keyb* 3)
  (%usb:claim-interface *keyb* 4))

(defun release ()
  (%usb:release-interface *keyb* 0)
  (%usb:release-interface *keyb* 1)
  (%usb:release-interface *keyb* 2)
  (%usb:release-interface *keyb* 3)
  (%usb:release-interface *keyb* 4))

;; ;;(bmRequestType=0x21, bRequest=9, wValue=0x0200, wIndex=0, data_or_wLength=array.array'(
