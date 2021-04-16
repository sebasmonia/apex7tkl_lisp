;;;; apex7tkl.lisp

(in-package #:apex7tkl)

(defparameter *keyb* nil "Local reference to the keyboard. Set during (initialize)")

(defun initialize ()
  (setf *keyb* (get-apex7tkl-device)))

(defun get-apex7tkl-device ()
  "Return the ."
  (let* ((vendor-id 4152)
         (product-id 5656)
         (device (car (cl-libusb:usb-get-devices-by-ids vendor-id product-id))))
    (when device
      (cl-libusb:usb-simple-setup device))
    device))


;; https://lisptips.com/post/44370032877/literal-syntax-for-integers

;; Can change the Esc (#x29) key color correctly, but needs a call to detach the kernel and then
;; the keyboard stops working until (libusb-ffi:usb-reset (cl-libusb::usb-handle-pointer a7t::*keyb*))

(defun test ()
  (static-vectors:with-static-vector (vect 642 :initial-element 0)
    (setf (elt vect 0) #x3a)
    (setf (elt vect 1) #x69)
    (setf (elt vect 2) #x29)
    (setf (elt vect 3) #x00)
    (setf (elt vect 4) #x00)
    (setf (elt vect 5) #x00)
    (claim)
    (cl-libusb:usb-control-msg *keyb* #x21 #x9 #x300 #x1 vect 3000)
    (release)
    ))

(defun claim ()
  (cl-libusb:usb-claim-interface *keyb* 0)
  (cl-libusb:usb-claim-interface *keyb* 1)
  (cl-libusb:usb-claim-interface *keyb* 2)
  (cl-libusb:usb-claim-interface *keyb* 3)
  (cl-libusb:usb-claim-interface *keyb* 4))

(defun release ()
  (cl-libusb:usb-release-interface *keyb* 0)
  (cl-libusb:usb-release-interface *keyb* 1)
  (cl-libusb:usb-release-interface *keyb* 2)
  (cl-libusb:usb-release-interface *keyb* 3)
  (cl-libusb:usb-release-interface *keyb* 4))

;;(bmRequestType=0x21, bRequest=9, wValue=0x0200, wIndex=0, data_or_wLength=array.array'(
