;;;; apex7tkl.lisp

(in-package #:apex7tkl)

(defun get-apex7tkl-device ()
  "Return the ."
  (let* ((vendor-id 4152)
         (product-id 5656)
         (device (car (cl-libusb:usb-get-devices-by-ids vendor-id product-id))))
    (when device
      (cl-libusb:usb-simple-setup device))
    device))
