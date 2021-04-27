;;;; apex7tkl.asd

(asdf:defsystem #:apex7tkl
  :description "Library to communicate with the Steelseries Apex 7 TKL"
  :author "Sebastián Monía <smonia@outlook.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:claw-usb
               #:cffi-c-ref
               #:static-vectors)
  :components ((:file "package")
               (:file "keycodes")
               (:file "images")
               (:file "apex7tkl")))
