;;;; apex7tkl.asd

(asdf:defsystem #:apex7tkl
  :description "Library to communicate with the Steelseries Apex 7 TKL"
  :author "Sebastián Monía <smonia@outlook.com>"
  :license  "MIT"
  :version "0.5.0"
  :serial t
  :build-operation "program-op"
  :build-pathname "apex7tkl"
  :entry-point "apex7tkl:main"
  :depends-on (#:alexandria
               #:claw-usb
               #:cl-gd)
  :components ((:file "package")
               (:file "keycodes")
               (:file "images")
               (:file "cli")
               (:file "apex7tkl")))
