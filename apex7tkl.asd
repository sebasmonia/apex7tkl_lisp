;;;; apex7tkl.asd

(asdf:defsystem #:apex7tkl
  :description "Library to communicate with the Steelseries Apex 7 TKL"
  :author "Sebastián Monía <smonia@outlook.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:cl-libusb
               #:static-vectors)
  :components ((:file "package")
               (:file "apex7tkl")))
