;;;; apex7tkl.asd

;; See https://lispcookbook.github.io/cl-cookbook/scripting.html#building-a-smaller-binary-with-sbcls-core-compression
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

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
               #:cl-gd
               #:cl-argparse)
  :components ((:file "package")
               (:file "keycodes")
               (:file "images")
               (:file "cli")
               (:file "apex7tkl")))
