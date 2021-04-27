;;;; package.lisp

(defpackage #:apex7tkl
  (:nicknames "a7t" :a7t)
  (:use #:common-lisp)
  (:import-from :alexandria)
  (:import-from :static-vectors)
  (:export
   #:initialize
   #:send-control-message
   #:set-color-region
   #:set-color-keys
   #:set-config))
