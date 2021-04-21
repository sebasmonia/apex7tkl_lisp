;;;; package.lisp

(defpackage #:apex7tkl
  (:nicknames "a7t" :a7t)
  (:use #:common-lisp)
  (:import-from :alexandria)
  (:import-from :static-vectors)
  (:export
   #:get-apex7tkl-device))
