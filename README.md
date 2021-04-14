# Steelseries Apex 7 TKL on Linux...via Common Lisp

A Common Lisp translation/port of the code at https://github.com/FrankGrimm/apex7tkl_linux.

Uses https://github.com/soemraws/cl-libusb.

## Usage

After placing the library in a place Quicklisp will find it...

```common-lisp
(ql:quickload :apex7tkl)
(defparameter *keyb* (a7t:get-apex7tkl-device))

```
