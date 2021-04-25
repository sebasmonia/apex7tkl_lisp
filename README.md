# Steelseries Apex 7 TKL on Linux...via Common Lisp

A Common Lisp translation/port of the code at https://github.com/FrankGrimm/apex7tkl_linux.

Uses https://github.com/borodust/claw-usb to consume libusb 1.0

## Usage

After placing both this and claw-usd in a place Quicklisp will find them (for example ~/common-lisp):

```common-lisp
(ql:quickload :apex7tkl)
;; This is a work in progress, the public API is a bit fluid at this time
(apex7tkl::initialize)
;; Make numbers 1~0 blue
(apex7tkl::set-color-region :number 0 #xFF 0)
;; Function keys red
(apex7tkl::set-color-region :f-keys 255 0 0)
;; The whole keyboard purple
(a7t::set-color-region :all 255 #x0 255)

;; More features will be ported from the original Python code *soon*

```
