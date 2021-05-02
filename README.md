# SteelSeries Apex 7 TKL on Linux, via a Common Lisp CLI tool

A Common Lisp translation/port of the code at https://github.com/FrankGrimm/apex7tkl_linux.

Uses https://github.com/borodust/claw-usb to consume libusb 1.0.

## Usage

### Dependencies

- claw-usb: you need to download it from the repo linked above.
- cl-gd: it is available in Quicklisp, but there's a manual setup step, see https://edicl.github.io/cl-gd/#install
- adopt (in Quicklisp)
- alexandria (in Quicklisp)


After having all the code that needs manual setup in place that Quicklisp can find it, for example `~/common-lisp`:

```common-lisp
(ql:quickload :apex7tkl)
;; You need to call initialize to load libusb and run some other init code
(apex7tkl:initialize)

;;; COLORS

;; Make numbers 1~0 blue
(apex7tkl:set-color-region :number 0 #xFF 0)
;; Function keys red
(apex7tkl:set-color-region :f-keys 255 0 0)
;; The whole keyboard purple
(a7t:set-color-region :all 255 0 255)

;;; OLED

;; Display an image. Images need to be 128x40 with black/white only
(apex7tkl:display-image "/path/to/my-image.png")
;; If an image has more colors, we can attempt to display something by using the average color
;; as a treshold:
(apex7tkl:display-image "/path/to/a-color-image.png" t)
;; Display text in the screen, using the font included in the GD library
;; Up to 3 lines can be passed to display-text, the rest are ignored:
(apex7tkl:display-text '("line 1" "line 2" "line 3"))

;;; CONFIG
;; Select one of the pre-defined configs (1 thru 5)
(apex7tkl:set-config 1)

```


## CLI Usage

-Coming Soon-
