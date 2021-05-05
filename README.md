# SteelSeries Apex 7 TKL on Linux, via a Common Lisp CLI tool

A Common Lisp translation/port of the code at https://github.com/FrankGrimm/apex7tkl_linux.

Uses https://github.com/borodust/claw-usb to consume libusb 1.0.

## Usage

### Dependencies

- claw-usb: you need to download it from the repo linked above.
- cl-gd: it is available in Quicklisp, but there's a manual setup step, see https://edicl.github.io/cl-gd/#install
- UIOP
- Alexandria


After having all the code that needs manual setup in a place that Quicklisp can find it, for example `~/common-lisp`:

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

I figured the CLI would be simple, which is true...but by not using a library for options, the code for 
it ain't pretty.   
Included `build.sh` as a little helper to dump an executable image.  
  
There are four commands: color, image, text and config. Use "help" to get a general overview not unlike the one below.  
  
You can also use "[command] help" for more details on each command.

### color

Paints a keyboard "region" with the RGB values specified.  
Valid regions are: alpha, number, f-keys, symbols-left, symbols-right1, symbols-right2, all
For each color, specify a value between 0 and 255.  
Example, to make all numbers green, `apex7tkl color numbers 0 255 0`

### image

Shows an image in the OLED screen. It should be 128x40, black/white only, but you can try a color image.
Any format that GD can open, should work.  
`apex7tkl image path/to/image.png`

### text

Up to three lines of text can be displayed. The screen fits 21 characters, your lines can be longer than that, the rest is sent to the keyboard but won't show.  
The line argumetns  are naturally separated by spaces, so surround your text in quotes or use \ to escape them as needed, for example: `apex7tkl text "line 1" line\ 2 Third-line-no-spaces`

### config

Change to one of the five configurations that can be preset in the keyboard.  
`apex7tkl config 1` (or 2, 3, 4 ,5)  
  
This is also an easy way to reset colors & screen :)
