# SteelSeries Apex 7 TKL on Linux, via a Common Lisp CLI tool

A Common Lisp translation/port of the code at https://github.com/FrankGrimm/apex7tkl_linux.

Uses https://github.com/borodust/claw-usb to consume libusb 1.0.

## Usage

### Dependencies

- claw-usb: you need to download it from the repo linked above.
- cl-gd: it is available in Quicklisp, but there's a manual setup step, see https://edicl.github.io/cl-gd/#install
- cl-argparse: although it is in Quicklisp, you'll need to use my fork, it adds an initialization parameter. Submitted a PR, so hopefully this disclaimer goes away soon!
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

The v2 of the CLI uses the excellent [cl-argparse](https://github.com/simkoc/cl-argparse) to declare the UI. I forked it to add a parameter to customize the "program name" in the help text.  

Included `build.sh` as a little helper to dump an executable image.  

There are four commands: color, image, text and config. Use `apex7tkl [command] -h` to get help.  

### color

Paints a keyboard "region" with the RGB values specified.  
Valid regions are: alpha, number, f-keys, symbols-left, symbols-right1, symbols-right2, all.  
For each color, specify a value between 0 and 255.  
&nbsp;&nbsp;  
`apex7tkl color numbers 0 255 0`
&nbsp;&nbsp;  
Will make all numbers 1~0 in the top row, green

### image

Shows an image in the OLED screen. It should be 128x40, black/white only, but you can try a color image.
Any format that GD can open, should work.  
&nbsp;&nbsp;  
`apex7tkl image path/to/image.png`

### text

Up to three lines of text can be displayed. The screen fits 21 characters, your lines can be longer than that, the rest is sent to the keyboard but won't show.  
The line arguments (`--line-1` or `-1` for line 1, etc) are naturally separated by spaces, so surround your text in quotes or use \ to escape them as needed.  
With the move to cl-argparse, the format of this command changed:  
&nbsp;&nbsp;  
`apex7tkl text -1 "line 1" -3 this\ is\ escaped\ too`
&nbsp;&nbsp;  
Will print in the first and third line, leaving the second line blank.

### config

Change to one of the five configurations that can be preset in the keyboard.  
&nbsp;&nbsp;  
`apex7tkl config 1` (or 2, 3, 4 ,5)  
&nbsp;&nbsp;  
This is also an easy way to reset colors & screen :)
