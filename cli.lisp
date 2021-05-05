;;;; cli.lisp

(in-package #:apex7tkl)

(defparameter *test-args* nil "Arguments used when `uiop:command-line-arguments' is empty, for testing.")

(defparameter +help-text+
  "Usage: apex7tkl [command] [argument(s)]

Interact with the SteelSeries Apex 7 TKL keyboard using the CLI.
There are four commands: color, image, text and config. You can use \"[command] help\" for more details.

Examples:
-Change the color of the F1~F12 keys to purple
    apex7tkl color f-keys 255 0 255

-Display an image in the OLED screen:
    apex7tkl image path/to/image.png

-Write text to the OLED screen:
    apex7tkl text \"line 1\" line_2 third\\ line

-Change to a predefined configuration:
    apex7tkl config 2

")

(defparameter +help-color+
  "Usage: apex7tkl color [region name] [red] [green] [blue]

Paints a keyboard \"region\" with the RGB values specified.
Valid regions are: alpha, number, f-keys, symbols-left, symbols-right1, symbols-right2, all
For each color specify a value between 0 and 255.

Examples:
-Change the color of the F1~F12 keys to purple
    apex7tkl color f-keys 255 0 255

-Make the rightmost keys green:
    apex7tkl color symbols-right2 0 0 255

-Make all the letters into...whatever that color will be
    apex7tkl color alpha 160 200 80

")

(defparameter +help-image+
  "Usage: apex7tkl image path/to/image.png

Image should be 128x40, black/white only. If a color image is used, an attempt will be made
to show it by using the average color value as threshold for black/white.

")

(defparameter +help-text-oled+
  "Usage: apex7tkl text line-1 line-2 line-3

Up to three lines of text can be displayed. The screen fits 21 characters, your lines can
be longer than that.
You have to \"use quotes\" or escape spaces with \\ if you need to include spaces in the
text to display.

-Example of displaying three lines, with different escaping for spaces:
    apex7tkl text \"line 1\" line_2 third\\ line

")

(defparameter +help-config+
  "Usage: apex7tkl config N

Change to the N config for the keyboard. Valid configs are in the range 1-5.

")

;; I used and liked Adopt for a work-related executable, but for this I thought I didn't
;; need anything as complex. It turned out into a mess of if/cond. I got by just because
;; it is relatively simple...Anyway, some of code still resembles this source:
;; https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/
(defun main ()
  "Entry point for the executable CLI tool."
  (handler-case
      (let* ((params (or (uiop:command-line-arguments) *test-args*))
             (command (first params))
             (command-args (rest params))
             (is-help (string= (first command-args) "help")))
        (setf *debug* (member "debug" params :test 'string=))
        (initialize)
        (cond ((string= command "help")
               (format *standard-output* "~a" +help-text+))
              ((string= command "color")
               (if is-help
                   (format *standard-output* "~a" +help-color+)
                   (set-color-region (alexandria:make-keyword (string-upcase (first command-args)))
                                     (parse-integer (second command-args))
                                     (parse-integer (third command-args))
                                     (parse-integer (fourth command-args)))))
              ((string= command "image")
               (if is-help
                   (format *standard-output* "~a" +help-image+)
                   (display-image (first command-args) t)))
              ((string= command "text")
               (if is-help
                   (format *standard-output* "~a" +help-text-oled+)
                   ;; I hope no one ever wants to print "help" in the first line >_>
                   (display-text command-args)))
              ((string= command "config")
               (if is-help
                   (format *standard-output* "~a" +help-config+)
                   (set-config (parse-integer (first command-args)))))
              (t (format *standard-output*  "Bad parameters. Use \"apex7tkl help\" for usage.~%~%"))))
    (error (c)
      (format *standard-output* "ERROR:  ~a~%~%Use \"apex7tkl help\" for usage.~%" c))))
