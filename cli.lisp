;;;; cli.lisp

(in-package #:apex7tkl)

(defparameter *test-args* nil "Arguments used when `uiop:command-line-arguments' is empty, for testing.")

(defun cli-color (cli-arguments)
  "Change key colors in the keyboard using the parameters in CLI-ARGUMENTS."
  (let ((region (alexandria:assoc-value cli-arguments "region" :test 'string=))
        (red (alexandria:assoc-value cli-arguments "red" :test 'string=))
        (blue (alexandria:assoc-value cli-arguments "blue" :test 'string=))
        (green (alexandria:assoc-value cli-arguments "green" :test 'string=)))
    (set-color-region (alexandria:make-keyword (string-upcase region))
                      (parse-integer red)
                      (parse-integer green)
                      (parse-integer blue))))

(defun cli-image (cli-arguments)
  "Display an image in the oled screen using the parameters in CLI-ARGUMENTS."
  (let ((image-path (uiop:native-namestring
                     (alexandria:assoc-value cli-arguments "path" :test 'string=))))
    (if (probe-file image-path)
        (display-image image-path)
        (progn
          (format t "~a doesn't exist.~%" image-path);; error message
          1)))) ;; returned exit code

(defun cli-text (cli-arguments)
  "Display text in the oled screen using the parameters in CLI-ARGUMENTS."
  (let ((lines (list
                (alexandria:assoc-value cli-arguments "line-1" :test 'string=)
                (alexandria:assoc-value cli-arguments "line-2" :test 'string=)
                (alexandria:assoc-value cli-arguments "line-3" :test 'string=))))
    (if (notevery (lambda (line) (= (length line) 0)) lines)
        (display-text lines)
        (progn
          (format t "No text provided. Check out \"apex7tkl text -h\".~%");; error message
          1)))) ;; returned exit code

(defun cli-config (cli-arguments)
  "Change the keyboard config using the parameters in CLI-ARGUMENTS"
  (set-config (parse-integer (alexandria:assoc-value cli-arguments "config-number" :test 'string=))))

(defparameter *arg-parser*
  (cl-argparse:create-main-parser
      (main-parser "Interact with the SteelSeries Apex 7 TKL keyboard using the CLI.
There are four commands: color, image, text and config. You can use \"apex7tkl [command]\" for more details."))
  "The main argument parser.")

(cl-argparse:add-subparser
 *arg-parser*
 (cl-argparse:create-sub-parser (color "Paints a keyboard \"region\" with the R G B values specified.")
   (cl-argparse:add-default color
                             :var "command"
                             :default #'cli-color)
   (cl-argparse:add-positional color
                               :name "region"
                               :help "Possible values are: alpha, number, f-keys, symbols-left, symbols-right1, symbols-right2, all")
   (cl-argparse:add-positional color
                               :name "red"
                               :help "A value between 0 and 255.")
   (cl-argparse:add-positional color
                               :name "green"
                               :help "A value between 0 and 255.")
   (cl-argparse:add-positional color
                               :name "blue"
                               :help "A value between 0 and 255.")))

(cl-argparse:add-subparser
 *arg-parser*
 (cl-argparse:create-sub-parser (image "Display an image in the OLED screen. Should be 128x40 and black and white only. If a color image is used, it will be displayed using the average color as threshold for b/w.")
   (cl-argparse:add-default image
                             :var "command"
                             :default #'cli-image)
   (cl-argparse:add-positional image
                               :name "path"
                               :help "Path to the image.")))

(cl-argparse:add-subparser
 *arg-parser*
 (cl-argparse:create-sub-parser (text "Display text in the OLED screen. Up to three lines of text can be displayed.
The screen fits 21 characters, your lines can be longer than that.")
   (cl-argparse:add-default text
                             :var "command"
                             :default #'cli-text)
   (cl-argparse:add-optional text
                             :var "line-3"
                             :short "3"
                             :long "line-3"
                             :default ""
                             :help "Text for the third line.")
   (cl-argparse:add-optional text
                             :var "line-2"
                             :short "2"
                             :long "line-2"
                             :default ""
                             :help "Text for the second line.")
   (cl-argparse:add-optional text
                             :var "line-1"
                             :short "1"
                             :long "line-1"
                             :default ""
                             :help "Text for the first line.")))

(cl-argparse:add-subparser
 *arg-parser*
 (cl-argparse:create-sub-parser (config "Change to the N config for the keyboard. Valid configs are in the range 1-5.")
   (cl-argparse:add-default config
                             :var "command"
                             :default #'cli-config)
   (cl-argparse:add-positional config
                               :name "config-number"
                               :help "The config number, 1~5.")))

(defun main ()
  ;; Don't "debug" when running from the CLI
  (let ((exit-code 0))
    (when (uiop:command-line-arguments)
      (setf *debug* nil))
    (handler-case
        (let ((cli-arguments (cl-argparse:parse *arg-parser* (or (uiop:command-line-arguments) *test-args*))))
          (initialize)
          (setf exit-code
                (funcall (cl-argparse:get-value "command" cli-arguments)
                         (cl-argparse:get-key-value-pairs cli-arguments)))
          (debug-message "Exit code: ~a~%" exit-code))
      (cl-argparse:cancel-parsing-error (e)
        (setf exit-code 1)
        (format t "~a~%" e))
      (error (e)
        (setf exit-code 1)
        (format t "ERROR: ~a~%" e)))
    ;; Only "quit" when running from the CLI
    (when (uiop:command-line-arguments)
      (uiop:quit exit-code))))
