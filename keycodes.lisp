;;;; keycodes.lisp

;; I will put this in a separate file for readability, but I THINK
;; that for the time being it doesn't need a new package
(in-package #:apex7tkl)

;; The mapping of keys to codes comes from the original Python code
;; Using "equalp" for the hash table => case insensitive look up
(defparameter *all-keys* (make-hash-table :test 'equalp)"Complete mapping of keyboard keys -> values to send the keyboard.")
(defparameter *function-keys* (make-hash-table :test #'equalp) "Mapping of function keys.")
(defparameter *alpha* (make-hash-table :test #'equalp) "Mapping of alphabetic keys.")
(defparameter *number* (make-hash-table :test #'equalp) "Mapping of number keys.")
(defparameter *symbols-left* (make-hash-table :test #'equalp) "Mapping of keys to the left of the alphabet.")
(defparameter *symbols-right1* (make-hash-table :test #'equalp) "Mapping of keys to the right of the alphabet.")
(defparameter *symbols-right2* (make-hash-table :test #'equalp) "Mapping of rightmost keys (arrows + insert, del, etc.).")

(defun build-key-codes ()
  "Populate all key code hashtables, used to identify keys when sending commands to keyboard."
  ;; function keys
  (loop for key-code from 58 below 70
        for num from 1 below 13
        do (let ((key (uiop:strcat "F" (write-to-string num))))
             (setf (gethash key *function-keys*) key-code)
             (setf (gethash key *all-keys*) key-code)))
  ;; alpha
  (loop for key-code from 4 below 30
        for char across "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        do (let ((key (string char)))
             (setf (gethash key *alpha*) key-code)
             (setf (gethash key *all-keys*) key-code)))
  (setf (gethash "SPACE" *alpha*) 44)
  (setf (gethash "SPACE" *all-keys*) 44)
  ;; number
  (loop for key-code from 30 below 41
        for char across "1234567890"
        do (let ((key (string char)))
             (setf (gethash key *number*) key-code)
             (setf (gethash key *all-keys*) key-code)))
  ;; symbols and special keys on the left side of the keyboard
  (loop for key-code in '(41 53 43 57 224 225 226 227)
        for key-name in '("ESC" "`" "TAB" "CAPSLOCK" "CTRL_LEFT" "SHIFT_LEFT" "ALT_LEFT" "WIN_LEFT")
        do (setf (gethash key-name *symbols-left*) key-code)
           (setf (gethash key-name *all-keys*) key-code))
  ;; symbols and special keys to the right of the letters
  (loop for key-code in '(45 46 47 48 49 51 52 54 55 56 228 229 230 231 240 42 40)
        for key-name in '("-" "=" "[" "]" "\\" ";" "'" "," "." "/" "CTRL_RIGHT" "SHIFT_RIGHT" "ALT_RIGHT" "WIN_RIGHT" "STEEL_META" "BACKSPACE" "RETURN")
        do (setf (gethash key-name *symbols-right1*) key-code)
           (setf (gethash key-name *all-keys*) key-code))
  ;; rightmost keys: arrows + pg up, pg down etc.
  (loop for key-code in '(73 74 75 76 77 78 79 80 81 82)
        for key-name in '("INS" "HOME" "PG_UP" "DEL" "END" "PG_DN" "ARROW_RIGHT" "ARROW_LEFT" "ARROW_DOWN" "ARROW_UP")
        do (setf (gethash key-name *symbols-right2*) key-code)
           (setf (gethash key-name *all-keys*) key-code)))

(defun get-region-codes (region-name)
  (alexandria:hash-table-values
   (cond
     ((eq region-name :all) *all-keys*)
     ((eq region-name :alpha) *alpha*)
     ((eq region-name :number) *number*)
     ((eq region-name :f-keys) *function-keys*)
     ((eq region-name :symbols-left) *symbols-left*)
     ((eq region-name :symbols-right1) *symbols-right1*)
     ((eq region-name :symbols-right2) *symbols-right2*)
     (t (error "Invalid region-name. Must be one of: :alpha :number :f-keys :symbols-left :symbols-right1 :symbols-right2 :all")))))

(defun get-key-code-by-name (key-name)
  "Get the value to send to the keyboard to refer to KEY-NAME."
  (let ((code (gethash key-name *all-keys*)))
    (unless code
      (error "Invalid key-name: ~a" key-name))
    code))
