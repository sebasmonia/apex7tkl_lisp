;;;; keycodes.lisp

;; I will put this in a separate file for readability, but I THINK
;; that for the time being it doesn't need a new package
(in-package #:apex7tkl)

;; The mapping of keys to codes comes from the original Python code
;; Using "equalp" for the hash table => case insensitive look up
(defparameter *all-keys* (make-hash-table :test 'equalp)"Complete mapping of keys to values to send the keyboard.")
(defparameter *function-keys* (make-hash-table :test #'equalp)"Mapping of F (function) keys to values to send the keyboard.")
(defparameter *alpha* (make-hash-table :test #'equalp)"Mapping of alphabetic keys to values to send the keyboard.")
(defparameter *numeric* (make-hash-table :test #'equalp)"Mapping of numeric keys to values to send the keyboard.")
(defparameter *symbols-left* (make-hash-table :test #'equalp)"Mapping of left-side symbols to values to send the keyboard.")
(defparameter *symbols-right1* (make-hash-table :test #'equalp)"Mapping of right-side symbols to values to send the keyboard.")
(defparameter *symbols-right2* (make-hash-table :test #'equalp)"Mapping of right-side symbols to values to send the keyboard.")


(defun build-key-codes ()
  "Populate all key code hashtables, used to identify keys when sending commands to keyboard."
  ;; function keys
  (loop for key-code from 58 below 70
        for num from 1 below 13
        do (let ((key (uiop:strcat "F" (write-to-string num)) key-code))
             (setf (gethash key *function-keys*) key-code)
             (setf (gethash key *all-keys*) key-code)))
  ;; alpha
  (loop for key-code from 4 below 30
        for char across "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        do (setf (gethash char *alpha*) key-code)
           (setf (gethash char *all-keys*) key-code))
  (setf (gethash "SPACE" *alpha*) key-code)
  (setf (gethash "SPACE" *all-keys*) key-code)
  ;; numeric
  (loop for key-code from 30 below 41
        for char across "1234567890"
        do (setf (gethash char *numeric*) key-code)
           (setf (gethash char *all-keys*) key-code))
  ;; symbols and special keys on the left side of the keyboard
  (loop for key-code in '(41 53 43 57 224 225 226 227)
        for key-name in '("ESC" #\` "TAB" "CAPSLOCK" "CTRL_LEFT" "SHIFT_LEFT" "ALT_LEFT" "WIN_LEFT")
        do (setf (gethash key-name *symbols-left*) key-code)
           (setf (gethash key-name *all-keys*) key-code))
  ;; symbols and special keys to the right of the letters
  (loop for key-code in '(45 46 47 48 49 51 52 54 55 56 228 229 230 231 240)
        for key-name in '("ESC" "`" "TAB" "CAPSLOCK" "CTRL_LEFT" "SHIFT_LEFT" "ALT_LEFT" "WIN_LEFT")
        do (setf (gethash key-name *symbols-left*) key-code)
           (setf (gethash key-name *all-keys*) key-code))




  )

KEY_REGIONS['SYMBOLS_RIGHT1'] = get_key_codes(["BACKSPACE", "RETURN", "-", "=", "[", "]", ";", "'", "\\", ",", ".", "/", "SHIFT_RIGHT", "CTRL_RIGHT", "WIN_RIGHT", "STEEL_META", "ALT_RIGHT"])




KEY_REGIONS = {}
KEY_REGIONS['FKEYS'] = KEYS_FKEYS
KEY_REGIONS['ALPHA'] = KEYS_ALPHA + list(get_key_codes(["SPACE"]))
KEY_REGIONS['NUMERIC'] = KEYS_NUMERIC
KEY_REGIONS['SYMBOLS_LEFT'] = get_key_codes(["ESC", "`", "TAB", "CAPSLOCK", "SHIFT_LEFT", "CTRL_LEFT", "WIN_LEFT", "ALT_LEFT"])
KEY_REGIONS['SYMBOLS_RIGHT1'] = get_key_codes(["BACKSPACE", "RETURN", "-", "=", "[", "]", ";", "'", "\\", ",", ".", "/", "SHIFT_RIGHT", "CTRL_RIGHT", "WIN_RIGHT", "STEEL_META", "ALT_RIGHT"])
KEY_REGIONS['SYMBOLS_RIGHT2'] = get_key_codes(["M1", "M2", "M3", "M4", "M5", "M6", "ARROW_UP", "ARROW_DOWN", "ARROW_LEFT", "ARROW_RIGHT"])
KEY_REGIONS["ALL"] = set(KEYMAP.values())
