;;;; cl-lean4-integration - Utility Functions
;;;;
;;;; Helper functions for name mangling, case conversion, and general utilities.

(in-package #:cl-lean4-integration)

;;; ============================================================================
;;; Name Conversion Utilities
;;; ============================================================================

(defun lisp-name-to-lean4 (name)
  "Convert a Lisp symbol name to a valid Lean4 identifier.

   Converts kebab-case to snake_case and handles special characters.
   Examples:
     'validate-block -> \"validate_block\"
     'get-utxo-set   -> \"get_utxo_set\"
     '*global-var*   -> \"global_var\""
  (let* ((str (etypecase name
                (symbol (symbol-name name))
                (string name)))
         (lowercase (string-downcase str)))
    ;; Remove earmuffs (*foo*) and convert hyphens to underscores
    (let ((result (substitute #\_ #\-
                              (if (and (> (length lowercase) 2)
                                       (char= (char lowercase 0) #\*)
                                       (char= (char lowercase (1- (length lowercase))) #\*))
                                  (subseq lowercase 1 (1- (length lowercase)))
                                  lowercase))))
      ;; Ensure it doesn't start with a digit
      (if (and (> (length result) 0)
               (digit-char-p (char result 0)))
          (concatenate 'string "n_" result)
          result))))

(defun lean4-name-to-lisp (name)
  "Convert a Lean4 identifier to a Lisp symbol name.

   Converts snake_case to kebab-case.
   Examples:
     \"validate_block\" -> \"VALIDATE-BLOCK\"
     \"get_utxo_set\"   -> \"GET-UTXO-SET\""
  (string-upcase (substitute #\- #\_ name)))

(defun camel-case (name)
  "Convert a Lisp symbol name to camelCase.

   Examples:
     'validate-block -> \"validateBlock\"
     'get-utxo-set   -> \"getUtxoSet\""
  (let* ((str (etypecase name
                (symbol (symbol-name name))
                (string name)))
         (lowercase (string-downcase str))
         (result (make-string-output-stream))
         (capitalize-next nil))
    (loop for char across lowercase
          do (cond
               ((char= char #\-)
                (setf capitalize-next t))
               (capitalize-next
                (write-char (char-upcase char) result)
                (setf capitalize-next nil))
               (t
                (write-char char result))))
    (get-output-stream-string result)))

(defun kebab-to-camel (name)
  "Alias for camel-case."
  (camel-case name))

(defun mangle-name (name &key (prefix "") (suffix ""))
  "Mangle a Lisp name for use in Lean4 with optional prefix/suffix.

   Examples:
     (mangle-name 'foo :prefix \"CLPIC_\")     -> \"CLPIC_foo\"
     (mangle-name 'bar :suffix \"_theorem\")   -> \"bar_theorem\""
  (concatenate 'string
               prefix
               (lisp-name-to-lean4 name)
               suffix))

(defun unmangle-name (mangled-name &key (prefix "") (suffix ""))
  "Reverse the mangling process to recover original Lisp name.

   Returns a string suitable for interning."
  (let* ((without-prefix (if (and (> (length prefix) 0)
                                   (string= prefix mangled-name
                                            :end2 (min (length prefix)
                                                       (length mangled-name))))
                              (subseq mangled-name (length prefix))
                              mangled-name))
         (without-suffix (if (and (> (length suffix) 0)
                                   (string= suffix without-prefix
                                            :start2 (max 0 (- (length without-prefix)
                                                              (length suffix)))))
                              (subseq without-prefix 0
                                      (- (length without-prefix) (length suffix)))
                              without-prefix)))
    (lean4-name-to-lisp without-suffix)))

;;; ============================================================================
;;; String Utilities
;;; ============================================================================

(defun string-starts-with-p (prefix string)
  "Check if STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun string-ends-with-p (suffix string)
  "Check if STRING ends with SUFFIX."
  (and (>= (length string) (length suffix))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(defun split-string (string delimiter)
  "Split STRING by DELIMITER character."
  (loop with result = nil
        with start = 0
        for pos = (position delimiter string :start start)
        while pos
        do (push (subseq string start pos) result)
           (setf start (1+ pos))
        finally (push (subseq string start) result)
                (return (nreverse result))))

(defun join-strings (strings &optional (delimiter ""))
  "Join STRINGS with DELIMITER."
  (with-output-to-string (s)
    (loop for (str . rest) on strings
          do (write-string str s)
             (when rest (write-string delimiter s)))))

;;; ============================================================================
;;; Logging Utilities
;;; ============================================================================

(defvar *lean4-log-level* :info
  "Log level for Lean4 operations: :debug :info :warn :error")

(defun log-message (level format-string &rest args)
  "Log a message if LEVEL meets the current log level."
  (let ((levels '(:debug 0 :info 1 :warn 2 :error 3)))
    (when (>= (getf levels level 0) (getf levels *lean4-log-level* 1))
      (format *trace-output* "~&[LEAN4 ~A] ~?~%"
              level format-string args))))

;;; ============================================================================
;;; Time Utilities
;;; ============================================================================

(defun get-elapsed-ms (start-time)
  "Get elapsed time in milliseconds since START-TIME."
  (round (* 1000 (/ (- (get-internal-real-time) start-time)
                     internal-time-units-per-second))))

(defmacro with-timing ((var) &body body)
  "Execute BODY and bind elapsed time in milliseconds to VAR."
  (let ((start (gensym "START")))
    `(let ((,start (get-internal-real-time)))
       (prog1 (progn ,@body)
         (setf ,var (get-elapsed-ms ,start))))))
