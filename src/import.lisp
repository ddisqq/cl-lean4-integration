;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-lean4-integration - Import (Lean4 -> CL)
;;;;
;;;; Functions for importing and parsing Lean4 proofs and results.

(in-package #:cl-lean4-integration)

;;; ============================================================================
;;; Main Import Interface
;;; ============================================================================

(defun import-proof (file-path)
  "Import a Lean4 proof from a file.

   FILE-PATH: Path to .lean file

   Returns a lean4-result struct."
  (unless (probe-file file-path)
    (return-from import-proof
      (make-lean4-result
       :success-p nil
       :errors (list (format nil "File not found: ~A" file-path)))))

  (let ((content (uiop:read-file-string file-path)))
    (import-verification-result content)))

(defun import-theorem (lean4-code)
  "Parse a Lean4 theorem definition.

   LEAN4-CODE: String containing Lean4 theorem code

   Returns a lean4-theorem struct."
  (let ((name nil)
        (statement nil)
        (proof nil)
        (hypotheses nil))

    ;; Extract theorem name
    (let ((name-match (search "theorem " lean4-code)))
      (when name-match
        (let* ((start (+ name-match 8))
               (end (or (position #\Space lean4-code :start start)
                        (position #\Newline lean4-code :start start)
                        (position #\: lean4-code :start start))))
          (setf name (subseq lean4-code start end)))))

    ;; Extract statement (between : and := or :=)
    (let ((colon-pos (position #\: lean4-code)))
      (when colon-pos
        (let ((by-pos (search " by" lean4-code :start2 colon-pos))
              (assign-pos (search ":=" lean4-code :start2 colon-pos)))
          (when (or by-pos assign-pos)
            (setf statement
                  (string-trim '(#\Space #\Newline #\Tab)
                               (subseq lean4-code (1+ colon-pos)
                                       (or by-pos assign-pos))))))))

    ;; Extract proof (after := by or := )
    (let ((by-pos (search " by" lean4-code)))
      (when by-pos
        (setf proof (string-trim '(#\Space #\Newline #\Tab)
                                 (subseq lean4-code (+ by-pos 3))))))

    (make-lean4-theorem
     :name (or name "unknown")
     :statement statement
     :hypotheses hypotheses
     :proof proof
     :status :pending)))

(defun import-verification-result (lean4-output)
  "Parse Lean4 verification output into a result struct.

   LEAN4-OUTPUT: String containing Lean4 output

   Returns a lean4-result struct."
  (let ((errors nil)
        (warnings nil)
        (success-p t))

    ;; Parse for errors
    (dolist (line (split-string lean4-output #\Newline))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
        (cond
          ;; Error line
          ((or (search "error:" trimmed :test #'char-equal)
               (search "Error:" trimmed))
           (push trimmed errors)
           (setf success-p nil))

          ;; Warning line
          ((or (search "warning:" trimmed :test #'char-equal)
               (search "Warning:" trimmed))
           (push warnings trimmed))

          ;; Sorry found (incomplete proof)
          ((search "sorry" trimmed :test #'char-equal)
           (push "Proof contains 'sorry' (incomplete)" warnings)))))

    (make-lean4-result
     :success-p success-p
     :output lean4-output
     :errors (nreverse errors)
     :warnings (nreverse warnings))))

;;; ============================================================================
;;; Lean4 Output Parsing
;;; ============================================================================

(defun parse-lean4-output (output)
  "Parse raw Lean4 output into structured diagnostics.

   OUTPUT: String of Lean4 output

   Returns list of diagnostics (plists)."
  (let ((diagnostics nil)
        (current-diag nil))

    (dolist (line (split-string output #\Newline))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
        (cond
          ;; New diagnostic line (file:line:col: severity: message)
          ((and (> (length trimmed) 0)
                (or (search "error:" trimmed :test #'char-equal)
                    (search "warning:" trimmed :test #'char-equal)
                    (search "info:" trimmed :test #'char-equal)))
           ;; Save previous diagnostic
           (when current-diag
             (push current-diag diagnostics))
           ;; Start new diagnostic
           (setf current-diag (parse-diagnostic-line trimmed)))

          ;; Continuation of previous diagnostic
          ((and current-diag (> (length trimmed) 0))
           (setf (getf current-diag :message)
                 (concatenate 'string
                              (getf current-diag :message)
                              (string #\Newline)
                              trimmed))))))

    ;; Don't forget last diagnostic
    (when current-diag
      (push current-diag diagnostics))

    (nreverse diagnostics)))

(defun parse-diagnostic-line (line)
  "Parse a single diagnostic line.

   LINE: String like 'file.lean:10:5: error: message'

   Returns plist with :file :line :column :severity :message."
  (let ((colon-count 0)
        (segments nil)
        (start 0))

    ;; Split by colons (but only first 4)
    (loop for i from 0 below (length line)
          for char = (char line i)
          when (and (char= char #\:) (< colon-count 4))
            do (push (subseq line start i) segments)
               (incf colon-count)
               (setf start (1+ i)))

    ;; Add remaining as message
    (push (string-trim '(#\Space) (subseq line start)) segments)
    (setf segments (nreverse segments))

    ;; Build plist
    (list :file (first segments)
          :line (parse-integer (or (second segments) "0") :junk-allowed t)
          :column (parse-integer (or (third segments) "0") :junk-allowed t)
          :severity (intern (string-upcase
                             (string-trim '(#\Space)
                                          (or (fourth segments) "info")))
                            :keyword)
          :message (or (fifth segments) ""))))

(defun parse-lean4-error (error-output)
  "Parse Lean4 error output.

   ERROR-OUTPUT: String containing error messages

   Returns list of error message strings."
  (let ((errors nil))
    (dolist (diag (parse-lean4-output error-output))
      (when (eq (getf diag :severity) :error)
        (push (getf diag :message) errors)))
    (nreverse errors)))

(defun parse-lean4-diagnostics (output)
  "Parse all diagnostics from Lean4 output.

   OUTPUT: String of Lean4 output

   Returns list of diagnostic plists."
  (parse-lean4-output output))

;;; ============================================================================
;;; Type Conversion (Lean4 -> Lisp)
;;; ============================================================================

(defun lean4-to-lisp-type (lean4-type)
  "Convert a Lean4 type string to Common Lisp type.

   LEAN4-TYPE: String like 'Int', 'List Nat', etc.

   Returns a CL type specifier."
  (let ((trimmed (string-trim '(#\Space #\Tab) lean4-type)))
    (cond
      ;; Basic types
      ((string-equal trimmed "Int") 'integer)
      ((string-equal trimmed "Nat") '(integer 0 *))
      ((string-equal trimmed "Float") 'float)
      ((string-equal trimmed "String") 'string)
      ((string-equal trimmed "Char") 'character)
      ((string-equal trimmed "Bool") 'boolean)
      ((string-equal trimmed "True") 't)
      ((string-equal trimmed "False") 'nil)
      ((string-equal trimmed "Unit") 'null)

      ;; Parameterized types
      ((string-starts-with-p "List " trimmed)
       `(list ,(lean4-to-lisp-type (subseq trimmed 5))))
      ((string-starts-with-p "Array " trimmed)
       `(vector ,(lean4-to-lisp-type (subseq trimmed 6))))
      ((string-starts-with-p "Option " trimmed)
       `(or null ,(lean4-to-lisp-type (subseq trimmed 7))))

      ;; Function types (A -> B)
      ((search " -> " trimmed)
       (let ((parts (split-string trimmed #\ )))
         (when (member "->" parts :test #'string=)
           'function)))

      ;; Default
      (t t))))

(defun lean4-to-lisp-expr (lean4-expr)
  "Convert a Lean4 expression to Common Lisp form.

   LEAN4-EXPR: String containing Lean4 expression

   Returns a Lisp form."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) lean4-expr)))
    (cond
      ;; Boolean literals
      ((string-equal trimmed "true") t)
      ((string-equal trimmed "false") nil)
      ((string-equal trimmed "True") t)
      ((string-equal trimmed "False") nil)

      ;; Numeric literals
      ((every #'digit-char-p trimmed)
       (parse-integer trimmed))
      ((and (> (length trimmed) 0)
            (char= (char trimmed 0) #\-)
            (every #'digit-char-p (subseq trimmed 1)))
       (parse-integer trimmed))

      ;; String literals
      ((and (> (length trimmed) 1)
            (char= (char trimmed 0) #\")
            (char= (char trimmed (1- (length trimmed))) #\"))
       (subseq trimmed 1 (1- (length trimmed))))

      ;; Logical operators
      ((search "/\\" trimmed)
       (let ((parts (split-by-operator trimmed "/\\")))
         `(and ,(lean4-to-lisp-expr (first parts))
               ,(lean4-to-lisp-expr (second parts)))))
      ((search "\\/" trimmed)
       (let ((parts (split-by-operator trimmed "\\/")))
         `(or ,(lean4-to-lisp-expr (first parts))
              ,(lean4-to-lisp-expr (second parts)))))

      ;; Quantifiers
      ((string-starts-with-p "forall " trimmed)
       (let* ((comma-pos (position #\, trimmed))
              (var (string-trim '(#\Space) (subseq trimmed 7 comma-pos)))
              (body (subseq trimmed (1+ comma-pos))))
         `(forall ,(lean4-name-to-lisp var)
                  ,(lean4-to-lisp-expr body))))
      ((string-starts-with-p "exists " trimmed)
       (let* ((comma-pos (position #\, trimmed))
              (var (string-trim '(#\Space) (subseq trimmed 7 comma-pos)))
              (body (subseq trimmed (1+ comma-pos))))
         `(exists ,(lean4-name-to-lisp var)
                  ,(lean4-to-lisp-expr body))))

      ;; Default - return as symbol or string
      (t (if (alpha-char-p (char trimmed 0))
             (intern (lean4-name-to-lisp trimmed))
             trimmed)))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun split-by-operator (expr operator)
  "Split expression by binary operator, respecting parentheses.

   Returns list of two parts or nil if operator not found."
  (let ((depth 0)
        (op-len (length operator)))
    (loop for i from 0 to (- (length expr) op-len)
          for char = (char expr i)
          do (cond
               ((char= char #\() (incf depth))
               ((char= char #\)) (decf depth))
               ((and (zerop depth)
                     (string= operator expr :start2 i :end2 (+ i op-len)))
                (return (list (string-trim '(#\Space #\( #\))
                                           (subseq expr 0 i))
                              (string-trim '(#\Space #\( #\))
                                           (subseq expr (+ i op-len))))))))))
