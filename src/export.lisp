;;;; cl-lean4-integration - Export (CL -> Lean4)
;;;;
;;;; Functions for translating Common Lisp specifications and code to Lean4.

(in-package #:cl-lean4-integration)

;;; ============================================================================
;;; Main Export Interface
;;; ============================================================================

(defun export-specification (spec &key (stream nil) (module-name "CLPIC.Spec"))
  "Export a specification to Lean4 code.

   SPEC: A specification struct
   STREAM: Output stream (nil returns string)
   MODULE-NAME: Lean4 module namespace

   Returns Lean4 source code as a string."
  (let ((output (or stream (make-string-output-stream))))
    ;; Header
    (format output "~A" (generate-lean4-header
                         :title (format nil "Specification: ~A"
                                        (specification-name spec))
                         :module-name module-name))

    ;; Main theorem
    (format output "~A" (generate-lean4-theorem spec))

    ;; Footer
    (format output "~%end ~A~%" module-name)

    (if stream
        nil
        (get-output-stream-string output))))

(defun export-function (fn-name &key (stream nil) (module-name "CLPIC.Functions"))
  "Export a function definition to Lean4.

   FN-NAME: Symbol naming the function to export
   STREAM: Output stream (nil returns string)
   MODULE-NAME: Lean4 module namespace

   Returns Lean4 source code."
  (unless (fboundp fn-name)
    (error 'translation-error
           :form fn-name
           :message (format nil "Function ~A is not defined" fn-name)))

  (let ((output (or stream (make-string-output-stream)))
        (lambda-list (function-lambda-list fn-name)))
    ;; Header
    (format output "~A" (generate-lean4-header
                         :title (format nil "Function: ~A" fn-name)
                         :module-name module-name))

    ;; Function definition placeholder
    (format output "/-- ~A -/~%" (or (documentation fn-name 'function) "No documentation"))
    (format output "def ~A" (lisp-name-to-lean4 fn-name))

    ;; Parameters
    (when lambda-list
      (format output " ~{(~A : Any)~^ ~}"
              (mapcar #'lisp-name-to-lean4
                      (remove-if (lambda (x) (member x lambda-list-keywords))
                                 lambda-list))))

    (format output " : Any := sorry~%")
    (format output "~%end ~A~%" module-name)

    (if stream
        nil
        (get-output-stream-string output))))

(defun export-theorem (theorem &key (stream nil))
  "Export a lean4-theorem struct to Lean4 code.

   THEOREM: A lean4-theorem struct
   STREAM: Output stream (nil returns string)

   Returns Lean4 source code."
  (let ((output (or stream (make-string-output-stream))))
    (format output "theorem ~A" (lean4-theorem-name theorem))

    ;; Hypotheses
    (when (lean4-theorem-hypotheses theorem)
      (format output "~%  ~{~A~%  ~}"
              (lean4-theorem-hypotheses theorem)))

    ;; Statement
    (format output " :~%  ~A := by~%"
            (or (lean4-theorem-statement theorem) "True"))

    ;; Proof
    (format output "  ~A~%"
            (or (lean4-theorem-proof theorem) "sorry"))

    (if stream
        nil
        (get-output-stream-string output))))

;;; ============================================================================
;;; Translation Functions
;;; ============================================================================

(defun translate-to-lean4 (form &key (context nil))
  "Translate a Lisp form to Lean4 syntax.

   FORM: Any Lisp form
   CONTEXT: Optional translation context

   Returns Lean4 code as a string."
  (declare (ignore context))
  (cond
    ;; Nil
    ((null form) "True")

    ;; Boolean
    ((eq form t) "True")

    ;; Symbol
    ((symbolp form) (lisp-name-to-lean4 form))

    ;; Number
    ((numberp form) (format nil "~A" form))

    ;; String
    ((stringp form) (format nil "\"~A\"" form))

    ;; List/Expression
    ((consp form)
     (lisp-expr-to-lean4 form))

    ;; Other
    (t
     (error 'translation-error
            :form form
            :message "Cannot translate form to Lean4"))))

(defun translate-type (lisp-type)
  "Translate a Common Lisp type to Lean4 type.

   LISP-TYPE: A type specifier

   Returns Lean4 type as a string."
  (cond
    ;; Simple type symbol
    ((symbolp lisp-type)
     (get-type-mapping lisp-type))

    ;; Compound type (e.g., (list integer))
    ((and (consp lisp-type) (eq (car lisp-type) 'list))
     (format nil "List ~A" (translate-type (or (second lisp-type) 'any))))

    ;; Array type
    ((and (consp lisp-type) (member (car lisp-type) '(vector array)))
     (format nil "Array ~A" (translate-type (or (second lisp-type) 'any))))

    ;; Function type
    ((and (consp lisp-type) (eq (car lisp-type) 'function))
     (let ((arg-types (second lisp-type))
           (return-type (third lisp-type)))
       (format nil "~{~A~^ -> ~} -> ~A"
               (mapcar #'translate-type (or arg-types '(any)))
               (translate-type (or return-type 'any)))))

    ;; Optional type
    ((and (consp lisp-type) (eq (car lisp-type) 'or)
          (member 'null (cdr lisp-type)))
     (let ((non-null (remove 'null (cdr lisp-type))))
       (format nil "Option ~A" (translate-type (car non-null)))))

    ;; Default
    (t "Any")))

(defun translate-expr (expr)
  "Translate a Lisp expression to Lean4."
  (lisp-expr-to-lean4 expr))

(defun lisp-expr-to-lean4 (expr)
  "Convert a Lisp expression to Lean4 syntax.

   Handles:
   - Arithmetic: + - * /
   - Comparisons: < > <= >= = /=
   - Logical: and or not
   - Quantifiers: forall exists
   - Function application"
  (cond
    ;; Nil/T
    ((null expr) "True")
    ((eq expr t) "True")

    ;; Atom
    ((atom expr)
     (cond
       ((symbolp expr) (lisp-name-to-lean4 expr))
       ((numberp expr) (format nil "~A" expr))
       ((stringp expr) (format nil "\"~A\"" expr))
       (t (format nil "~S" expr))))

    ;; List/compound expression
    ((consp expr)
     (case (first expr)
       ;; Arithmetic
       ((+ - * /)
        (format nil "(~A ~A ~A)"
                (lisp-expr-to-lean4 (second expr))
                (first expr)
                (lisp-expr-to-lean4 (third expr))))

       ;; Modulo
       ((mod rem)
        (format nil "(~A % ~A)"
                (lisp-expr-to-lean4 (second expr))
                (lisp-expr-to-lean4 (third expr))))

       ;; Comparisons
       ((< > <= >= =)
        (let ((op (case (first expr)
                    (<= "<=") (>= ">=") (t (first expr)))))
          (format nil "(~A ~A ~A)"
                  (lisp-expr-to-lean4 (second expr))
                  op
                  (lisp-expr-to-lean4 (third expr)))))

       ;; Not equal
       (/=
        (format nil "(~A ~A ~A)"
                (lisp-expr-to-lean4 (second expr))
                "/="
                (lisp-expr-to-lean4 (third expr))))

       ;; Logical AND
       (and
        (format nil "(~A /\\ ~A)"
                (lisp-expr-to-lean4 (second expr))
                (lisp-expr-to-lean4 (third expr))))

       ;; Logical OR
       (or
        (format nil "(~A \\/ ~A)"
                (lisp-expr-to-lean4 (second expr))
                (lisp-expr-to-lean4 (third expr))))

       ;; Logical NOT
       (not
        (format nil "(not ~A)" (lisp-expr-to-lean4 (second expr))))

       ;; Universal quantifier
       (forall
        (format nil "forall ~A, ~A"
                (lisp-expr-to-lean4 (second expr))
                (lisp-expr-to-lean4 (third expr))))

       ;; Existential quantifier
       (exists
        (format nil "exists ~A, ~A"
                (lisp-expr-to-lean4 (second expr))
                (lisp-expr-to-lean4 (third expr))))

       ;; Implication
       ((implies ->)
        (format nil "(~A -> ~A)"
                (lisp-expr-to-lean4 (second expr))
                (lisp-expr-to-lean4 (third expr))))

       ;; If-then-else
       (if
        (format nil "(if ~A then ~A else ~A)"
                (lisp-expr-to-lean4 (second expr))
                (lisp-expr-to-lean4 (third expr))
                (lisp-expr-to-lean4 (or (fourth expr) nil))))

       ;; Let binding
       (let
        (let ((bindings (second expr))
              (body (third expr)))
          (format nil "let ~{~A~^; ~} in ~A"
                  (mapcar (lambda (b)
                            (format nil "~A := ~A"
                                    (lisp-name-to-lean4 (car b))
                                    (lisp-expr-to-lean4 (cadr b))))
                          bindings)
                  (lisp-expr-to-lean4 body))))

       ;; Function application (default)
       (t
        (format nil "(~A~{ ~A~})"
                (lisp-name-to-lean4 (first expr))
                (mapcar #'lisp-expr-to-lean4 (rest expr))))))))

;;; ============================================================================
;;; Lean4 Code Generation
;;; ============================================================================

(defun generate-lean4-header (&key (title "CLPIC Module")
                                (imports '("Mathlib.Tactic"))
                                (module-name "CLPIC"))
  "Generate Lean4 module header.

   TITLE: Header comment title
   IMPORTS: List of imports
   MODULE-NAME: Namespace name

   Returns header as string."
  (with-output-to-string (s)
    (format s "/-~%  ~A~%  Auto-generated by cl-lean4-integration~%-/~%~%" title)
    (dolist (imp imports)
      (format s "import ~A~%" imp))
    (format s "~%namespace ~A~%~%" module-name)))

(defun generate-lean4-theorem (spec)
  "Generate a Lean4 theorem from a specification.

   SPEC: A specification struct

   Returns theorem code as string."
  (let ((fn-name (specification-function spec))
        (preconds (specification-preconditions spec))
        (postconds (specification-postconditions spec)))
    (with-output-to-string (s)
      ;; Theorem statement
      (format s "theorem ~A_correct" (lisp-name-to-lean4 fn-name))

      ;; Preconditions as hypotheses
      (when preconds
        (format s "~%  (")
        (loop for (name expr) in preconds
              for first = t then nil
              do (unless first (format s "~%   "))
                 (format s "h_~A : ~A"
                         (lisp-name-to-lean4 name)
                         (lisp-expr-to-lean4 expr)))
        (format s ")"))

      ;; Statement
      (format s " :~%")

      ;; Postconditions as conclusion
      (cond
        ((null postconds)
         (format s "  True := by~%"))
        ((= 1 (length postconds))
         (format s "  ~A := by~%"
                 (lisp-expr-to-lean4 (second (first postconds)))))
        (t
         (format s "  (")
         (loop for (name expr) in postconds
               for first = t then nil
               do (unless first (format s " /\\~%   "))
                  (format s "~A" (lisp-expr-to-lean4 expr)))
         (format s ") := by~%")))

      ;; Proof tactics
      (format s "  ~A~%"
              (generate-proof-tactics postconds
                                      (specification-invariants spec))))))

(defun generate-proof-tactics (postconds invariants)
  "Generate Lean4 tactics for proving postconditions.

   POSTCONDS: List of postcondition expressions
   INVARIANTS: List of invariant expressions

   Returns tactic string."
  (declare (ignore invariants))
  (cond
    ;; No postconditions - trivial proof
    ((null postconds)
     "trivial")

    ;; Single postcondition - analyze structure
    ((= 1 (length postconds))
     (synthesize-tactic (second (first postconds))))

    ;; Multiple postconditions - use constructor and prove each
    (t
     (format nil "constructor~%~{  . ~A~%~}"
             (mapcar (lambda (post) (synthesize-tactic (second post)))
                     postconds)))))

(defun synthesize-tactic (expr)
  "Synthesize a Lean4 tactic for proving an expression.

   EXPR: Lisp expression to prove

   Returns tactic string."
  (cond
    ;; Nil/T - trivial
    ((null expr) "trivial")
    ((eq expr t) "trivial")

    ;; Reflexive equality (= x x)
    ((and (consp expr)
          (eq (car expr) '=)
          (equal (second expr) (third expr)))
     "rfl")

    ;; Numeric comparison
    ((and (consp expr)
          (member (car expr) '(< > <= >= = /=))
          (every #'numberp (cdr expr)))
     "omega")

    ;; Conjunction
    ((and (consp expr) (eq (car expr) 'and))
     (format nil "constructor~%    . ~A~%    . ~A"
             (synthesize-tactic (second expr))
             (synthesize-tactic (third expr))))

    ;; Implication
    ((and (consp expr) (member (car expr) '(implies ->)))
     (format nil "intro h~%    ~A"
             (synthesize-tactic (third expr))))

    ;; Universal quantification
    ((and (consp expr) (eq (car expr) 'forall))
     (format nil "intro x~%    ~A"
             (synthesize-tactic (third expr))))

    ;; Existential quantification
    ((and (consp expr) (eq (car expr) 'exists))
     "use ?witness -- TODO: provide witness~%    sorry")

    ;; Disjunction
    ((and (consp expr) (eq (car expr) 'or))
     (format nil "left~%    ~A" (synthesize-tactic (second expr))))

    ;; Default - use simp then aesop
    (t
     "simp only [*]~%    <;> aesop")))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defparameter lambda-list-keywords
  '(&optional &rest &key &allow-other-keys &aux &body &environment &whole)
  "Lambda list keywords to filter out.")

(defun function-lambda-list (fn-name)
  "Get the lambda list of a function."
  (handler-case
      #+sbcl (sb-introspect:function-lambda-list (fdefinition fn-name))
      #-sbcl nil
    (error () nil)))
