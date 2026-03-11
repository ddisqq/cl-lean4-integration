;;;; cl-lean4-integration - Core Type Definitions
;;;;
;;;; Data structures for specifications, Lean4 types, theorems, and results.

(in-package #:cl-lean4-integration)

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defvar *lean4-executable* "lean4"
  "Path to Lean4 executable.")

(defvar *lean4-project-path* nil
  "Path to Lean4 project directory (for lake builds).")

(defvar *lean4-timeout* 30
  "Default timeout in seconds for Lean4 operations.")

;;; ============================================================================
;;; Conditions
;;; ============================================================================

(define-condition lean4-error (error)
  ((message :initarg :message :reader lean4-error-message))
  (:report (lambda (c s)
             (format s "Lean4 error: ~A" (lean4-error-message c))))
  (:documentation "Base condition for Lean4 integration errors."))

(define-condition translation-error (lean4-error)
  ((form :initarg :form :reader translation-error-form)
   (context :initarg :context :reader translation-error-context :initform nil))
  (:report (lambda (c s)
             (format s "Translation error for ~S: ~A"
                     (translation-error-form c)
                     (lean4-error-message c))))
  (:documentation "Error during translation from Lisp to Lean4."))

(define-condition verification-error (lean4-error)
  ((theorem :initarg :theorem :reader verification-error-theorem)
   (diagnostics :initarg :diagnostics :reader verification-error-diagnostics :initform nil))
  (:report (lambda (c s)
             (format s "Verification failed for ~A: ~A"
                     (verification-error-theorem c)
                     (lean4-error-message c))))
  (:documentation "Error during Lean4 proof verification."))

(define-condition server-error (lean4-error)
  ((code :initarg :code :reader server-error-code :initform nil))
  (:report (lambda (c s)
             (format s "Lean4 server error~@[ (~A)~]: ~A"
                     (server-error-code c)
                     (lean4-error-message c))))
  (:documentation "Error in Lean4 server communication."))

;;; ============================================================================
;;; Specification - Formal Property Description
;;; ============================================================================

(defstruct (specification (:constructor %make-specification))
  "A formal specification of function behavior.

   Slots:
   - NAME: Symbol identifying this specification
   - FUNCTION: Symbol of the function being specified
   - PRECONDITIONS: List of (name expr) pairs
   - POSTCONDITIONS: List of (name expr) pairs
   - INVARIANTS: List of (name expr) pairs
   - CONFIDENCE: Float 0.0-1.0 indicating spec confidence
   - SOURCE: Keyword indicating origin (:manual :inferred :mined)
   - METADATA: Additional properties as alist"
  (name nil :type (or symbol null))
  (function nil :type (or symbol null))
  (preconditions nil :type list)
  (postconditions nil :type list)
  (invariants nil :type list)
  (confidence 1.0 :type single-float)
  (source :manual :type keyword)
  (metadata nil :type list))

(defun make-specification (&key name function preconditions postconditions
                             invariants (confidence 1.0) (source :manual) metadata)
  "Create a new specification."
  (%make-specification
   :name (or name (gensym "SPEC-"))
   :function function
   :preconditions preconditions
   :postconditions postconditions
   :invariants invariants
   :confidence (coerce confidence 'single-float)
   :source source
   :metadata metadata))

;;; ============================================================================
;;; Lean4 Type Representation
;;; ============================================================================

(defstruct (lean4-type (:constructor %make-lean4-type)
                       (:conc-name l4type-)
                       (:predicate nil))  ; We define our own lean4-type-p
  "Representation of a Lean4 type.

   Slots:
   - NAME: Type name (symbol or string)
   - KIND: :type, :prop, :sort, or :universe
   - PARAMS: List of type parameters
   - UNIVERSE: Universe level (0+)
   - CONSTRUCTORS: For inductive types, list of constructors
   - IMPLICIT-P: Whether parameters are implicit
   - DEPENDENT-P: Whether this is a dependent type"
  (name nil :type (or symbol string null))
  (kind :type :type (member :type :prop :sort :universe))
  (params nil :type list)
  (universe 0 :type (or fixnum symbol))
  (constructors nil :type list)
  (implicit-p nil :type boolean)
  (dependent-p nil :type boolean))

(defun make-lean4-type (&key name (kind :type) params (universe 0)
                          constructors implicit-p dependent-p)
  "Create a new Lean4 type."
  (%make-lean4-type
   :name name
   :kind kind
   :params params
   :universe universe
   :constructors constructors
   :implicit-p implicit-p
   :dependent-p dependent-p))

(defun lean4-type-name (type)
  "Get the name of a Lean4 type."
  (l4type-name type))

(defun lean4-type-params (type)
  "Get the parameters of a Lean4 type."
  (l4type-params type))

(defun lean4-type-universe (type)
  "Get the universe level of a Lean4 type."
  (l4type-universe type))

(defun lean4-type-p (obj)
  "Check if OBJ is a lean4-type."
  (typep obj 'lean4-type))

;;; ============================================================================
;;; Lean4 Theorem
;;; ============================================================================

(deftype theorem-status ()
  "Status of a theorem proof."
  '(member :pending :verified :failed :timeout :unknown))

(defstruct (lean4-theorem (:constructor %make-lean4-theorem))
  "A Lean4 theorem with its proof status.

   Slots:
   - NAME: Theorem name (symbol)
   - STATEMENT: Theorem statement as Lean4 code (string)
   - HYPOTHESES: List of hypothesis names/types
   - PROOF: Proof term or tactic script (string or list)
   - DEPENDENCIES: List of required lemmas/theorems
   - STATUS: :pending :verified :failed :timeout :unknown"
  (name nil :type (or symbol string null))
  (statement nil :type (or string null))
  (hypotheses nil :type list)
  (proof nil :type (or string list null))
  (dependencies nil :type list)
  (status :pending :type theorem-status))

(defun make-lean4-theorem (&key name statement hypotheses proof dependencies
                             (status :pending))
  "Create a new Lean4 theorem."
  (%make-lean4-theorem
   :name name
   :statement statement
   :hypotheses hypotheses
   :proof proof
   :dependencies dependencies
   :status status))

;;; ============================================================================
;;; Lean4 Verification Result
;;; ============================================================================

(defstruct (lean4-result (:constructor %make-lean4-result))
  "Result from Lean4 verification.

   Slots:
   - SUCCESS-P: T if verification succeeded
   - OUTPUT: Raw output from Lean4
   - ERRORS: List of error messages
   - WARNINGS: List of warning messages
   - TIME-MS: Verification time in milliseconds
   - DIAGNOSTICS: List of structured diagnostics"
  (success-p nil :type boolean)
  (output "" :type string)
  (errors nil :type list)
  (warnings nil :type list)
  (time-ms 0 :type fixnum)
  (diagnostics nil :type list))

(defun make-lean4-result (&key success-p output errors warnings time-ms diagnostics)
  "Create a Lean4 result."
  (%make-lean4-result
   :success-p success-p
   :output (or output "")
   :errors errors
   :warnings warnings
   :time-ms (or time-ms 0)
   :diagnostics diagnostics))

;;; ============================================================================
;;; Lean4 Bridge
;;; ============================================================================

(defstruct (lean4-bridge (:constructor %make-lean4-bridge)
                         (:predicate nil))  ; We define our own lean4-bridge-p
  "Bridge to Lean4 theorem prover.

   Slots:
   - PATH: Path to Lean4 executable
   - PROJECT-PATH: Path to Lean4 project directory
   - AVAILABLE-P: Whether Lean4 is available
   - VERSION: Lean4 version string
   - PROCESS: Server process (if running)
   - CACHE: Hash table for caching results"
  (path "lean4" :type string)
  (project-path nil :type (or string null))
  (available-p nil :type boolean)
  (version nil :type (or string null))
  (process nil)
  (cache (make-hash-table :test 'equal) :type hash-table))

(defvar *lean4-bridge* nil
  "Global Lean4 bridge instance.")

(defun lean4-bridge-p (obj)
  "Check if OBJ is a lean4-bridge."
  (typep obj 'lean4-bridge))

;;; ============================================================================
;;; Built-in Type Mappings
;;; ============================================================================

(defparameter *type-mappings*
  '((integer . "Int")
    (fixnum . "Int")
    (bignum . "Int")
    (float . "Float")
    (single-float . "Float")
    (double-float . "Float")
    (rational . "Rat")
    (string . "String")
    (character . "Char")
    (boolean . "Bool")
    (t . "Bool")
    (nil . "Bool")
    (list . "List")
    (vector . "Array")
    (hash-table . "HashMap")
    (symbol . "String")
    (keyword . "String")
    ;; Blockchain-specific types (when using this in a blockchain context)
    (byte-vector . "ByteArray")
    (hash256 . "ByteArray")
    (address . "ByteArray")
    (pubkey . "ByteArray")
    (signature . "ByteArray"))
  "Mapping from Common Lisp types to Lean4 types.")

(defun get-type-mapping (lisp-type)
  "Get the Lean4 type for a Common Lisp type."
  (or (cdr (assoc lisp-type *type-mappings*))
      "Any"))  ; Default to Any for unknown types

(defun register-type-mapping (lisp-type lean4-type)
  "Register a new type mapping."
  (let ((existing (assoc lisp-type *type-mappings*)))
    (if existing
        (setf (cdr existing) lean4-type)
        (push (cons lisp-type lean4-type) *type-mappings*)))
  lean4-type)
