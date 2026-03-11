;;;; cl-lean4-integration - Package Definition
;;;;
;;;; Common Lisp to Lean4 theorem prover integration.
;;;; Enables exporting CL specifications to Lean4 and importing proofs.

(in-package #:cl-user)

(defpackage #:cl-lean4-integration
  (:use #:cl)
  (:nicknames #:lean4 #:l4)
  (:documentation "Common Lisp to Lean4 theorem prover integration.

Provides a bridge between Common Lisp and the Lean4 theorem prover for:
- Exporting CL specifications to Lean4 syntax
- Importing and validating Lean4 proofs
- Managing Lean4 server communication via LSP

Quick Start:
  ;; Start the Lean4 bridge
  (lean4:initialize-bridge)

  ;; Export a specification to Lean4
  (lean4:export-specification my-spec)

  ;; Verify a theorem
  (lean4:verify-theorem theorem-name)

  ;; Import proof results
  (lean4:import-proof proof-file)")

  ;; ============================================================================
  ;; Utility Functions
  ;; ============================================================================
  (:export
   #:lisp-name-to-lean4
   #:lean4-name-to-lisp
   #:camel-case
   #:kebab-to-camel
   #:mangle-name
   #:unmangle-name)

  ;; ============================================================================
  ;; Core Types
  ;; ============================================================================
  (:export
   ;; Specification
   #:specification
   #:make-specification
   #:specification-p
   #:specification-name
   #:specification-function
   #:specification-preconditions
   #:specification-postconditions
   #:specification-invariants
   #:specification-confidence

   ;; Lean4 Types
   #:lean4-type
   #:make-lean4-type
   #:lean4-type-p
   #:lean4-type-name
   #:lean4-type-params
   #:lean4-type-universe

   ;; Lean4 Theorem
   #:lean4-theorem
   #:make-lean4-theorem
   #:lean4-theorem-p
   #:lean4-theorem-name
   #:lean4-theorem-statement
   #:lean4-theorem-proof
   #:lean4-theorem-status

   ;; Proof Result
   #:lean4-result
   #:make-lean4-result
   #:lean4-result-p
   #:lean4-result-success-p
   #:lean4-result-output
   #:lean4-result-errors
   #:lean4-result-time-ms

   ;; Bridge
   #:lean4-bridge
   #:lean4-bridge-p
   #:lean4-bridge-available-p
   #:lean4-bridge-version)

  ;; ============================================================================
  ;; Export (CL -> Lean4)
  ;; ============================================================================
  (:export
   #:export-specification
   #:export-function
   #:export-theorem
   #:translate-to-lean4
   #:translate-type
   #:translate-expr
   #:lisp-expr-to-lean4
   #:generate-lean4-header
   #:generate-lean4-theorem
   #:generate-proof-tactics
   #:synthesize-tactic)

  ;; ============================================================================
  ;; Import (Lean4 -> CL)
  ;; ============================================================================
  (:export
   #:import-proof
   #:import-theorem
   #:import-verification-result
   #:parse-lean4-output
   #:parse-lean4-error
   #:parse-lean4-diagnostics
   #:lean4-to-lisp-type
   #:lean4-to-lisp-expr)

  ;; ============================================================================
  ;; Bridge (Communication with Lean4)
  ;; ============================================================================
  (:export
   ;; Bridge lifecycle
   #:*lean4-bridge*
   #:initialize-bridge
   #:shutdown-bridge
   #:bridge-available-p
   #:get-lean4-version

   ;; Server management
   #:start-lean4-server
   #:stop-lean4-server
   #:server-running-p
   #:restart-server

   ;; Verification
   #:verify-theorem
   #:verify-file
   #:verify-content
   #:check-proof

   ;; LSP communication
   #:send-request
   #:receive-response
   #:lsp-initialize
   #:lsp-shutdown

   ;; Configuration
   #:*lean4-executable*
   #:*lean4-project-path*
   #:*lean4-timeout*
   #:*lean4-log-level*

   ;; Conditions
   #:lean4-error
   #:lean4-error-message
   #:translation-error
   #:verification-error
   #:server-error))

(defpackage #:cl-lean4-integration.test
  (:use #:cl #:cl-lean4-integration)
  (:export #:run-all-tests))
