;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-lean4-integration.asd - Common Lisp Lean4 Proof Assistant Bridge
;;;;
;;;; A standalone library for integrating Common Lisp with the Lean4 theorem
;;;; prover. Enables exporting CL specifications to Lean4 and importing proofs.

(asdf:defsystem #:cl-lean4-integration
  :name "cl-lean4-integration"
  :description "Common Lisp to Lean4 theorem prover integration"
  :version "0.1.0"
  :author "Parkian Company LLC"
  :license "MIT"
  :depends-on ()  ; Pure Common Lisp - no external dependencies
  :serial t
  :components
  ((:file "package")
   (:module "src"
    :serial t
    :components
    ((:file "util")
     (:file "types")
     (:file "export")
     (:file "import")
     (:file "bridge"))))
  :in-order-to ((asdf:test-op (test-op #:cl-lean4-integration/test))))

(asdf:defsystem #:cl-lean4-integration/test
  :name "cl-lean4-integration/test"
  :description "Tests for cl-lean4-integration"
  :depends-on (#:cl-lean4-integration)
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "test-lean4"))))
  :perform (asdf:test-op (o c)
             (let ((result (uiop:symbol-call :cl-lean4-integration.test :run-all-tests)))
               (unless result
                 (error "Tests failed")))))
