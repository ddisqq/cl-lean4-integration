;;;; cl-lean4-integration.asd - Common Lisp Lean4 Proof Assistant Bridge
;;;;
;;;; A standalone library for integrating Common Lisp with the Lean4 theorem
;;;; prover. Enables exporting CL specifications to Lean4 and importing proofs.

(defsystem #:cl-lean4-integration
  :name "cl-lean4-integration"
  :description "Common Lisp to Lean4 theorem prover integration"
  :version "1.0.0"
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
  :in-order-to ((test-op (test-op #:cl-lean4-integration/test))))

(defsystem #:cl-lean4-integration/test
  :name "cl-lean4-integration/test"
  :description "Tests for cl-lean4-integration"
  :depends-on (#:cl-lean4-integration)
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "test-lean4"))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-lean4-integration.test :run-all-tests)))
