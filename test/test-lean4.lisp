;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; test/test-lean4.lisp - Tests for cl-lean4-integration

(in-package #:cl-lean4-integration.test)

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro deftest (name &body body)
  `(defun ,name ()
     (incf *test-count*)
     (handler-case
         (progn ,@body
                (incf *pass-count*)
                (format t "  PASS: ~A~%" ',name))
       (error (e)
         (incf *fail-count*)
         (format t "  FAIL: ~A - ~A~%" ',name e)))))

(defmacro assert-true (form &optional message)
  `(unless ,form
     (error "Assertion failed~@[: ~A~]" ,message)))

(defmacro assert-equal (expected actual &optional message)
  `(unless (equal ,expected ,actual)
     (error "Expected ~S but got ~S~@[: ~A~]" ,expected ,actual ,message)))

;;; ============================================================================
;;; Name Translation Tests
;;; ============================================================================

(deftest test-lisp-name-to-lean4
  "Lisp names convert to Lean4 format"
  (assert-equal "fooBar" (lisp-name-to-lean4 'foo-bar))
  (assert-equal "myFunc" (lisp-name-to-lean4 'my-func)))

(deftest test-translate-type
  "Lisp types translate to Lean4"
  (assert-true (stringp (translate-type 'integer)))
  (assert-true (stringp (translate-type 'string))))

;;; ============================================================================
;;; Expression Translation Tests
;;; ============================================================================

(deftest test-translate-number
  "Numbers translate directly"
  (assert-equal "42" (translate-to-lean4 42)))

(deftest test-translate-string
  "Strings get quoted"
  (assert-equal "\"hello\"" (translate-to-lean4 "hello")))

(deftest test-translate-nil
  "NIL becomes True"
  (assert-equal "True" (translate-to-lean4 nil)))

;;; ============================================================================
;;; Specification Tests
;;; ============================================================================

(deftest test-make-specification
  "Specifications can be created"
  (let ((spec (make-specification :name "test"
                                   :function 'identity)))
    (assert-true (specification-p spec))
    (assert-equal "test" (specification-name spec))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-all-tests ()
  "Run all tests and return T on success."
  (setf *test-count* 0
        *pass-count* 0
        *fail-count* 0)
  (format t "~&Running cl-lean4-integration tests...~%~%")

  (format t "Name Translation Tests:~%")
  (test-lisp-name-to-lean4)
  (test-translate-type)

  (format t "~%Expression Translation Tests:~%")
  (test-translate-number)
  (test-translate-string)
  (test-translate-nil)

  (format t "~%Specification Tests:~%")
  (test-make-specification)

  (format t "~%========================================~%")
  (format t "Results: ~D/~D passed (~D failed)~%"
          *pass-count* *test-count* *fail-count*)
  (format t "========================================~%")
  (zerop *fail-count*))
