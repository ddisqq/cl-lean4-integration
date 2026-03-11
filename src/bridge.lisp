;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-lean4-integration - Bridge (Communication with Lean4)
;;;;
;;;; Functions for managing Lean4 server and executing verification.

(in-package #:cl-lean4-integration)

;;; ============================================================================
;;; Bridge Initialization
;;; ============================================================================

(defun initialize-bridge (&key (executable *lean4-executable*)
                             (project-path *lean4-project-path*)
                             (check-version t))
  "Initialize the Lean4 bridge.

   EXECUTABLE: Path to Lean4 executable
   PROJECT-PATH: Path to Lean4 project (for lake builds)
   CHECK-VERSION: If T, verify Lean4 installation

   Returns T if successful, NIL otherwise."
  (let ((available (when check-version (lean4-installed-p executable))))
    (setf *lean4-bridge*
          (%make-lean4-bridge
           :path executable
           :project-path (when project-path (namestring project-path))
           :available-p available
           :version (when available (get-lean4-version executable))))

    (when available
      (log-message :info "Lean4 bridge initialized: ~A"
                   (lean4-bridge-version *lean4-bridge*)))
    available))

(defun shutdown-bridge ()
  "Shutdown the Lean4 bridge and cleanup resources."
  (when *lean4-bridge*
    ;; Stop server if running
    (when (server-running-p)
      (stop-lean4-server))
    ;; Clear cache
    (clrhash (lean4-bridge-cache *lean4-bridge*))
    (setf *lean4-bridge* nil)
    (log-message :info "Lean4 bridge shutdown"))
  t)

(defun bridge-available-p ()
  "Check if the Lean4 bridge is available and ready."
  (and *lean4-bridge*
       (lean4-bridge-available-p *lean4-bridge*)))

;;; ============================================================================
;;; Lean4 Installation Detection
;;; ============================================================================

(defun lean4-installed-p (&optional (executable *lean4-executable*))
  "Check if Lean4 is installed and accessible."
  (handler-case
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program (list executable "--version")
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (declare (ignore output error-output))
        (zerop exit-code))
    (error () nil)))

(defun get-lean4-version (&optional (executable *lean4-executable*))
  "Get Lean4 version string."
  (handler-case
      (string-trim '(#\Space #\Newline #\Return)
                   (uiop:run-program (list executable "--version")
                                     :output :string))
    (error () "unknown")))

;;; ============================================================================
;;; Server Management
;;; ============================================================================

(defun start-lean4-server (&key (port 0) (host "127.0.0.1"))
  "Start a Lean4 LSP server.

   PORT: Port to listen on (0 for auto-assign)
   HOST: Host to bind to

   Returns T if server started successfully."
  (declare (ignore port host))
  (unless (bridge-available-p)
    (error 'server-error
           :message "Lean4 bridge not initialized. Call INITIALIZE-BRIDGE first."))

  (when (server-running-p)
    (log-message :warn "Lean4 server already running")
    (return-from start-lean4-server t))

  ;; For now, we don't actually start a persistent server
  ;; Each verification runs Lean4 directly
  (log-message :info "Lean4 server ready (direct execution mode)")
  t)

(defun stop-lean4-server ()
  "Stop the Lean4 LSP server."
  (when (and *lean4-bridge* (lean4-bridge-process *lean4-bridge*))
    (handler-case
        (progn
          #+sbcl (sb-ext:process-kill (lean4-bridge-process *lean4-bridge*) 15)
          (setf (lean4-bridge-process *lean4-bridge*) nil))
      (error (e)
        (log-message :warn "Error stopping server: ~A" e))))
  t)

(defun server-running-p ()
  "Check if Lean4 server is running."
  (and *lean4-bridge*
       (lean4-bridge-process *lean4-bridge*)
       #+sbcl (sb-ext:process-alive-p (lean4-bridge-process *lean4-bridge*))
       #-sbcl nil))

(defun restart-server ()
  "Restart the Lean4 server."
  (stop-lean4-server)
  (start-lean4-server))

;;; ============================================================================
;;; Verification Functions
;;; ============================================================================

(defun verify-theorem (theorem &key (timeout *lean4-timeout*))
  "Verify a theorem using Lean4.

   THEOREM: A lean4-theorem struct or string of Lean4 code
   TIMEOUT: Timeout in seconds

   Returns a lean4-result struct."
  (let ((code (etypecase theorem
                (lean4-theorem (export-theorem theorem))
                (string theorem))))
    (verify-content code :timeout timeout)))

(defun verify-file (file-path &key (timeout *lean4-timeout*))
  "Verify a Lean4 file.

   FILE-PATH: Path to .lean file
   TIMEOUT: Timeout in seconds

   Returns a lean4-result struct."
  (unless (probe-file file-path)
    (return-from verify-file
      (make-lean4-result
       :success-p nil
       :errors (list (format nil "File not found: ~A" file-path)))))

  (run-lean4-on-file file-path :timeout timeout))

(defun verify-content (lean4-code &key (timeout *lean4-timeout*))
  "Verify Lean4 code content.

   LEAN4-CODE: String containing Lean4 code
   TIMEOUT: Timeout in seconds

   Returns a lean4-result struct."
  (unless (bridge-available-p)
    (return-from verify-content
      (make-lean4-result
       :success-p nil
       :errors '("Lean4 not available"))))

  ;; Check cache first
  (let ((cached (gethash lean4-code (lean4-bridge-cache *lean4-bridge*))))
    (when cached
      (return-from verify-content cached)))

  ;; Write to temp file and verify
  (let ((temp-file (uiop:with-temporary-file (:pathname p :type "lean" :keep t)
                     (with-open-file (f p :direction :output :if-exists :supersede)
                       (write-string lean4-code f))
                     p)))
    (unwind-protect
        (let ((result (run-lean4-on-file temp-file :timeout timeout)))
          ;; Cache result
          (setf (gethash lean4-code (lean4-bridge-cache *lean4-bridge*)) result)
          result)
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(defun check-proof (proof-code &key (timeout *lean4-timeout*))
  "Check if a proof is valid.

   PROOF-CODE: String containing Lean4 proof
   TIMEOUT: Timeout in seconds

   Returns T if proof is valid, NIL otherwise."
  (let ((result (verify-content proof-code :timeout timeout)))
    (lean4-result-success-p result)))

;;; ============================================================================
;;; Lean4 Execution
;;; ============================================================================

(defun run-lean4-on-file (file-path &key (timeout *lean4-timeout*))
  "Run Lean4 on a file and return result.

   FILE-PATH: Path to .lean file
   TIMEOUT: Timeout in seconds

   Returns a lean4-result struct."
  (let ((start-time (get-internal-real-time))
        (executable (lean4-bridge-path *lean4-bridge*)))
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list executable (namestring file-path))
                              :output :string
                              :error-output :string
                              :ignore-error-status t
                              :timeout timeout)
          (let* ((elapsed-ms (get-elapsed-ms start-time))
                 (diagnostics (parse-lean4-output
                               (concatenate 'string output error-output)))
                 (errors (remove-if-not
                          (lambda (d) (eq (getf d :severity) :error))
                          diagnostics))
                 (warnings (remove-if-not
                            (lambda (d) (eq (getf d :severity) :warning))
                            diagnostics)))
            (make-lean4-result
             :success-p (and (zerop exit-code) (null errors))
             :output output
             :errors (mapcar (lambda (d) (getf d :message)) errors)
             :warnings (mapcar (lambda (d) (getf d :message)) warnings)
             :time-ms elapsed-ms
             :diagnostics diagnostics)))
      (uiop:subprocess-error (e)
        (make-lean4-result
         :success-p nil
         :errors (list (format nil "Process error: ~A" e))
         :time-ms (get-elapsed-ms start-time)))
      (error (e)
        (make-lean4-result
         :success-p nil
         :errors (list (format nil "Error: ~A" e))
         :time-ms (get-elapsed-ms start-time))))))

;;; ============================================================================
;;; LSP Protocol (Stubs for future implementation)
;;; ============================================================================

(defun send-request (method params &key (timeout *lean4-timeout*))
  "Send an LSP request to the Lean4 server.

   METHOD: LSP method name (string)
   PARAMS: Parameters (alist or plist)
   TIMEOUT: Timeout in seconds

   Returns response or signals error."
  (declare (ignore method params timeout))
  (unless (server-running-p)
    (error 'server-error :message "Server not running"))
  ;; TODO: Implement LSP protocol
  (error 'server-error :message "LSP communication not yet implemented"))

(defun receive-response (&key (timeout *lean4-timeout*))
  "Receive a response from the Lean4 server.

   TIMEOUT: Timeout in seconds

   Returns response plist."
  (declare (ignore timeout))
  ;; TODO: Implement LSP protocol
  nil)

(defun lsp-initialize ()
  "Send LSP initialize request."
  (send-request "initialize"
                '(:processId nil
                  :capabilities (:textDocumentSync 1))))

(defun lsp-shutdown ()
  "Send LSP shutdown request."
  (send-request "shutdown" nil))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun quick-verify (lisp-expr &key (expected-result t))
  "Quick verification of a simple Lisp expression.

   LISP-EXPR: Lisp expression to verify
   EXPECTED-RESULT: Expected result (default T)

   Returns T if verification succeeds."
  (let* ((lean4-expr (lisp-expr-to-lean4 lisp-expr))
         (lean4-code (format nil
                             "import Mathlib.Tactic

theorem quick_check : ~A = ~A := by
  ~A"
                             lean4-expr
                             (if expected-result "True" "False")
                             (synthesize-tactic lisp-expr)))
         (result (verify-content lean4-code)))
    (lean4-result-success-p result)))

(defun verify-specification-quick (spec)
  "Quick verification of a specification.

   SPEC: A specification struct

   Returns (VALUES success-p result)."
  (let* ((lean4-code (export-specification spec))
         (result (verify-content lean4-code)))
    (values (lean4-result-success-p result) result)))
