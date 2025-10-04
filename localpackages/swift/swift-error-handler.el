;;; swift-error-handler.el --- Enhanced error handling for Swift development tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides comprehensive error handling and recovery for Swift/Xcode development

;;; Code:

(require 'cl-lib)

(defgroup swift-error-handler nil
  "Error handling for Swift development tools."
  :group 'programming
  :prefix "swift-error-handler-")

(defcustom swift-error-handler-max-retries 3
  "Maximum number of retries for failed operations."
  :type 'integer
  :group 'swift-error-handler)

(defcustom swift-error-handler-retry-delay 2
  "Delay in seconds between retry attempts."
  :type 'number
  :group 'swift-error-handler)

(defcustom swift-error-handler-log-errors t
  "Whether to log errors to a buffer."
  :type 'boolean
  :group 'swift-error-handler)

(defvar swift-error-handler--log-buffer "*Swift Error Log*"
  "Buffer name for error logging.")

(defvar swift-error-handler--retry-table (make-hash-table :test 'equal)
  "Table tracking retry counts for operations.")

;; Error types
(cl-defstruct swift-error
  "Swift error structure."
  type           ; Error type symbol (e.g., 'build, 'simulator, 'device)
  message        ; Error message
  context        ; Additional context
  timestamp      ; When the error occurred
  recoverable-p  ; Whether the error is recoverable
  recovery-fn)   ; Function to call for recovery

(defun swift-error-handler-log (error-obj)
  "Log ERROR-OBJ to the error buffer."
  (when swift-error-handler-log-errors
    (with-current-buffer (get-buffer-create swift-error-handler--log-buffer)
      (goto-char (point-max))
      (insert (format "[%s] %s: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S" 
                                        (swift-error-timestamp error-obj))
                      (swift-error-type error-obj)
                      (swift-error-message error-obj)))
      (when (swift-error-context error-obj)
        (insert (format "  Context: %s\n" (swift-error-context error-obj))))
      (when (swift-error-recoverable-p error-obj)
        (insert "  [Recoverable]\n")))))

(defmacro swift-error-handler-with-retry (operation-id max-retries &rest body)
  "Execute BODY with automatic retry on failure.
OPERATION-ID uniquely identifies the operation.
MAX-RETRIES specifies maximum retry attempts."
  (declare (indent 2))
  `(cl-block retry-block
     (let ((retry-count (or (gethash ,operation-id swift-error-handler--retry-table) 0))
           (max-tries (or ,max-retries swift-error-handler-max-retries)))
       (condition-case err
           (progn
             (remhash ,operation-id swift-error-handler--retry-table)
             ,@body)
         (error
          (if (< retry-count max-tries)
              (progn
                (puthash ,operation-id (1+ retry-count) swift-error-handler--retry-table)
                (message "Operation %s failed (attempt %d/%d): %s. Retrying..."
                         ,operation-id (1+ retry-count) max-tries
                         (error-message-string err))
                (sit-for swift-error-handler-retry-delay)
                (swift-error-handler-with-retry ,operation-id ,max-retries ,@body))
            (progn
              (remhash ,operation-id swift-error-handler--retry-table)
              (signal (car err) (cdr err)))))))))

(defun swift-error-handler-handle (error-obj)
  "Handle ERROR-OBJ with appropriate recovery strategy."
  (swift-error-handler-log error-obj)
  
  (cond
   ;; Attempt recovery if possible
   ((and (swift-error-recoverable-p error-obj)
         (swift-error-recovery-fn error-obj))
    (message "Attempting to recover from: %s" (swift-error-message error-obj))
    (funcall (swift-error-recovery-fn error-obj)))
   
   ;; Non-recoverable error
   (t
    (message "Error: %s" (swift-error-message error-obj))
    (when (yes-or-no-p "Would you like to view the error log?")
      (swift-error-handler-show-log)))))

(defun swift-error-handler-show-log ()
  "Display the error log buffer."
  (interactive)
  (let ((buffer (get-buffer-create swift-error-handler--log-buffer)))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer))))

(defun swift-error-handler-clear-log ()
  "Clear the error log."
  (interactive)
  (when (get-buffer swift-error-handler--log-buffer)
    (with-current-buffer swift-error-handler--log-buffer
      (erase-buffer))
    (message "Error log cleared")))

;; Specific error handlers for common scenarios

(defun swift-error-handler-build-failed (output)
  "Handle build failure with OUTPUT."
  (let ((error-obj (make-swift-error
                   :type 'build
                   :message "Build failed"
                   :context output
                   :timestamp (current-time)
                   :recoverable-p t
                   :recovery-fn (lambda ()
                                 (when (yes-or-no-p "Clean build folder and retry?")
                                   (xcode-additions:clean-build-folder)
                                   (swift-additions:compile-app))))))
    (swift-error-handler-handle error-obj)))

(defun swift-error-handler-simulator-boot-failed (simulator-id)
  "Handle simulator boot failure for SIMULATOR-ID."
  (let ((error-obj (make-swift-error
                   :type 'simulator
                   :message (format "Failed to boot simulator %s" simulator-id)
                   :context nil
                   :timestamp (current-time)
                   :recoverable-p t
                   :recovery-fn (lambda ()
                                 (message "Shutting down all simulators...")
                                 (call-process-shell-command "xcrun simctl shutdown all")
                                 (sit-for 2)
                                 (message "Retrying simulator boot...")
                                 (ios-simulator:boot-simulator-with-id simulator-id)))))
    (swift-error-handler-handle error-obj)))

(defun swift-error-handler-app-install-failed (app-path device-id)
  "Handle app installation failure for APP-PATH on DEVICE-ID."
  (let ((error-obj (make-swift-error
                   :type 'install
                   :message "Failed to install app"
                   :context (format "App: %s, Device: %s" app-path device-id)
                   :timestamp (current-time)
                   :recoverable-p t
                   :recovery-fn (lambda ()
                                 (when (yes-or-no-p "Uninstall existing app and retry?")
                                   (call-process-shell-command 
                                    (format "xcrun simctl uninstall %s %s"
                                            device-id
                                            (xcode-additions:fetch-or-load-app-identifier)))
                                   (sit-for 1)
                                   ;; Retry installation
                                   (message "Retrying installation..."))))))
    (swift-error-handler-handle error-obj)))

(defun swift-error-handler-validate-environment ()
  "Validate the Swift development environment and report issues."
  (interactive)
  (let ((issues '()))
    
    ;; Check Xcode installation
    (condition-case nil
        (shell-command-to-string "xcode-select -p")
      (error (push "Xcode command line tools not installed" issues)))
    
    ;; Check simulator availability
    (when (string-empty-p (shell-command-to-string "xcrun simctl list devices available"))
      (push "No available simulators found" issues))
    
    ;; Check Swift version
    (condition-case nil
        (shell-command-to-string "swift --version")
      (error (push "Swift not found in PATH" issues)))
    
    ;; Report results
    (if issues
        (progn
          (with-current-buffer (get-buffer-create "*Swift Environment Issues*")
            (erase-buffer)
            (insert "Swift Development Environment Issues:\n\n")
            (dolist (issue issues)
              (insert (format "• %s\n" issue)))
            (insert "\nSuggested fixes:\n")
            (when (member "Xcode command line tools not installed" issues)
              (insert "  Run: xcode-select --install\n"))
            (when (member "No available simulators found" issues)
              (insert "  Open Xcode > Window > Devices and Simulators\n"))
            (display-buffer (current-buffer))))
      (message "Swift development environment validation passed"))))

;; Integration with existing error handling

(defun swift-error-handler-wrap-function (orig-fun &rest args)
  "Wrap ORIG-FUN with error handling, passing ARGS."
  (condition-case err
      (apply orig-fun args)
    (error
     (let ((error-obj (make-swift-error
                      :type 'generic
                      :message (error-message-string err)
                      :context (format "Function: %s" orig-fun)
                      :timestamp (current-time)
                      :recoverable-p nil
                      :recovery-fn nil)))
       (swift-error-handler-handle error-obj)
       (signal (car err) (cdr err))))))

;; Auto-recovery strategies

(defun swift-error-handler-auto-recover-build ()
  "Attempt automatic recovery from build failures."
  (message "Attempting automatic build recovery...")
  
  ;; Strategy 1: Clean module cache
  (when (file-exists-p "~/Library/Developer/Xcode/DerivedData/ModuleCache")
    (delete-directory "~/Library/Developer/Xcode/DerivedData/ModuleCache" t))
  
  ;; Strategy 2: Reset package resolved if it exists
  (when (file-exists-p "Package.resolved")
    (delete-file "Package.resolved"))
  
  ;; Strategy 3: Kill any stuck build processes
  (call-process "pkill" nil nil nil "-f" "xcodebuild")
  
  (message "Recovery steps completed. Please retry the build."))

(provide 'swift-error-handler)
;;; swift-error-handler.el ends here