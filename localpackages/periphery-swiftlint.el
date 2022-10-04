;;; Periphery-swiftlint  -*- lexical-binding: t; -*-

;;; Commentary: Package for showing all swiftlint results abulated list

;;; Code:
(require 'periphery)
(require 'projectile)
(require 'cl-lib)

(defconst swiftlint "swiftlint")

(cl-defun periphery--swiftlint:async-shell-command-to-string (&key process-name &key command &key callback)
  "Execute shell command COMMAND asynchronously in the background.
PROCESS-NAME is the name of the process."

  (let ((output-buffer (generate-new-buffer process-name))
        (callback-fun callback))
    (set-process-sentinel
     (start-process process-name output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun get-swiftlint-file-root ()
    "Get the path of the swiftlint file."
    (let* ((directory (directory-files-recursively (projectile-project-root) "\\.swiftlint\\.yml$" nil))
        (lint-directory (file-name-directory (car-safe (last directory)))))
      lint-directory))

(defun periphery--swiftlint:analyze-result (result)
  "Analyze RESULT."
  (periphery-run-parser result)
  (periphery-message-with-count
   :tag "[Done]"
   :text "Result"
   :attributes 'success))

(defun periphery-run-swiftlint ()
  "Lint the whole project not just current file."
  (interactive)
  (if (executable-find swiftlint)
      (progn
        (let ((default-directory (get-swiftlint-file-root)))
          (periphery--swiftlint:async-shell-command-to-string
           :process-name swiftlint
           :command swiftlint
           :callback #'periphery--swiftlint:analyze-result))
        (periphery-message
         :tag "[Linting]"
         :text (file-name-nondirectory (directory-file-name
                                        (file-name-directory default-directory)))
         :attributes 'success))
    (periphery-message
     :tag "[Failed]"
     :text (format "Install %s to use this command." swiftlint)
     :attributes 'warning)))

(provide 'perihery-swiftlint)

;;; periphery-swiftlint.el ends here.
