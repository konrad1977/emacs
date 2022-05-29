
;;; Periphery-swiftlint 

;;; Commentary: Package for showing all swiftlint results abulated list

;;; Code:
(require 'periphery)

(defconst swiftlint "swiftlint")

(defun async-shell-command-to-string (process-name command callback)
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
    (interactive)
    (let* (
        (directory (directory-files-recursively (vc-root-dir) "\\.swiftlint\\.yml$" nil))
        (lint-directory (file-name-directory (car-safe (last directory)))))
      lint-directory))

(defun periphery-swiftlint-analyze-result (result)
  "Analyze RESULT."
  (periphery-run-parser result)
  (periphery-message :tag "[Done]" :text "Linting done" :attributes 'success))

(defun periphery-run-swiftlint ()
  "Lint the whole project not just current file."
  (interactive)
  (if (executable-find swiftlint)
      (progn
        (let ((default-directory (get-swiftlint-file-root)))
          (async-shell-command-to-string swiftlint swiftlint #'periphery-swiftlint-analyze-result))
        (periphery-message :tag "[Linting]" :text "Linting project" :attributes 'success))
    (periphery-message :tag "[Failed]" :text (format "Install %s to use this command." swiftlint) :attributes 'warning)))

(provide 'perihery-swiftlint)
;;; periphery-swiftlint.el ends here.
