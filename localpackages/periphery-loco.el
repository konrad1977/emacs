;;; Periphery-loco --- Validate .strings and .swift file and show the result as flycheck list.  -*- lexical-binding: t; -*-

;;; Commentary: Package for showing linting as result in a tabulated list

;;; Code:
(require 'periphery)

(defvar loco-command "loco")

(defun periphery-loco:async-shell-command-to-string (process-name command callback)
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

(defun send-loco-result-to-periphery (text)
  "Let periphery parse the (as TEXT)."
  (periphery-run-parser text))

(defun periphery-loco:run-linter ()
  "Run linter."
  (interactive)
  (if (executable-find loco-command)
      (progn
        (let ((default-directory (vc-root-dir)))
          (periphery-message :tag "[Loco]" :text "Linting, stand by..." :attributes 'info)
          (periphery-loco:async-shell-command-to-string "loco" (concat loco-command " --no-color") #'send-loco-result-to-periphery))
        )
    (periphery-message :tag "[Failed]" :text (format "Install %s to use this command." loco-command) :attributes 'warning)))

(provide 'perihery-loco)
;;; periphery-loco.el ends here.

