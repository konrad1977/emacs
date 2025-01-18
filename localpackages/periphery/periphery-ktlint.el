;;; periphery-ktlint.el --- Validate ktlint-format and show the result as flycheck list.  -*- lexical-binding: t; -*-
;;; Commentary: Package for showing linting as result in a tabulated list
;;; Code:
(require 'periphery)
(require 'periphery-helper)
(require 'cl-lib)
(require 'mode-line-hud)

(defvar ktlint-command "ktlint")
(defvar android-kotlin-version "1.9")

(defun periphery-ktlint-parse (text)
  "Parse the output of ktlint and return a list of errors."
  (periphery-parse-ktlint-result text))

;;;###autoload
(defun periphery-ktlint-lint-buffer()
  "Lint current buffer using ktlint."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (periphery-run-ktlint-buffer
       :command (concat ktlint-command " " file))))

;;;###autoload
(defun periphery-ktlint-autocorrect-buffer()
  "Autocorrect current buffer using ktlint."
  (interactive)
  (mode-line-hud:notification :message (propertize "running ktlint" 'face 'font-lock-keyword-face) :seconds 1)
  (save-some-buffers t)
  (if-let* ((file (buffer-file-name)))
      (periphery-run-ktlint-buffer
       :command (concat ktlint-command " -F " file))))

(cl-defun periphery-run-ktlint-buffer (&key command)
  "Run ktlint for current buffer (as COMMAND FILE)."
  (if (executable-find ktlint-command)
      (progn
        (let ((output (shell-command-to-string command)))
          (if (string= output "")
              (message "ktlint: No issues found.")
            (periphery-ktlint-parse output))))
    (mode-line-hud:notification
     :message (propertize "Ktlint not installed" 'face 'font-lock-keyword-face)
     :seconds 1)))

(provide 'periphery-ktlint)
;;; periphery-ktlint.el ends here
