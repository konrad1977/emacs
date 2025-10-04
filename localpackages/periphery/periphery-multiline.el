;;; periphery-multiline.el --- Multi-line error reconstruction -*- lexical-binding: t -*-

;;; Commentary:
;; Handles multi-line compiler output where errors span multiple lines

;;; Code:

(require 'periphery-core)
(require 'cl-lib)

(defun periphery-parse-multiline-compiler-output (input)
  "Parse INPUT that may contain multi-line compiler errors.
Reconstructs split error messages and passes complete lines to the parser."
  (let ((lines (split-string input "\n"))
        (current-error nil)
        (reconstructed-lines '()))
    
    (dolist (line lines)
      (cond
       ;; Start of a new error - has path and line number
       ((and (string-match "^\\(/[^:]+\\):\\([0-9]+\\):" line)
             (not (string-match ":\\s*\\(error\\|warning\\|note\\):" line)))
        ;; This line has a path but might be missing the error type
        ;; Look ahead to see if we need to combine with next line
        (setq current-error line))
       
       ;; Continuation of previous line - might have the error type
       ((and current-error
             (string-match "^\\s*\\(error\\|warning\\|note\\):" line))
        ;; Combine with previous line
        (let ((combined (concat current-error " " (string-trim line))))
          (push combined reconstructed-lines))
        (setq current-error nil))
       
       ;; Complete error line
       ((string-match "^\\(/[^:]+\\):\\([0-9]+\\):\\(?:[0-9]+:\\)?\\s*\\(error\\|warning\\|note\\):" line)
        (push line reconstructed-lines)
        (setq current-error nil))
       
       ;; Regular line, preserve as is
       (t
        (when current-error
          ;; Previous line was incomplete, add it as is
          (push current-error reconstructed-lines)
          (setq current-error nil))
        (push line reconstructed-lines))))
    
    ;; Handle any remaining current-error
    (when current-error
      (push current-error reconstructed-lines))
    
    ;; Join back and parse
    (let ((reconstructed (string-join (nreverse reconstructed-lines) "\n")))
      (when periphery-debug
        (message "Reconstructed %d lines" (length reconstructed-lines)))
      (periphery-core-parse :input reconstructed :type :compiler))))

;; Override the default parser to use multiline handling
(defun periphery-run-parser-multiline (input)
  "Parse INPUT with multi-line error handling."
  (when periphery-debug
    (message "periphery-run-parser-multiline called"))
  (let ((errors (periphery-parse-multiline-compiler-output input)))
    (setq periphery-errorList errors)
    (when (or (periphery--is-buffer-visible) periphery-errorList)
      (periphery--listing-command periphery-errorList))
    (not (null periphery-errorList))))

(provide 'periphery-multiline)
;;; periphery-multiline.el ends here