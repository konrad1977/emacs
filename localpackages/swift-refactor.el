;;; swift-refactor --- A small package for refactoring -*- lexical-binding: t -*-
;;; Code:

(require 'eglot)

(defgroup swift-refactor nil
  "Provides refactoring tools for Swift."
  :group 'tools
  :prefix "swift-refactor-")

(defun swift-refactor:extract-region ()
  "Extract active region to its own function."
  (interactive)
  (swift-refactor:run-active-region #'swift-refactor:extract-function))

(defun swift-refactor:add-try-catch ()
  "Add try catch."
  (interactive)
  (swift-refactor:run-active-region #'swift-refactor:add-try-catch-with))

(defun swift-refactor:run-active-region (function)
  "Run active region with (as FUNCTION)."
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end))
          (functionToCall function))
      (beginning-of-line)
      (funcall functionToCall start end))))

(defun swift-refactor:add-try-catch-with (start end)
  "Extract region between START & END."
  (interactive)
  (ignore-errors
    (let ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (delete-region start end)
          (insert "do {\n")
          (insert content)
          (insert "} catch {\n print(error)\n}\n")
          (indent-region (region-beginning) (region-end))))))

(defun swift-refactor:extract-function (start end)
  "Extract region between START & END."
  (interactive)
  (ignore-errors
    (let ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (delete-region start end)
        (insert "extractedMethod()\n")
        (indent-region start end)
        (beginning-of-thing 'defun)
          (insert "\tprivate func extractedMethod() {\n")
          (insert content)
          (insert "\t}\n\n"))
      (indent-region (region-beginning) (region-end)))))

(provide 'swift-refactor)
;;; swift-refactor.el ends here
