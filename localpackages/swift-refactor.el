;;; swift-refactor --- A small package for refactoring -*- lexical-binding: t -*-
;;; Code:

(require 'eglot)

(defgroup swift-refactor nil
  "Provides refactoring tools for Swift."
  :group 'tools
  :prefix "swift-refactor-")

(defun swift-refactor:extract-function ()
  "Extract active region to its own function."
  (interactive)
  (swift-refactor:run-active-region #'swift-refactor:extract-function-with))

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

(defun swift-refactor:extract-function-with (start end)
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

;; Taken from  https://gitlab.com/woolsweater/dotemacs.d/-/blob/main/modules/my-swift-mode.el
;;;###autoload
(defun swift-refactor:split-function-list ()
  "While on either the header of a function-like declaration or a call to a function, split each parameter/argument to its own line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (condition-case nil
        (atomic-change-group
          (search-forward "(")
          (let ((end))
            (while (not end)
              (newline-and-indent)
              (let ((parens 0)
                    (angles 0)
                    (squares 0)
                    (curlies 0)
                    (comma))
                (while (not (or comma end))
                  (re-search-forward
                   (rx (or ?\( ?\) ?< ?> ?\[ ?\] ?{ ?} ?\" ?,))
                   (line-end-position))
                  (pcase (match-string 0)
                    ("(" (cl-incf parens))
                    (")" (if (> parens 0)
                             (cl-decf parens)
                           (backward-char)
                           (newline-and-indent)
                           (setq end t)))
                    ;; Note; these could be operators in an expression;
                    ;; there's no obvious way to fully handle that.
                    ("<" (cl-incf angles))
                    ;; At a minimum we can skip greater-than and func arrows
                    (">" (unless (zerop angles)
                           (cl-decf angles)))
                    ("[" (cl-incf squares))
                    ("]" (cl-decf squares))
                    ("{" (cl-incf curlies))
                    ("}" (cl-decf curlies))
                    ("\"" (let ((string-end))
                            (while (not string-end)
                              (re-search-forward (rx (or ?\" (seq ?\\ ?\")))
                                                 (line-end-position))
                              (setq string-end (equal (match-string 0) "\"")))))
                    ("," (when (and (zerop parens) (zerop angles)
                                    (zerop squares) (zerop curlies))
                           (setq comma t)))))))))
      (error (user-error "Cannot parse function decl or call here")))))

(provide 'swift-refactor)
;;; swift-refactor.el ends here
