;;; strings-mode.el --- Highlight strings.

(defface strings-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "The key in strings file."
  :group 'string-mode)

(defface strings-value-face
  '((t (:inherit font-lock-type-face)))
  "The value in strings file."
  :group 'string-mode)

(defface strings-delimiter-face
  '((t (:inherit font-lock-constant-face)))
  "The simicolon at the end."
  :group 'string-mode)

(defface strings-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Comments."
  :group 'string-mode)

(define-derived-mode strings-mode fundamental-mode
  (setq font-lock-defaults '(()))
  (setq mode-name "Strings"))

(font-lock-add-keywords
 'strings-mode '(
                 ("^\\(\"[^\"]+\"\\)\s=\s\\(\"[^\"]+\"\\)"
                  (0 'strings-variable-face t)
                  (1 'strings-value-face t))
                 ("\\/\\*[^*]*\\*/" 0 'strings-comment-face t)
                 ("\\(;\\)" 0 'strings-delimiter-face t)))

;;; Code:
(provide 'strings-mode)

