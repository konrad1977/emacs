;;; localizeable-strings-mode.el --- Highlight strings.

;;; Code:
(defface localizeable-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "The key in strings file."
  :group 'string-mode)

(defface localizeable-value-face
  '((t (:inherit font-lock-type-face)))
  "The value in strings file."
  :group 'string-mode)

(defface localizeable-delimiter-face
  '((t (:inherit font-lock-constant-face)))
  "The simicolon at the end."
  :group 'string-mode)

(defface localizeable-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Comments."
  :group 'string-mode)

;;;###autoload
(define-derived-mode localizeable-strings-mode fundamental-mode
  (setq indicate-empty-lines t            ;; Show empty lines
		indicate-unused-lines t           ;; Show unused lines
        display-line-numbers t
        left-fringe-width 12
        left-margin-width 1
        mode-name "Localizable")
  (setq font-lock-defaults '(())))

(font-lock-add-keywords
 'localizeable-strings-mode '(
                 ("^\\(\"[^\"]+\"\\)\s=\s\\(\"[^\"]+\"\\)"
                  (0 'localizeable-variable-face t)
                  (1 'localizeable-value-face t))
                 ("\/\\*[^*]*\\*+\\(?:[^/*][^*]*\\*+\\)*/" 0 'localizeable-comment-face t)
                 ("\\(;\\)" 0 'localizeable-delimiter-face t)))

(add-to-list 'auto-mode-alist '("\\.strings\\'" . localizeable-strings-mode))

(provide 'localizeable-strings)

;;; localizeable-strings-mode.el ends here

