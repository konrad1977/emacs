;;; swift-refactor.el --- A small package for refactoring -*- lexical-binding: t -*-
;;; Code:

(defgroup swift-refactor nil
  "Provides refactoring tools for Swift."
  :group 'tools
  :prefix "swift-refactor-")

(defun clean-up-region-whitespace (start end)
  "Delete extra whitespace in the region between START and END."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+" nil t)
      (replace-match " " nil nil))))

(defun delete-to-next-closing-brace ()
  "Delete all text between the current line and the next closing brace }, but not including the brace itself."
  (interactive)
  (let ((start (point)))
    (when (re-search-forward "\w+\s?{\\|}" nil t)
    ;; (when (re-search-forward "\\w+\s?\\(?![\\(\\)]\\){}|\\}" nil t)
      (forward-char)
      (forward-line -1)
      (delete-region start (point))
      (indent-region start (point)))))

(cl-defun swift-refactor:delete-until-balancing-char (&key opening-char &key closing-char)
  "Deletes the current line starting with OPENING-CHAR and the matching CLOSING-CHAR brace somewhere below it."
  (interactive)
  (save-excursion
    (end-of-line)
    (when (looking-back opening-char nil t)
      (delete-line)
      (let ((brace-count 1)
            (closing-brace-pos nil)
            (opening-brace-pos (line-beginning-position)))
        (while (and (not closing-brace-pos)
                    (re-search-forward (format "[%s]" closing-char) nil t))
          (if (eq (char-after) (format "?\%s" opening-char))
              (setq brace-count (1+ brace-count))
            (setq brace-count (1- brace-count)))
          (when (eq brace-count 0)
            (setq closing-brace-pos (point))))
        (when closing-brace-pos
          (delete-region (1- closing-brace-pos) (1+ closing-brace-pos))
          (forward-line 2)
          (delete-to-next-closing-brace)
          (indent-region (1- opening-brace-pos) (line-end-position)))))))

(defun swift-refactor:delete-current-line-with-matching-brace ()
  "Deletes the current line starting with '{' and the matching '}' brace somewhere below it."
  (interactive)
  (swift-refactor:delete-until-balancing-char :opening-char "{" :closing-char "}"))

(defun swift-refactor:insert-at (start end name)
  "Insert a an element with NAME and '{' and ending '}' using START AND END."
  (ignore-errors
    (save-excursion
      (goto-char start)
      (insert  (concat name " {\n"))
      (goto-char end)
      (forward-line)
      (insert "}\n")
      (indent-region (1- start) (line-end-position)))))

(defun swift-refactor:insert-around (name)
  "Insert element around selection (as NAME)."
  (interactive "sEnter element name: ")
  (let ((name (if (string-blank-p (string-trim-right name)) "Element" name)))
    (swift-refactor:run-active-region #'swift-refactor:insert-at name)))

(defun swift-refactor:run-active-region (function &rest args)
  "Run active region with (as FUNCTION) and pass any additional ARGS to FUNCALL."
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (beginning-of-line)
      (when (fboundp 'function)
        (apply #'funcall function start end args)))))

(defun swift-refactor:extract-function (method-name)
  "Extract active region to its own function (as METHOD-NAME)."
  (interactive "sEnter method name (optional): ")
  (let ((method-name (if (string-blank-p (string-trim-right method-name)) "extractedMethod" method-name)))
    (swift-refactor:run-active-region #'swift-refactor:extract-function-with method-name)))

(defun swift-refactor:tidy-up-constructor ()
  "Clean up constructor from Type.init() to Type()."
  (interactive)
  (swift-refactor:run-active-region #'swift-refactor:tidy-up-constructor-with))

(defun swift-refactor:extract-function-with (start end method-name)
  "Extracts the Swift code region between START and END into a new function with the given METHOD-NAME."
  (ignore-errors
    (let* ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (kill-region start end)
        (insert (concat method-name "()\n"))
        (beginning-of-defun)
        (insert (concat "\tprivate func " method-name "() {\n"))
        (insert content)
        (insert "\t}\n\n"))
      (indent-region (region-beginning) (region-end))
      (indent-according-to-mode))))

(defun swift-refactor:tidy-up-constructor-with (start end)
  "Clean up the constructor and removes .init from code."
  (ignore-errors
    (let* ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (kill-region start end)
        (insert (remove-init-from-string content))))))

(defun swift-refactor:add-try-catch ()
  "Add try catch."
  (interactive)
  (swift-refactor:run-active-region #'swift-refactor:add-try-catch-with))

(defun swift-refactor:add-try-catch-with (start end)
  "Extract region between START & END."
  (ignore-errors
    (let ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (delete-region start end)
          (insert "do {\n")
          (insert content)
          (insert "} catch {\n print(error)\n}\n")
          (indent-region (region-beginning) (region-end))
          (indent-according-to-mode)))))

(defun remove-init-from-string (string)
  "Remove '.init' from the given STRING."
  (replace-regexp-in-string "\\.init" "" string))

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
