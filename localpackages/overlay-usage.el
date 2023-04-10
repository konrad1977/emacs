;;; overlay-usage.el --- ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Package for showing usage of function.

;;; Code:

(require 'rect)
(require 'project)

(defvar overlay-usage-mode nil)

(defvar-local functions-overlays-list nil
  "List of overlays for functions.")

(defvar-local variables-overlays-list nil
  "List of overlays for variables.")

(defvar-local classes-overlays-list nil
  "List of overlays for structs and classes.")

(defgroup overlay-usage nil
  "Plugin shows complexity information."
  :prefix "overlay-usage-"
  :group 'comm)

(defface overlay-usage-default-face
  '((t :inherit font-lock-comment-face :foreground "#999999" :height 0.7 :italic nil))
  "Face added to code-usage display."
  :group 'overlay-usage)

(defface overlay-usage-function-symbol-face
  '((t :inherit font-lock-function-name-face :height 0.8 :weight semi-bold))
  "Face added to code-usage display."
  :group 'overlay-usage)

(defface overlay-usage-class-symbol-face
  '((t :inherit font-lock-constant-face :height 0.8 :weight semi-bold))
  "Face added to code-usage display."
  :group 'overlay-usage)

(defface overlay-usage-variable-symbol-face
  '((t :inherit font-lock-keyword-face :height 0.7 :weight semi-bold))
  "Face added to code-usage display."
  :group 'overlay-usage)

(define-minor-mode overlay-usage-mode
  "Toggle overlay-usage-mode."
  :group overlay-usage
  :init-value nil
  :lighter "OverlayUsage"
  (if overlay-usage-mode
      (overlay-usage-enable)
    (overlay-usage-disable)))


(defun project-root-dir ()
  "Get the root directory of the current project."
  (let ((project (project-current)))
    (when project
      (project-root project))))


(defun overlay-usage-enable ()
  "Enable overlay-usage."
  (add-hook 'after-save-hook (lambda () (overlay-usage:add-all-overlays)) nil t)
  (overlay-usage:add-all-overlays))


(defun overlay-usage-disable ()
  "Disable overlay-usage-mode."
  (overlay-recenter (point-max))
  (remove-hook 'after-save-hook (lambda () (overlay-usage:remove-all-overlays) t))
  (overlay-usage:remove-all-overlays))


(defun overlay-usage:add-all-overlays ()
  "Add all overlays."
  (let ((default-directory (project-root-dir)))
    (overlay-usage:remove-all-overlays)
    (overlay-add-to-functions)
    (overlay-add-to-variables)
    (overlay-add-to-classes-and-structs)))


(defun overlay-usage:remove-all-overlays ()
  "Remove all overlays."
  (overlay-usage:remove-overlays-for-functions)
  (overlay-usage:remove-overlays-for-variables)
  (overlay-usage:remove-overlays-for-classes))


(defun overlay-usage:remove-overlays-for-functions ()
  "Clean up all overlays for functions."
  (mapc #'delete-overlay functions-overlays-list))


(defun overlay-usage:remove-overlays-for-variables ()
  "Clean up all overlays for variables."
  (mapc #'delete-overlay variables-overlays-list))

(defun overlay-usage:remove-overlays-for-classes ()
  "Clean up all overlays for classes."
  (mapc #'delete-overlay classes-overlays-list))


(defun overlay-usage:previous-line-has-text-p ()
  "Return non-nil if the previous line has any non-whitespace text."
  (save-excursion
    (forward-line -1) ; Move to previous line
    (looking-at "\\S-"))) ; Check if line has non-whitespace text


(cl-defun add-overlays-for-functions (&key position spaces extension)
  "Add overlay (as POSITION with SPACES and search EXTENSION)."
  (goto-char position)
  (let* ((function-name (thing-at-point 'symbol))
         (extension extension)
         (count (string-to-number
                 (shell-command-to-string
                  (shell-command-functions-from
                   :extension extension
                   :function function-name))))
         (ov (make-overlay
              (line-end-position 0)
              (line-end-position 0))))

    (overlay-put ov 'after-string
                 (concat (if (overlay-usage:previous-line-has-text-p) spaces " ")
                 (propertize-with-symbol (- count 1) "λ︎" 'overlay-usage-function-symbol-face)))
    (push ov functions-overlays-list)))


(cl-defun add-overlays-for-variables (&key position filename)
  "Add overlay (as POSITION and FILENAME) for variables."
  (goto-char position)
  (let* ((variable-name (thing-at-point 'symbol))
         (filename filename)
         (command (shell-command-variable-from
                   :filename filename
                   :variable variable-name))
         (count (string-to-number (shell-command-to-string command)))
         (ov (make-overlay
              (line-end-position)
              (line-end-position))))
    (overlay-put ov 'after-string
                 (concat " "
                 (propertize-with-symbol count "⇠" 'overlay-usage-variable-symbol-face)))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'priority 900)
    (push ov variables-overlays-list)))

(cl-defun add-overlays-for-classes (&key position spaces extension)
  "Add overlay for classes (as POSITION with SPACES and search EXTENSION)."
  (goto-char position)
  (let* ((class-name (thing-at-point 'symbol))
         (extension extension)
         (count (string-to-number
                 (shell-command-to-string
                  (overlay-usage:shell-command-classes-from
                   :extension extension
                   :name class-name))))
         (ov (make-overlay
              (line-end-position 0)
              (line-end-position 0))))

    (overlay-put ov 'after-string
                 (concat (if (overlay-usage:previous-line-has-text-p) spaces " ")
                 (propertize-with-symbol count "✦︎" 'overlay-usage-class-symbol-face)))
    (push ov classes-overlays-list)))


(defun propertize-with-symbol (count symbol font)
  "Propertize with symbol (as COUNT as SYMBOL as FONT)."
  (cond
   ((< count 1)
    (concat (propertize (format "%s︎ " symbol) 'face font)
            (propertize "No references found" 'face 'overlay-usage-default-face)))
   ((= count 1)
    (concat (propertize (format "%s︎ " symbol) 'face font)
            (propertize "1 reference" 'face 'overlay-usage-default-face)))
   ((> count 1)
    (concat (propertize (format "%s " symbol) 'face font)
            (propertize (concat " " (number-to-string count) " references")
                                'face 'overlay-usage-default-face)))))


(defun extension-from-file ()
  "Get file extension."
  (file-name-extension buffer-file-name))


(cl-defun shell-command-functions-from (&key extension function)
  "Shell command from EXTENSION and FUNCTION."
  (cond
   ((string-suffix-p "swift" extension t)
    (format "rg -t swift -e '^[^\/]*\\b%s\\(' --pcre2 | wc -l" function))
   ((string-suffix-p "el" extension t)
    (format "rg -t elisp -e '^[^;\/\n].*\\b%s\\b' --pcre2 | wc -l" function))))


(cl-defun overlay-usage:shell-command-classes-from (&key extension name)
  "Shell command from EXTENSION and NAME."
  (cond
   ((string-suffix-p "swift" extension t)
    (format "rg -t swift -e '^[^\/]*\\b%s\\b[\\(|\.init]' --pcre2 | wc -l" name))))


(cl-defun shell-command-variable-from (&key filename variable)
  "Shell command from FILENAME and VARIABLE."
  (cond
   ((string-suffix-p "swift" (file-name-extension filename) t)
    (format "rg -t swift %s -e '^[^\/]*\\b%s\\b(?!:)' --pcre2 | wc -l" filename variable))))


(defun overlay-usage:find-function-regex-for-file-type (extension)
  "Detect what the function start with from the (EXTENSION)."
  (cond
   ((string-match-p (regexp-quote "swift") extension) "^[^\/\n].*\s+\\bfunc\\b")
   ((string-match-p (regexp-quote "el") extension) "^[^;\n].*\\bdefun\\b")))


(defun overlay-add-to-functions ()
  "Add overlay to functions."
  (save-excursion
    (let* ((extension (extension-from-file))
           (func-regex (overlay-usage:find-function-regex-for-file-type extension)))

      (goto-char (point-min))
      (while (search-forward-regexp (concat func-regex " \\([a-zA-Z0-9_-\(]+\\)") nil t)
        (let ((position (match-beginning 1))
              (column (save-excursion
                        (back-to-indentation)
                        (current-column))))
          (beginning-of-line)
          (when (not (looking-at "^\\s-*/\\|;"))
            (add-overlays-for-functions
             :position position
             :spaces (spaces-string column)
             :extension (format "%s" extension))))
        (forward-line)))))


(defun overlay-add-to-variables ()
  "Add overlays to variables."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "\s+\\b\\(?:let\\|var\\)\s+\\(\\w+\\)" nil t)
      (let ((position (match-beginning 1)))
        (beginning-of-line)
        (when (not (looking-at "^\\s-*//"))
          (add-overlays-for-variables
           :position position
           :filename (buffer-file-name))))
      (forward-line))))


(defun overlay-add-to-classes-and-structs ()
  "Add overlays for structs and classes."
  (save-excursion
    (goto-char (point-min))
    (let ((extension (extension-from-file)))
      (while (search-forward-regexp "\\b\\(?:struct\\|class\\)\s+\\(\\w+\\)" nil t)
        (let ((position (match-beginning 1))
              (column (save-excursion
                        (back-to-indentation)
                        (current-column))))
          (beginning-of-line)
          (when (not (looking-at "^\\s-*//"))
            (add-overlays-for-classes
             :position position
             :spaces (spaces-string column)
             :extension extension)))
        (forward-line)))))

(provide 'overlay-usage)
;;; overlay-usage.el ends here
