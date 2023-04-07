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

(defgroup overlay-usage nil
  "Plugin shows complexity information."
  :prefix "overlay-usage-"
  :group 'comm)

(defface overlay-usage-default-face
  '((t :font "Iosevka Aile" :height 0.7 :foreground "#898989"))
  "Face added to code-usage display."
  :group 'overlay-usage)

(defface overlay-usage-function-symbol-face
  '((t :inherit font-lock-function-name-face :height 0.7 :weight semi-bold))
  "Face added to code-usage display."
  :group 'overlay-usage)

(defface overlay-usage-variable-symbol-face
  '((t :inherit font-lock-keyword-face :height 0.7))
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
  (add-hook 'after-save-hook (lambda ()
   (overlay-add-to-functions)
   (overlay-add-to-variables)) nil t)
  
  (overlay-add-to-functions)
  (overlay-add-to-variables))


(defun overlay-usage-disable ()
  "Disable overlay-usage-mode."
  (overlay-recenter (point-max))
  (remove-hook 'after-save-hook (lambda ()
                                   (overlay-usage-remove-overlays-for-functions)
                                   (overlay-usage-remove-overlays-for-variables)) t)
  (overlay-usage-remove-overlays-for-functions)
  (overlay-usage-remove-overlays-for-variables))


(defun overlay-usage-remove-overlays-for-functions ()
  "Clean up all overlays for functions."
  (mapc #'delete-overlay functions-overlays-list))


(defun overlay-usage-remove-overlays-for-variables ()
  "Clean up all overlays for variables."
  (mapc #'delete-overlay variables-overlays-list))


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
              (line-beginning-position 0)
              (line-beginning-position 0))))

    (overlay-put ov 'after-string
                 (concat spaces
                 (propertize-with-symbol (- count 1) "λ︎" 'overlay-usage-function-symbol-face)))
    (overlay-put ov 'end (+ (line-beginning-position 0) (length spaces)))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'priority 1000)
    (push ov functions-overlays-list)))


(cl-defun add-overlays-for-variables (&key position filename)
  "Add overlay (as POSITION with SPACES) for variables."
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
    (message command)
    (overlay-put ov 'after-string
                 (propertize-with-symbol count "⇠" 'overlay-usage-variable-symbol-face))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'priority 900)
    (push ov variables-overlays-list)))


(defun propertize-with-symbol (count symbol font)
  "Propertize with symbol (as COUNT as SYMBOL as FONT-FACE)."
  (cond
   ((< count 1)
    (concat (propertize (format " %s︎ " symbol) 'face font)
            (propertize "No references found" 'face 'overlay-usage-default-face)))
   ((= count 1)
    (concat (propertize (format " %s︎ " symbol) 'face font)
            (propertize "Found 1 reference" 'face 'overlay-usage-default-face)))
   ((> count 1)
    (concat (propertize (format " %s " symbol) 'face font)
            (propertize (concat "Found " (number-to-string count) " references")
                                'face 'overlay-usage-default-face)))))


(defun extension-from-file ()
  "Get file extension."
  (file-name-extension buffer-file-name))


(cl-defun shell-command-functions-from (&key extension function)
  "Shell command from EXTENSION and FUNCTION."
  (cond
   ((string-suffix-p "swift" extension t)
    (format "rg --glob '*.%s' -e '%s\\(' | wc -l" extension function))
   ((string-suffix-p "el" extension t)
    (format "rg --glob '*.%s' -e '%s' | wc -l" extension function))))

(cl-defun shell-command-variable-from (&key filename variable)
  "Shell command from FILENAME and VARIABLE."
  (cond
   ((string-suffix-p "swift" (file-name-extension filename) t)
    (format "rg %s -e '\\b%s\\b(?!:)' --pcre2 | wc -l" filename variable))))


(defun regex-for-file-type (extension)
  "Detect what the function start with from the (EXTENSION)."
  (cond
   ((string-match-p (regexp-quote "swift") extension) "func")
   ((string-match-p (regexp-quote "el") extension) "defun")))


(defun overlay-add-to-functions ()
  "Add overlay to functions."
  (overlay-usage-remove-overlays-for-functions)
  (save-excursion
    (let* ((extension (extension-from-file))
           (func-regex (regex-for-file-type extension))
           (default-directory (project-root-dir)))

      (goto-char (point-min))
      
      (while (search-forward-regexp (concat func-regex " \\([a-zA-Z0-9_-\(]+\\)") nil t)
        (let ((position (match-beginning 1))
              (column (save-excursion
                        (back-to-indentation)
                        (current-column))))
          (add-overlays-for-functions
           :position position
           :spaces (spaces-string column)
           :extension (format "%s" extension)))))))


(defun overlay-add-to-variables ()
  "Add overlays to variables."
  (message "Adding overlay to variables")
  (overlay-usage-remove-overlays-for-variables)
  (save-excursion
    (let ((default-directory (project-root-dir)))
     (goto-char (point-min))
      
      (while (search-forward-regexp "\\b\\(?:let\\|var\\)\s+\\(\\w+\\)" nil t)
        (let ((position (match-beginning 1)))
          (add-overlays-for-variables
           :position position
           :filename (buffer-file-name)))))))

(provide 'overlay-usage)
;;; overlay-usage.el ends here
