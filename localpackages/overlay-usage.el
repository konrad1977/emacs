;;; overlay-usage.el --- Show function, variable and class usage -- lexical-binding: t; --
;;; Commentary:
;;; Package for showing usage counts of functions, variables, and classes.

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

(defconst commented-lines-regex "^\\s-*/\\|;"
  "Comment-line regex.")

(defcustom overlay-usage-language-settings
  '((swift . ((extension . "swift")
              (function-regex . "^[^/\n]*\\_<func\\_>\\s+\\([^(=]+\\)")
              (function-usage-regex . "\\b%s\\b")
              (variable-regex . "\\s+\\b\\(?:let\\|var\\|case\\)\\b\\s+\\(\\w+\\)")
              (variable-usage-regex . "^[^/\n\"]*(?<!\\.)\\b%s\\b(?!\\s*[:=])")
              (class-regex . "\\b\\(?:\\bstruct\\b\\|\\bclass\\b\\)\\s+\\(\\w+\\)")
              (private-keywords . ("private" "fileprivate"))
              (comment-prefix . "//")))
    (kotlin . ((extension . "kt")
               (function-regex . "^[^/\n]*\\_<fun\\_>\\s+\\([^(=]+\\)")
               (function-usage-regex . "\\b%s\\b")
               (variable-regex . "\\b\\(?:val\\|var\\)\\b\\s+\\(\\w+\\)")
               (variable-usage-regex . "^[^/\n\"]*(?<!\\.)\\b%s\\b(?!\\s*[:=])")
               (class-regex . "\\b\\(?:class\\|interface\\|object\\)\\b\\s+\\(\\w+\\)")
               (private-keywords . ("private"))
               (comment-prefix . "//")))
    (elisp . ((extension . "el")
              (function-regex . "^[^;\n]*\\(defun\\s-+\\([^[:space:]()]+\\)")
              (function-usage-regex . "\\b%s\\b")
              (variable-regex . "\\(?:defvar\\|defvar-local\\|defface\\|defconst\\|defgroup\\)\\s-+\\([^[:space:]()]+\\)")
              (variable-usage-regex . "^(?!.*\\(def\\w+).*\\b%s\\b(?!:)")
              (class-regex . nil)
              (private-keywords . nil)
              (comment-prefix . ";"))))
  "Settings for different programming languages."
  :type '(alist :key-type symbol :value-type (alist :key-type symbol :value-type sexp))
  :group 'overlay-usage)

(defcustom overlay-usage-supported-extensions
  '("swift" "kt" "el")
  "List of supported file extensions."
  :type '(repeat string)
  :group 'overlay-usage)

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

(defface overlay-usage-function-symbol-face-public
  '((t :inherit font-lock-type-face :height 0.8 :weight semi-bold))
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

(defface overlay-usage-count-symbol-face
  '((t :inherit default :height 0.7 :weight semi-bold))
  "Face added to code-usage display."
  :group 'overlay-usage)

;;;###autoload
(define-minor-mode overlay-usage-mode
  "Toggle 'overlay-usage-mode'."
  :group overlay-usage
  :init-value nil
  :lighter "OverlayUsage"
  (if overlay-usage-mode
      (overlay-usage-enable)
    (overlay-usage-disable)))

(defun overlay-usage:get-language-for-extension (extension)
  "Get language settings for the given file EXTENSION.
Returns the language symbol if found, otherwise nil."
  (car (cl-find-if (lambda (lang-settings)
                     (string= extension
                             (alist-get 'extension (cdr lang-settings))))
                   overlay-usage-language-settings)))

(defun overlay-usage:get-language-setting (language setting)
  "Get SETTING for LANGUAGE from language settings."
  (alist-get setting (alist-get language overlay-usage-language-settings)))

(defun overlay-usage:project-root-dir ()
  "Get the root directory of the current project."
  (when-let ((project (project-current)))
    (project-root project)))

(defun overlay-usage-enable ()
  "Enable overlay-usage."
  (add-hook 'after-save-hook #'overlay-usage:update-all-buffer nil t)
  (add-hook 'after-revert-hook #'overlay-usage:update-all-buffer nil t)
  (overlay-usage:add-all-overlays))

(defun overlay-usage-disable ()
  "Disable 'overlay-usage-mode'."
  (remove-hook 'after-save-hook #'overlay-usage:update-all-buffer t)
  (remove-hook 'after-revert-hook #'overlay-usage:update-all-buffer t)
  (overlay-usage:remove-all-overlays))

(defun overlay-usage:buffer-visible-p (buffer)
  "Return non-nil if BUFFER is visible in any window."
  (not (eq (get-buffer-window buffer 'visible) nil)))

(defun overlay-usage:update-all-buffer ()
  "Update all buffers."
  (dolist (buf (buffer-list))
    (when (overlay-usage:buffer-visible-p buf)
      (with-current-buffer buf
        (overlay-usage:add-all-overlays)))))

(defun overlay-usage:add-all-overlays ()
  "Add all overlays."
  (overlay-usage:remove-all-overlays)
  (let* ((default-directory (overlay-usage:project-root-dir))
         (extension (overlay-usage:extension-from-file))
         (language (overlay-usage:get-language-for-extension extension)))
    (when language
      (if (overlay-usage:get-language-setting language 'class-regex)
          (progn
            (overlay-usage:setup-functions :extension extension :private t)
            (overlay-usage:setup-classes-and-structs))
        (overlay-usage:setup-functions :extension extension :private nil)
        (overlay-usage:setup-variables)))))

(defun overlay-usage:remove-all-overlays ()
  "Remove all overlays."
  (dolist (overlay-list (list functions-overlays-list variables-overlays-list classes-overlays-list))
    (mapc #'delete-overlay overlay-list)))

(defun overlay-usage:propertize-with-symbol (count symbol font)
  "Propertize with symbol (as COUNT as SYMBOL as FONT)."
  ;; Ensure symbol is a string
  (let ((symbol-str (if (stringp symbol) symbol ""))
        (count-val (if (numberp count) count 0)))
    (cond
     ((< count-val 1)
      (concat (propertize (format "%s︎ " symbol-str) 'face font)
              (propertize "No references found" 'face 'overlay-usage-default-face)))
     ((= count-val 1)
      (concat (propertize (format "%s︎ " symbol-str) 'face font)
              (propertize "1" 'face 'overlay-usage-count-symbol-face)
              (propertize " reference" 'face 'overlay-usage-default-face)))
     (t
      (concat (propertize (format "%s " symbol-str) 'face font)
              (propertize (number-to-string count-val) 'face 'overlay-usage-count-symbol-face)
              (propertize " references" 'face 'overlay-usage-default-face))))))

(defun overlay-usage:extension-from-file ()
  "Get file extension."
  (file-name-extension buffer-file-name))

(cl-defun add-overlays-for-functions (position spaces)
  "Add overlay at POSITION with SPACES for functions."
  (goto-char position)
  (let* ((filename (buffer-file-name))
         (function-name (thing-at-point 'symbol))
         (shell-command (overlay-usage:shell-command-functions-from
                        :filename filename
                        :function (regexp-quote function-name)))
         (count-str (when shell-command
                     (shell-command-to-string shell-command)))
         (count (string-to-number (if (and count-str (stringp count-str))
                                    (string-trim count-str)
                                    "0")))
         (ov (make-overlay
              (line-end-position 0)
              (line-end-position 0))))
    (overlay-put ov 'after-string
                 (concat spaces
                         (overlay-usage:propertize-with-symbol (max 0 (1- count)) "λ︎"
                          'overlay-usage-function-symbol-face-public)))
    (push ov functions-overlays-list)))

(defun add-overlays-for-variables (position)
  "Add overlay at POSITION for variables."
  (goto-char position)
  (let* ((variable-name (thing-at-point 'symbol))
         (shell-command (shell-command-variable-from
                        :filename (buffer-file-name)
                        :variable (regexp-quote variable-name)))
         (count-str (when shell-command
                     (shell-command-to-string shell-command)))
         (count (string-to-number (if (and count-str (stringp count-str))
                                    (string-trim count-str)
                                    "0")))
         (ov (make-overlay
              (line-end-position)
              (line-end-position))))
    (overlay-put ov 'after-string
                 (concat " " (overlay-usage:propertize-with-symbol (max 0 (1- count)) "⇠" 'overlay-usage-variable-symbol-face)))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'priority 900)
    (push ov variables-overlays-list)))

(cl-defun add-overlays-for-classes (&key position spaces extension)
  "Add overlay for classes (as POSITION with SPACES and search EXTENSION)."
  (goto-char position)
  (let* ((class-name (thing-at-point 'symbol))
         (shell-command (overlay-usage:shell-command-classes-from
                        :extension extension
                        :name class-name))
         (count-str (when shell-command
                     (shell-command-to-string shell-command)))
         (count (string-to-number (if (and count-str (stringp count-str))
                                    (string-trim count-str)
                                    "0")))
         (ov (make-overlay
              (line-end-position 0)
              (line-end-position 0)))
         (spaces-str (if (stringp spaces) spaces "")))
    (overlay-put ov 'after-string
                 (concat spaces-str
                         (overlay-usage:propertize-with-symbol (- count 1) "✦︎" 'overlay-usage-class-symbol-face)))
    (push ov classes-overlays-list)))

(defun overlay-usage:shell-command-functions-from (&key filename function)
  "Shell command to search for FUNCTION in FILENAME."
  (when-let* ((extension (file-name-extension filename))
              (language (overlay-usage:get-language-for-extension extension)))
    (cond
     ((eq language 'elisp)
      (message "Elisp function search command for %s: %s" function
        (format "rg -t elisp -c '\\\\(\\b%s\\b\\|[[:space:]]%s\\b' %s"
                function function filename)))
     (t
      (let ((extension-setting (overlay-usage:get-language-setting language 'extension)))
        (format "rg -t %s -c '\\\\b%s\\\\b' %s"
                extension-setting function filename))))))

(cl-defun shell-command-variable-from (&key filename variable)
  "Shell command from FILENAME and VARIABLE."
  (when-let* ((extension (file-name-extension filename))
              (language (overlay-usage:get-language-for-extension extension)))
    (cond
     ((eq language 'elisp)
      (message "Elisp variable search command for %s: %s" variable
        (format "rg -t elisp -c '\\\\(def\\\\(?:var\\|const\\|custom\\\\)\\\\s+%s\\\\b\\|\\\\b%s\\\\b\\\\)' --pcre2 %s"
                variable variable filename)))
     (t
      (let ((extension-setting (overlay-usage:get-language-setting language 'extension)))
        (format "rg -t %s -c '\\\\b%s\\\\b' %s"
                extension-setting variable filename))))))

(cl-defun overlay-usage:shell-command-classes-from (&key extension name)
  "Shell command to search for class NAME in files with EXTENSION."
  (when-let* ((language (overlay-usage:get-language-for-extension extension)))
    (format "rg -t %s -c '%s' ."
            (overlay-usage:get-language-setting language 'extension)
            (format (overlay-usage:get-language-setting language 'class-regex) name))))

(defun overlay-usage:setup-classes-and-structs ()
  "Add overlays for structs and classes."
  (save-excursion
    (let* ((extension (overlay-usage:extension-from-file))
           (language (overlay-usage:get-language-for-extension extension))
           (classes-regex (overlay-usage:get-language-setting language 'class-regex)))
      (when classes-regex
        (goto-char (point-min))
        (while (re-search-forward classes-regex nil t)
          (let ((position (match-beginning 1))
                (column (save-excursion
                         (goto-char (match-beginning 0))
                         (current-column))))
            (beginning-of-line)
            (unless (looking-at commented-lines-regex)
              (add-overlays-for-classes
               :position position
               :spaces (spaces-string column)
               :extension extension)))
          (forward-line))))))

(cl-defun overlay-usage:setup-functions (&key extension private)
  "Add overlay to functions with EXTENSION as PRIVATE."
  (save-excursion
    (let* ((language (overlay-usage:get-language-for-extension extension))
           (func-regex (overlay-usage:get-language-setting language 'function-regex))
           (private-keywords (overlay-usage:get-language-setting language 'private-keywords)))
      (when func-regex
        (goto-char (point-min))
        (while (re-search-forward func-regex nil t)
          (let ((position (match-beginning 1))
                (column (save-excursion
                         (goto-char (match-beginning 0))
                         (current-column))))
            (beginning-of-line)
            (when (and private-keywords
                      (eq private (looking-at (format "^\\s-*\\b\\(%s\\)\\b"
                                                    (string-join private-keywords "\\|")))))
              (unless (looking-at commented-lines-regex)
                (add-overlays-for-functions position (spaces-string column)))))
          (forward-line))))))

(defun overlay-usage:setup-variables ()
  "Add overlays to variables."
  (save-excursion
    (let* ((extension (overlay-usage:extension-from-file))
           (language (overlay-usage:get-language-for-extension extension))
           (variable-regex (overlay-usage:get-language-setting language 'variable-regex)))
      (when variable-regex
        (goto-char (point-min))
        (while (re-search-forward variable-regex nil t)
          (let ((position (match-beginning 1)))
            (beginning-of-line)
            (unless (looking-at commented-lines-regex)
              (add-overlays-for-variables position)))
          (forward-line))))))

(defun spaces-string (column)
  "Create a string of spaces for indentation at COLUMN."
  (if (and (numberp column) (>= column 0))
      (make-string column ?\s)
    ""))

(provide 'overlay-usage)
;;; overlay-usage.el ends here
