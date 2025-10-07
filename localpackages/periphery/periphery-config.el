;;; periphery-config.el --- Configuration system for periphery parsers -*- lexical-binding: t -*-

;;; Commentary:
;; This file provides a generic configuration system for periphery parsers.
;; It allows easy registration and management of different parser types.

;;; Code:

(require 'cl-lib)

(defgroup periphery-config nil
  "Configuration for periphery parser system."
  :group 'periphery)

;; Face definitions
(defface periphery-filename-face
  '((t (:inherit link)))
  "Filename face."
  :group 'periphery)

(defface periphery-linenumber-face
  '((t (:inherit line-number)))
  "Line number face."
  :group 'periphery)

(defface periphery-warning-face
  '((t (:foreground "#f9e2af")))
  "Warning face."
  :group 'periphery)

(defface periphery-warning-face-full
  '((t (:foreground "#f9e2af" :bold t :background "#2E2A1E" :distant-foreground "#f9e2af")))
  "Warning face."
  :group 'periphery)

(defface periphery-error-face
  '((t (:foreground "#f38ba8")))
  "Error face."
  :group 'periphery)

(defface periphery-error-face-full
  '((t (:foreground "#f38ba8" :bold t :background "#2D1E28" :distant-foreground "#f38ba8")))
  "Error face."
  :group 'periphery)

(defface periphery-identifier-face
  '((t (:inherit periphery-error-face :background "#2D1E28")))
  "Identifier face."
  :group 'periphery)

(defface periphery-message-face
  '((t (:foreground "#fbfafb" :weight thin)))
  "Message face."
  :group 'periphery)

(defface periphery-fix-face
  '((t (:foreground "#89b4fa")))
  "FIX|FIXME face."
  :group 'periphery)

(defface periphery-fix-face-full
  '((t (:foreground "#1B2431" :background "#89b4fa" :distant-foreground "#89b4fa")))
  "FIX with background."
  :group 'periphery)

(defface periphery-note-face
  '((t (:inherit compilation-info)))
  "Info face."
  :group 'periphery)

(defface periphery-note-face-full
  '((t (:foreground "#1E2B2E" :bold t :background "#a6e3a1" :distant-foreground "#a6e3a1")))
  "Info face."
  :group 'periphery)

(defface periphery-info-face
  '((t (:inherit periphery-note-face)))
  "Note face."
  :group 'periphery)

(defface periphery-info-face-full
  '((t (:inherit periphery-note-face-full)))
  "Note face full."
  :group 'periphery)

(defface periphery-performance-face
  '((t (:foreground "#cba6f7")))
  "Performance face."
  :group 'periphery)

(defface periphery-performance-face-full
  '((t (:foreground "#2B1E2E" :bold t :background "#cba6f7" :distant-foreground "#cba6f7" )))
  "Performance face."
  :group 'periphery)

(defface periphery-hack-face-full
  '((t (:foreground "#28181C" :bold t :background "#f38ba8" :distant-foreground  "#f38ba8")))
  "Performance face."
  :group 'periphery)

(defface periphery-todo-face
  '((t (:foreground "#74c7ec" :weight normal)))
  "Performance face."
  :group 'periphery)

(defface periphery-todo-face-full
  '((t (:foreground "#182A32" :background "#74c7ec" :distant-foreground  "#74c7ec" :weight normal)))
  "Performance face."
  :group 'periphery)

(defface periphery-mark-face-full
  '((t (:foreground "#313244" :background "#9399b2" :distant-foreground "#9399b2" :weight light)))
  "Performance face."
  :group 'periphery)

(defface periphery-first-sentence-face
  '((t (:foreground "#9399b2" :weight normal)))
  "Face for the first sentence of the message (up to the first colon)."
  :group 'periphery)

(defcustom periphery-parser-configs nil
  "Alist of parser configurations.
Each entry is (PARSER-ID . PLIST) where PLIST contains:
  :name         - Display name for the parser
  :regex        - Main regex pattern for parsing
  :type         - Parser type (:compiler :search :linter :test)
  :enabled      - Whether parser is enabled
  :priority     - Sort priority (higher = earlier)
  :parse-fn     - Function to parse a line/match
  :face-fn      - Function to determine face from keyword
  :filter-fn    - Optional function to filter/transform input"
  :type '(alist :key-type symbol :value-type plist)
  :group 'periphery-config)

(defvar periphery-registered-parsers (make-hash-table :test 'eq)
  "Hash table of registered parser configurations.")

;;;###autoload
(cl-defun periphery-register-parser (id &key name regex type parse-fn
                                        face-fn filter-fn priority enabled)
  "Register a parser with given ID and configuration.
ID is a unique symbol identifying the parser.
NAME is the display name.
REGEX is the pattern to match.
TYPE is one of :compiler :search :linter :test.
PARSE-FN is a function (LINE) -> ENTRY or nil.
FACE-FN is a function (KEYWORD) -> FACE.
FILTER-FN is optional function (INPUT) -> FILTERED-INPUT.
PRIORITY determines sort order (default 50).
ENABLED determines if parser is active (default t)."
  (let ((config (list :name (or name (symbol-name id))
                      :regex regex
                      :type (or type :compiler)
                      :parse-fn parse-fn
                      :face-fn face-fn
                      :filter-fn filter-fn
                      :priority (or priority 50)
                      :enabled (if (null enabled) t enabled))))
    (puthash id config periphery-registered-parsers)
    config))

;;;###autoload
(defun periphery-get-parser (id)
  "Get parser configuration by ID."
  (gethash id periphery-registered-parsers))

;;;###autoload
(defun periphery-get-parsers-by-type (type)
  "Get all parsers of given TYPE sorted by priority."
  (let (parsers)
    (maphash (lambda (id config)
               (when (and (plist-get config :enabled)
                          (eq (plist-get config :type) type))
                 (push (cons id config) parsers)))
             periphery-registered-parsers)
    (sort parsers (lambda (a b)
                    (> (plist-get (cdr a) :priority)
                       (plist-get (cdr b) :priority))))))

;;;###autoload
(defun periphery-enable-parser (id)
  "Enable parser with given ID."
  (interactive (list (periphery--select-parser "Enable parser: ")))
  (when-let* ((config (gethash id periphery-registered-parsers)))
    (plist-put config :enabled t)
    (puthash id config periphery-registered-parsers)
    (message "Enabled parser: %s" (plist-get config :name))))

;;;###autoload
(defun periphery-disable-parser (id)
  "Disable parser with given ID."
  (interactive (list (periphery--select-parser "Disable parser: ")))
  (when-let* ((config (gethash id periphery-registered-parsers)))
    (plist-put config :enabled nil)
    (puthash id config periphery-registered-parsers)
    (message "Disabled parser: %s" (plist-get config :name))))

(defun periphery--select-parser (prompt)
  "Select a parser interactively with PROMPT."
  (let (parsers)
    (maphash (lambda (id config)
               (push (cons (format "%s [%s]" 
                                   (plist-get config :name)
                                   (if (plist-get config :enabled) "ON" "OFF"))
                           id)
                     parsers))
             periphery-registered-parsers)
    (cdr (assoc (completing-read prompt parsers nil t) parsers))))

;; Syntax highlighting configuration
(defcustom periphery-highlight-patterns
  '((parentheses . "\\(\(.+?\)\\)")
    (strings . "\\(\"[^\"]+\"\\)")
    (quotes . "\\('[^']+'\\)")
    ;; Separate patterns for marks and content
    (quote-marks . "\\('\\)")
    (quote-content . "'\\([^']+\\)'")
    (string-marks . "\\(\"\\)")
    (string-content . "\"\\([^\"]+\\)\""))
  "Regex patterns for syntax highlighting in messages.
Each entry is (ELEMENT . PATTERN) where ELEMENT is the syntax element
and PATTERN is the regex to match it."
  :type '(alist :key-type symbol :value-type string)
  :group 'periphery-config)

(defcustom periphery-syntax-faces
  '((parentheses . periphery-error-face)
    (strings . highlight)
    (quotes . highlight)
    (quote-content . highlight)
    (quote-marks . periphery-identifier-face)
    (string-content . highlight)
    (string-marks . periphery-identifier-face))
  "Face configuration for syntax highlighting in error messages.
Each entry is (ELEMENT . FACE) where ELEMENT is the syntax element
and FACE is the face to apply."
  :type '(alist :key-type symbol :value-type face)
  :group 'periphery-config)

;;;###autoload
(defun periphery-add-highlight-pattern (element pattern &optional face)
  "Add a new highlight pattern for ELEMENT with PATTERN and optional FACE.
ELEMENT is a symbol identifying the syntax element.
PATTERN is the regex pattern to match.
FACE is the face to apply (defaults to periphery-identifier-face)."
  (interactive "SElement name: \nsRegex pattern: ")
  (let ((face (or face 'periphery-identifier-face)))
    ;; Add pattern
    (setf (alist-get element periphery-highlight-patterns) pattern)
    ;; Add face mapping
    (setf (alist-get element periphery-syntax-faces) face)
    (message "Added highlight pattern for %s" element)))

;;;###autoload
(defun periphery-remove-highlight-pattern (element)
  "Remove highlight pattern for ELEMENT."
  (interactive 
   (list (intern (completing-read "Remove pattern: " 
                                  (mapcar #'symbol-name 
                                          (mapcar #'car periphery-highlight-patterns))
                                  nil t))))
  ;; Remove from both alists
  (setq periphery-highlight-patterns 
        (assq-delete-all element periphery-highlight-patterns))
  (setq periphery-syntax-faces 
        (assq-delete-all element periphery-syntax-faces))
  (message "Removed highlight pattern for %s" element))

;;;###autoload
(defun periphery-list-highlight-patterns ()
  "List all configured highlight patterns."
  (interactive)
  (message "Configured highlight patterns:")
  (dolist (pattern periphery-highlight-patterns)
    (let ((element (car pattern))
          (regex (cdr pattern))
          (face (alist-get (car pattern) periphery-syntax-faces)))
      (message "  %s: %s -> %s" element regex face))))

;; Built-in parser configurations  
(defconst periphery-builtin-patterns
  '((compiler . "\\(/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.*\\)")
    (xctest . "^\\([^:]+\\):\\([0-9]+\\):\\w?\\([^:]*\\)[^.]+\\.\\([^:|]*\\)\\s?:\\(.*\\)")
    (search . "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).\\(.*\\)")
    (todos . "\\(TODO\\|PERF\\|NOTE\\|FIXME\\|FIX\\|HACK\\|MARK\\)[[:space:]]*:?[[:space:]]*\\(.*\\)")
    (ktlint . "\\(^[^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\([^(]+\\) (\\(standard:[^)]+\\))")
    (package-error . "^xcodebuild: error: Could not resolve package dependencies:")
    (build-failure . "^The following build commands failed:"))
  "Built-in regex patterns for common parsing scenarios.")

;;;###autoload
(defun periphery-config-initialize-defaults ()
  "Initialize default parser configurations."
  
  ;; Compiler errors/warnings parser
  (periphery-register-parser
   'compiler
   :name "Compiler Errors"
   :regex (alist-get 'compiler periphery-builtin-patterns)
   :type :compiler
   :priority 100
   :parse-fn #'periphery--parse-compiler-line
   :face-fn #'periphery--face-from-severity)
  
  ;; XCTest parser
  (periphery-register-parser
   'xctest
   :name "XCTest Results"
   :regex (alist-get 'xctest periphery-builtin-patterns)
   :type :test
   :priority 90
   :parse-fn #'periphery--parse-xctest-line
   :face-fn #'periphery--face-from-severity)
  
  ;; Search results parser
  (periphery-register-parser
   'search
   :name "Search Results"
   :regex (alist-get 'search periphery-builtin-patterns)
   :type :search
   :priority 80
   :parse-fn #'periphery--parse-search-line
   :face-fn #'periphery--face-from-match-type)
  
  ;; TODO/FIXME parser
  (periphery-register-parser
   'todos
   :name "TODOs and FIXMEs"
   :regex (alist-get 'todos periphery-builtin-patterns)
   :type :search
   :priority 70
   :parse-fn #'periphery--parse-todo-line
   :face-fn #'periphery--face-from-todo-keyword)
  
  ;; Ktlint parser
  (periphery-register-parser
   'ktlint
   :name "Ktlint"
   :regex (alist-get 'ktlint periphery-builtin-patterns)
   :type :linter
   :priority 60
   :parse-fn #'periphery--parse-ktlint-line
   :face-fn #'periphery--face-from-severity))

;; Parser function stubs (to be implemented in periphery-parsers.el)
(defun periphery--parse-compiler-line (line)
  "Parse a compiler error/warning LINE."
  nil) ; Placeholder

(defun periphery--parse-xctest-line (line)
  "Parse an XCTest result LINE."
  nil) ; Placeholder

(defun periphery--parse-search-line (line)
  "Parse a search result LINE."
  nil) ; Placeholder

(defun periphery--parse-todo-line (line)
  "Parse a TODO/FIXME LINE."
  nil) ; Placeholder

(defun periphery--parse-ktlint-line (line)
  "Parse a ktlint warning LINE."
  nil) ; Placeholder

(defun periphery--face-from-severity (severity)
  "Get face for given SEVERITY level."
  (intern (format "periphery-%s-face" (downcase severity))))

(defun periphery--face-from-match-type (type)
  "Get face for given match TYPE."
  'periphery-warning-face)

(defun periphery--face-from-todo-keyword (keyword)
  "Get face for given TODO KEYWORD."
  (intern (format "periphery-%s-face" (downcase keyword))))

(provide 'periphery-config)
;;; periphery-config.el ends here
