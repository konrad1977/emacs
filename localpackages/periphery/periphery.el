;;; -*- lexical-binding: t -*-
;;; Periphery --- A simple package to parse output from compile commands

;;; Commentary: --- A simple package

;;; Code:

(require 'periphery-core)
(require 'periphery-config)
(require 'periphery-parsers)

;; Initialize parsers when loading periphery
(periphery-parsers-initialize)

(defconst periphery-buffer-name "*Periphery*")

(defcustom periphery-debug nil
  "Enable debug mode periphery."
  :type 'boolean
  :group 'periphery)

(defvar default-length 8)

(defvar periphery-mode-map nil
  "Keymap for periphery.")

(defcustom periphery-trim-message-prefix nil
  "When non-nil, trim the message prefix (up to the first colon) in error messages."
  :type 'boolean
  :group 'periphery)

(setq periphery-mode-map (make-sparse-keymap))
(define-key periphery-mode-map (kbd "RET") #'periphery--open-current-line)
(define-key periphery-mode-map (kbd "<return>") 'periphery--open-current-line)
(define-key periphery-mode-map (kbd "o") 'periphery--open-current-line)

(defvar periphery-mode-map nil "Keymap for periphery.")

(defvar-local periphery-errorList '())

(define-derived-mode periphery-mode tabulated-list-mode "Periphery-mode"
  "Periphery mode.  A mode to show compile errors like Flycheck."
  (setq tabulated-list-format [
                               ("Type" 9 nil)
                               ("File" 28 t)
                               ("Line" 4 nil)
                               ("Message" 120 nil)
                               ]
        tabulated-list-padding 1
        tabulated-list-sort-key (cons "Line" nil))
  (use-local-map periphery-mode-map)
  (tabulated-list-init-header))

(defun periphery--open-current-line ()
  "Open current current line."
  (interactive)
  (open-current-line-with (tabulated-list-get-id)))

(defun periphery--severity-priority (severity)
  "Return numeric priority for SEVERITY (lower number = higher priority)."
  (if (not (stringp severity))
      4  ; Default priority for non-strings
    (let ((type (downcase (string-trim severity))))
      (cond
       ((string-prefix-p "error" type) 1)
       ((string-prefix-p "warning" type) 2)
       ((string-prefix-p "note" type) 3)
       (t 4)))))

(defun periphery--listing-command (errorList)
  "Create an ERRORLIST for the current mode, prioritizing errors."
  (let ((sorted-list (sort errorList
                           (lambda (a b)
                             (let* ((entry-a (cadr a))
                                    (entry-b (cadr b))
                                    (severity-a (aref entry-a 0))
                                    (severity-b (aref entry-b 0))
                                    (file-a (aref entry-a 1))
                                    (file-b (aref entry-b 1))
                                    (priority-a (periphery--severity-priority severity-a))
                                    (priority-b (periphery--severity-priority severity-b)))
                               (if (= priority-a priority-b)
                                   (string< file-a file-b)
                                 (< priority-a priority-b)))))))

    (save-selected-window
      (let* ((buffer (get-buffer-create periphery-buffer-name))
             (window (get-buffer-window buffer)))
        (pop-to-buffer buffer nil)
        (periphery-mode)

        (unless (equal (current-buffer) buffer)
          (select-window window))

        (setq tabulated-list-entries (-non-nil sorted-list))

        (tabulated-list-print t)

        (if (proper-list-p tabulated-list-entries)
            (periphery-message-with-count
             :tag ""
             :text "Errors or warnings"
             :count (format "%d" (length tabulated-list-entries))
             :attributes 'periphery-todo-face))

        ;; Auto-open first error (only if it's actually an error, not a warning)
        (run-at-time 0.2 nil
                     (lambda ()
                       (with-current-buffer (get-buffer periphery-buffer-name)
                         (goto-char (point-min))
                         (when-let* ((entry (tabulated-list-get-entry))
                                     (severity (aref entry 0)))
                           (when (string-match-p "error" (downcase severity))
                             (periphery--open-current-line))))))))))



(defun periphery--propertize-severity (severity)
  "Colorize TEXT using SEVERITY."
  (if-let* ((type (upcase (string-trim-left severity))))
      (let ((display-type (if (> (string-width type) 8)
                              "ERROR"
                            type)))
        (propertize (format " %s " (periphery--center-text display-type))
                    'face (periphery--full-color-from-keyword severity)))))

(defun periphery--center-text (word)
  "Center WORD to default length."
  (let* ((word-len (string-width word))
         (padding (/ (- default-length word-len) 3))
         (result (concat (make-string padding ?\s) word)))
    (while (< (string-width result) (- default-length 1))
      (setq result (concat result " ")))
    result))

(defun periphery--color-from-keyword (keyword)
  "Get color face from KEYWORD."
  (let ((type (upcase (string-trim-left keyword))))
    (pcase type
      ((or "WARNING" "MATCH") 'periphery-warning-face)
      ("INFO" 'periphery-note-face)
      ("ERROR" 'periphery-error-face)
      ("NOTE" 'periphery-note-face)
      ((or "FIX" "FIXME") 'periphery-fix-face)
      ((or "PERF" "PERFORMANCE") 'periphery-performance-face)
      ("TODO" 'periphery-todo-face)
      ("HACK" 'periphery-hack-face)
      (_ 'periphery-error-face))))

(defun periphery--full-color-from-keyword (keyword)
  "Get full color face from KEYWORD."
  (let ((type (upcase (string-trim-left keyword))))
    (pcase type
      ((or "WARNING" "MATCH") 'periphery-warning-face-full)
      ("INFO" 'periphery-note-face-full)
      ("ERROR" 'periphery-error-face-full)
      ("NOTE" 'periphery-note-face-full)
      ((or "FIX" "FIXME") 'periphery-fix-face-full)
      ((or "PERF" "PERFORMANCE") 'periphery-performance-face-full)
      ("TODO" 'periphery-todo-face-full)
      ("HACK" 'periphery-hack-face-full)
      (_ 'periphery-error-face-full))))

(cl-defun periphery--mark-all-symbols (&key input regex property)
  "Highlight all quoted symbols (as INPUT REGEX PROPERTY)."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        (when (< match-start match-end)  ; Ensure non-zero-width match
          (add-text-properties match-start match-end property)
          (goto-char match-end))))  ; Move point past the match
    (buffer-string)))

(defun parse-missing-package-product (buffer)
  "Parse missing package product errors from the current BUFFER."
  (let ((regex-missing-product ": error: Missing package product '\\([^']+\\)' (in target '\\([^']+\\)' from project '\\([^']+\\)')")
        (errors '()))
    (goto-char (point-min))
    (while (re-search-forward regex-missing-product nil t)
      (let* ((product (match-string 1))
             (target (match-string 2))
             (project (match-string 3))
             (failure-msg (format "Missing package product '%s' in target '%s' from project '%s'"
                                product target project)))
        (push (periphery--build-list
               :path (format "%s/%s" project target)
               :file "Package.swift"
               :line "1"
               :keyword "error"
               :result failure-msg
               :regex (alist-get 'quotes periphery-regex-patterns))
              errors)))
    errors))

(defun parse-package-dependency-errors (buffer)
  "Parse package dependency errors from the current BUFFER."
  (let ((regex-package-error "^xcodebuild: error: Could not resolve package dependencies:")
        (errors '()))
    (goto-char (point-min))
    (when (re-search-forward regex-package-error nil t)
      (let ((failure-msg (buffer-substring-no-properties (point) (pos-eol))))
        (forward-line)
        ;; Collect all related package error messages
        (while (and (not (eobp))
                    (or (looking-at "^  ") (looking-at "^$")))
          (unless (looking-at "^$") ; Skip empty lines
            (setq failure-msg (concat failure-msg "\n"
                                    (buffer-substring-no-properties (point) (pos-eol)))))
          (forward-line))
        (push (periphery--build-list
               :path "Package Dependencies"
               :file "Package.swift"
               :line "1"
               :keyword "error"
               :result failure-msg
               :regex (alist-get 'quotes periphery-regex-patterns))
              errors)))
    errors))

(defun parse-build-failures (buffer)
  "Parse build failures from the current BUFFER."
  (let ((regex-build-failure "^The following build commands failed:")
        (errors '()))
    (goto-char (point-min))
    (when (re-search-forward regex-build-failure nil t)
      (let ((failure-msg (buffer-substring-no-properties (point) (pos-eol))))
        (forward-line)
        (while (and (not (eobp)) (looking-at "\t"))
          (setq failure-msg (concat failure-msg "\n"
                                    (buffer-substring-no-properties (point) (pos-eol))))
          (forward-line))
        ;; Shorten the path in the failure message
        (setq failure-msg
              (replace-regexp-in-string
               "\\(/Users/[^/]+/.*?/\\)\\([^/]+/[^/]+\\.build/.*\\)"
               "\\2"
               failure-msg))
        (push (periphery--build-list
               :path "_Build Failure"
               :file "_Build Failure"
               :line "999"
               :keyword "error"
               :result failure-msg
               :regex (alist-get 'quotes periphery-regex-patterns))
              errors)))
    errors))

(cl-defun periphery-run-parser (input &rest config)
  "Run parser on INPUT with dynamic CONFIG. Return t if errors found.
CONFIG can be:
- :config PARSERS - Use specific parser list (e.g., '(compiler ktlint))
- :todo - Parse for TODO/FIXME/HACK comments  
- :ktlint - Parse ktlint output
- :compiler - Parse compiler output (default)
- :search QUERY - Parse search results with optional query highlighting
- :linter - Parse generic linter output
- :test - Parse test output"
  (when periphery-debug
    (message "periphery-run-parser called with %d chars of input and config: %S" 
             (length input) config))
  
  (let ((type :compiler)
        (parsers nil)
        (query nil))
    
    ;; Process configuration parameters
    (while config
      (let ((key (pop config)))
        (cond
         ((eq key :config)
          (setq parsers (pop config)))
         ((eq key :todo)
          (setq type :search)  ; TODOs are found via search
          (setq parsers '(search))) ; Use search parser to detect TODO patterns
         ((eq key :ktlint)
          (setq type :linter)
          (setq parsers '(ktlint)))
         ((eq key :compiler)
          (setq type :compiler)
          (setq parsers '(compiler)))
         ((eq key :search)
          (setq type :search)
          (setq parsers '(search))
          (when config (setq query (pop config))))
         ((eq key :linter)
          (setq type :linter))
         ((eq key :test)
          (setq type :test)
          (setq parsers '(xctest)))
         (t
          (message "Unknown periphery-run-parser config: %s" key)))))
    
    ;; Use the core parsing system with dynamic configuration
    (let ((errors (periphery-core-parse
                   :input input
                   :type type
                   :parsers parsers
                   :query query)))
      (setq periphery-errorList errors)
      (when (or (periphery--is-buffer-visible) periphery-errorList)
        (periphery--listing-command periphery-errorList))
      (not (null periphery-errorList))))) ; Return t if any errors found

(defun periphery-sort-error (errors)
  "Sort ERRORS by severity (Error, Warning, Note) and then by path."
  (sort (nreverse errors)
        (lambda (a b)
          (let* ((entry-a (cadr a))
                 (entry-b (cadr b))
                 (priority-a (periphery--severity-priority (aref entry-a 0)))
                 (priority-b (periphery--severity-priority (aref entry-b 0))))
            ;; First compare by severity priority
            (if (= priority-a priority-b)
                ;; If same severity, sort by path
                (string< (car a) (car b))
              ;; Otherwise sort by severity (lower priority number comes first)
              (< priority-a priority-b))))))

(defun periphery--is-buffer-visible ()
  "Check if periphery buffer is visible."
  (when-let* ((buffer (get-buffer periphery-buffer-name)))
    (get-buffer-window buffer)))

(defun periphery-kill-buffer ()
  "Kill the periphery buffer."
  (interactive)
  (when-let* ((buffer (get-buffer periphery-buffer-name)))
    (kill-buffer buffer)))

(defun periphery:toggle-buffer ()
  "Toggle visibility of the Periphery buffer window."
  (interactive)
  (if-let* ((buffer (get-buffer periphery-buffer-name)))
      (if (get-buffer-window buffer)
          (delete-window (get-buffer-window buffer))
        (display-buffer buffer))
    (message "Buffer %s does not exist" periphery-buffer-name)))

(defun periphery-toggle-debug ()
  "Toggle the periphery debug mode on or off."
  (interactive)
  (setq periphery-debug (not periphery-debug))
  (message "Periphery debug mode %s" (if periphery-debug "enabled" "disabled")))

(cl-defun periphery-run-test-parser (input succesCallback)
  "Parse test input using dynamic parser system."
  (if (periphery-run-parser input :test)
      (periphery--listing-command (delete-dups periphery-errorList))
    (funcall succesCallback)))

;;; - Bartycrouch parsing
(defun periphery--clean-up-comments (text)
  "Cleanup comments from (as TEXT) fixmes and todos."
  (save-match-data
    (and (string-match (alist-get 'todos periphery-regex-patterns) text)
         (if-let* ((keyword (match-string 1 text))
                (comment (match-string 2 text)))
             (list keyword comment)))))

(cl-defun periphery-message-with-count (&key tag &key text &key count &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES with Count."
  (if (not (string= text ""))
      (mode-line-hud:update :message (format "%s %s '%s'" tag  (propertize count 'face 'periphery-error-face)  (propertize text 'face attributes)))
    (mode-line-hud:update :message (format "%s %s" tag count ))))

(defun parse--search-query (text query)
  "Parse error and notes (as TEXT) and QUERY."
  (setq default-length 8)
  (setq-local case-fold-search nil) ;; Make regex case sensitive
  (save-match-data
    (and (string-match (alist-get 'search periphery-regex-patterns) text)
         (let* ((file (match-string 1 text))
                (line (match-string 2 text))
                (column (match-string 3 text))
                (message (string-trim-left (match-string 4 text)))
                (fileWithLine (format "%s:%s:%s" file line column)))

           (if-let* ((todo (periphery--clean-up-comments message)))
                 (periphery--build-todo-list
                  :path fileWithLine
                  :file file
                  :line line
                  :keyword (nth 0 todo)
                  :result (nth 1 todo)
                  :regex (format "\\(%s\\)" query))

             (periphery--build-list
              :path fileWithLine
              :file file
              :line line
              :keyword "Match"
              :result message
              :regex (format "\\(%s\\)" query)))))))

;; Delegate to new core building function
(cl-defun periphery--build-list (&key path file line keyword result regex)
  "Build list from (as PATH FILE LINE KEYWORD RESULT REGEX)."
  (periphery-core-build-entry
   :path path
   :file file
   :line line
   :severity keyword
   :message (periphery--mark-all-symbols
             :input (periphery--mark-all-symbols
                     :input (periphery--mark-all-symbols
                             :input (periphery--process-message result)
                             :regex (alist-get 'strings periphery-highlight-patterns)
                             :property '(face highlight))
                     :regex (alist-get 'parentheses periphery-highlight-patterns)
                     :property '(face periphery-warning-face))
             :regex regex
             :property '(face periphery-identifier-face))
   :face-fn #'periphery--full-color-from-keyword))

(defun periphery--process-message (message)
  "Process MESSAGE, optionally trimming the prefix and applying face properties."
  (let* ((trimmed-message (if periphery-trim-message-prefix
                              (periphery--trim-message-prefix message)
                            message))
         (propertized-message (propertize trimmed-message 'face 'periphery-message-face)))
    (if periphery-trim-message-prefix
        propertized-message
      (periphery--highlight-first-sentence propertized-message))))

(defun periphery--trim-message-prefix (message)
  "Trim the prefix of MESSAGE up to the first colon, if there's text after it."
  (if-let* ((colon-pos (string-match ":" message))
            (rest (string-trim (substring message (1+ colon-pos))))
            ((not (string-empty-p rest))))
      rest
    message))

(defun periphery--highlight-first-sentence (text)
  "Highlight the first sentence (up to the first colon) in TEXT."
  (let ((colon-pos (string-match ":" text)))
    (if colon-pos
        (concat
         (propertize (substring text 0 (1+ colon-pos))
                     'face 'periphery-first-sentence-face)
         (substring text (1+ colon-pos)))
      text)))

(cl-defun periphery--build-todo-list (&key path &key file &key line &key keyword &key result &key regex)
  "Build list from (as PATH FILE LINE KEYWORD RESULT REGEX)."
  (list path (vector
              (periphery--propertize-severity keyword)
              (propertize (file-name-sans-extension (file-name-nondirectory file)) 'face 'periphery-filename-face)
              (propertize line 'face 'periphery-linenumber-face)
              (periphery--mark-all-symbols
               :input (periphery--mark-all-symbols
                       :input (periphery--mark-all-symbols
                               :input
                                (propertize
                                 result
                                 'face
                                  (periphery--color-from-keyword keyword))
                               :regex (alist-get 'strings periphery-regex-patterns)
                               :property '(face highlight))
                       :regex (alist-get 'parentheses periphery-regex-patterns)
                       :property '(face periphery-warning-face))
               :regex regex
               :property '(face periphery-identifier-face)))))

(cl-defun periphery-parse-search-result (&key text query)
  "Parse search result (as TITLE TEXT QUERY)."
  (setq default-length 8)
  (if (string-empty-p text)
      (periphery-message-with-count
       :tag "No matches found"
       :text ""
       :count "0"
       :attributes 'periphery-error-face)
    ;; Use new dynamic parser system
    (let ((found-errors (periphery-run-parser text :search query)))
      (when found-errors
        (progn
          (if (proper-list-p tabulated-list-entries)
              (periphery-message-with-count
               :tag "Found"
               :text ""
               :count (format "%d" (length periphery-errorList))
               :attributes 'success))
          (switch-to-buffer-other-window periphery-buffer-name))))))

;;;###autoload
(defun periphery-parse-ktlint-result (input)
  "Parse Klint result."
  (periphery-run-parser input :ktlint))

;;;###autoload
(defun svg-color-from-tag (tag)
  "Get color from (as TAG)."
  (cond
   ((string-match-p "TODO" tag) 'periphery-todo-face-full)
   ((string-match-p "NOTE:" tag) 'periphery-note-face-full)
   ((string-match-p "HACK" tag) 'periphery-hack-face-full)
   ((string-match-p "PERF" tag) 'periphery-performance-face-full)
   ((string-match-p "FIXME\\|FIX" tag) 'periphery-fix-face-full)
   ((string-match-p "MARK" tag) 'periphery-mark-face-full)
   (t 'periphery-hack-face-full)))

;;;###autoload
(defun periphery--remove-leading-keyword (tag)
  "Remove leading keyword and C style -comment from (as TAG)."
  (replace-regexp-in-string "^[;|\/]+\\W+\\w+\\b:\\W?" "" tag))

;;;###autoload
(defun periphery--remove-comments (tag)
  "Remove comments from (as TAG)."
  (string-trim-left
   (replace-regexp-in-string ":" ""
                             (replace-regexp-in-string "^[;|\/]+\\W+" "" tag))))

;;;###autoload
(defun periphery-svg-tags ()
  "Get svg tags."
  '(
    ("^[ \t]*\\([;|\/][;|\/]?\\W+\\w+\\b:.*\\)" . ((lambda (tag)
                                                     (svg-tag-make (periphery--remove-leading-keyword (concat tag "\t"))
                                                                   :face (svg-color-from-tag tag)
                                                                   :inverse t))))
    ("^[ \t]*\\([;|\/][;|\/]?\\W+\\w+\\b:\\)" . ((lambda (tag)
                                                   (svg-tag-make (periphery--remove-comments tag)
                                                                 :face (svg-color-from-tag tag)
                                                                 :crop-right t))))

    ("^[ \t]*//\\S+\\(swiftlint:[^\s].*\\)$" . ((lambda (tag)
                                                          (let* ((parts (split-string tag ":" t))
                                                                 (action (nth 1 parts))
                                                                 (text (string-join (cddr parts) " "))
                                                                 (face (if (string= action "enable")
                                                                      'periphery-hack-face-full
                                                                    'periphery-fix-face-full)))
                                                            (svg-tag-make text :face face :crop-left t)
                                                            (svg-tag-make action :face face :inverse t)
                                                            ))))))
(provide 'periphery)
;;; periphery.el ends here
