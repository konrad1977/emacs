;;; -*- lexical-binding: t -*-
;;; Periphery --- A simple package to parse output from compile commands

;;; Commentary: --- A simple package

;;; Code:

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
  '((t (:foreground "#f9e2af" :bold t :background "#2E2A1E" :distant-foreground "#f9e2af" )))
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
  '((t (:foreground "#fbfafb" :font-weight thin)))
  "Message face."
  :group 'periphery)

(defface periphery-fix-face
  '((t (:foreground "#89b4fa")))
  "FIX|FIXME face."
  :group 'periphery)

(defface periphery-fix-face-full
  '((t (:foreground "#1B2431" :bold t :background "#89b4fa" :distant-foreground "#89b4fa")))
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
  '((t (:foreground "#74c7ec")))
  "Performance face."
  :group 'periphery)

(defface periphery-todo-face-full
  '((t (:foreground "#182A32" :bold t :background "#74c7ec" :distant-foreground  "#74c7ec")))
  "Performance face."
  :group 'periphery)

(defface periphery-mark-face-full
  '((t (:foreground "#313244" :background "#9399b2" :distant-foreground "#9399b2" :weight light)))
  "Performance face."
  :group 'periphery)

(defface periphery-first-sentence-face
  '((t (:foreground "#9399b2" :weight normal :height 0.9)))
  "Face for the first sentence of the message (up to the first colon)."
  :group 'periphery)

(defvar DEBUG nil
  "Debug mode.")

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
(defconst periphery-buffer-name "*Periphery*")

(defvar periphery-mode-map nil "Keymap for periphery.")

(defvar default-length 8)

(defconst periphery-regex-parser "\\(\\/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)\\s?\\(\\w+\\):\\(.*\\)\n\\s+\\(.*\\(?:\n\\s+.*\\)*\\)"
  "Parse vimgrep like strings (compilation).")

(defconst mark-inside-parenteses "\\(\(.+?\)\\)"
  "Mark parenteses.")

(defconst mark-strings-regex "\\(\"[^\"]+\"\\)"
  "Mark all string.")

(defconst periphery-regex-mark-quotes "\\('[^']+'\\)"
  "Mark quotes in text.")

(defconst xctest-regex-parser "^\\([^:]+\\):\\([0-9]+\\):\\w?\\([^:]*\\)[^.]+\\.\\([^:|]*\\)\s?:\\(.*\\)"
  "XCTest regex.")

(defconst todos-clean-regex "\\(TODO\\|PERF\\|NOTE\\|FIXME\\|FIX\\|HACK\\|MARK\\)\s?:?\s?\\(.*\\)"
  "Parse Todos and hacks.")

(defconst periphery-parse-search "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).\\(.*\\)")

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

(defun periphery--listing-command (errorList)
  "Create an ERRORLIST for the current mode, prioritizing errors."
  (let ((sorted-list (sort errorList
                           (lambda (a b)
                             (let ((severity-a (aref (car (cdr a)) 0))
                                   (severity-b (aref (car (cdr b)) 0))
                                   (file-a (aref (car (cdr a)) 1))
                                   (file-b (aref (car (cdr b)) 1)))
                               (if (string= severity-a severity-b)
                                   (string< file-a file-b)
                                 (string-prefix-p "error" severity-b)))))))

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
           :attributes 'error))))))

(defun periphery--parse-xctest-putput (line)
  "Run regex for xctest case."
  (save-match-data
    (and (string-match xctest-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (result (match-string 4 line))
                (message (match-string 5 line))
                (fileWithLine (format "%s:%s" file linenumber)))

           (periphery--build-list
            :path fileWithLine
            :file result
            :line linenumber
            :keyword "Failed"
            :result message
            :regex mark-strings-regex)))))

(defun periphery--parse-output-line (line)
  "Run regex over curent LINE."
  (save-match-data
    (and (string-match periphery-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (column (match-string 3 line))
                (type (match-string 4 line))
                (result (match-string 5 line))
                (fileWithLine (format "%s:%s:%s" file linenumber column)))

           (periphery--build-list
            :path fileWithLine
            :file file
            :line linenumber
            :keyword type
            :result result
            :regex periphery-regex-mark-quotes)
           ))))

(defun periphery--propertize-severity (severity text)
  "Colorize TEXT using SEVERITY."
  (if-let* ((type (upcase (string-trim-left severity))))
    (propertize (format " %s " (periphery--center-text type)) 'face (periphery--full-color-from-keyword severity))))

(defun periphery--center-text (word)
  "Center (as WORD)."
  (if (<= (length word) default-length)
      (progn
        (setq padding  (/ (- default-length (string-width word)) 3))
        (setq copy (concat (make-string padding ?\s) word))

        (while (< (string-width copy) (- default-length 1))
          (setq copy (concat copy " ")))
        copy
        )
    word))

(cl-defun periphery--full-color-from-keyword (keyword)
  "Get color from KEYWORD."
  (let ((type (upcase (string-trim-left keyword))))
    (cond
     ((string= type "WARNING") 'periphery-warning-face-full)
     ((string= type "MATCH") 'periphery-warning-face-full)
     ((string= type "INFO") 'periphery-note-face-full)
     ((string= type "ERROR") 'periphery-error-face-full)
     ((string= type "NOTE") 'periphery-note-face-full)
     ((or (string= type "FIX") (string= type "FIXME")) 'periphery-fix-face-full)
     ((or (string= type "PERF") (string= type "PERFORMANCE")) 'periphery-performance-face-full)
     ((string= type "TODO") 'periphery-todo-face-full)
     ((string= type "HACK") 'periphery-hack-face-full)
     (t 'periphery-error-face-full))))

(cl-defun periphery--color-from-keyword (keyword)
  "Get color from KEYWORD."
  (let ((type (upcase (string-trim-left keyword))))
    (cond
     ((string= type "WARNING") 'periphery-warning-face)
     ((string= type "TODO") 'periphery-todo-face)
     ((or (string= type "ERROR") (string= type "HACK")) 'periphery-error-face)
     ((or (string= type "FIX") (string= type "FIXME")) 'periphery-fix-face)
     ((or (string= type "PERF") (string= type "PERFORMANCE")) 'periphery-performance-face)
     (t 'periphery-info-face))))

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

(defun parse-compiler-errors (text)
  "Parse compiler error messages from LOG (as TEXT)."
  (let ((regex "\\(^\/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)\s+\\([^:]+\\):\\(.*\\)\\([^^|\/]*\\)")
        (errors '()))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let* ((path (or (match-string 1) ""))
               (line (or (match-string 2) ""))
               (column (or (match-string 3) ""))
               (error-type (or (match-string 4) ""))
               (msg-part1 (string-trim-left (or (match-string 5) "")))
               (msg-part2 (string-trim-left (or (match-string 6) "")))
               (msg (string-trim (format "%s: %s" msg-part1 msg-part2))))
          (when (and (not (string-empty-p path))
                     (not (string-empty-p line))
                     (not (string-empty-p error-type)))
            (push (periphery--build-list
                   :path (format "%s:%s:%s" path line column)
                   :file path
                   :line line
                   :keyword error-type
                   :result msg
                   :regex periphery-regex-mark-quotes)
                  errors)))))
    (nreverse errors)))

(cl-defun periphery-run-parser (input)
  "Run parser on INPUT more efficiently."
  (when DEBUG
    (message input))

  (let* ((relevant-lines (seq-filter (lambda (line) (string-prefix-p "/" line))
                                     (split-string input "\n" t)))
         (filtered-input (string-join relevant-lines "\n"))
         (errors (parse-compiler-errors filtered-input)))

    (setq periphery-errorList (delete-dups errors))

    (when (or (periphery--is-buffer-visible) periphery-errorList)
      (periphery--listing-command periphery-errorList))))

(defun periphery--is-buffer-visible ()
  "Check if a buffer is visible."
  (let ((buffer (get-buffer periphery-buffer-name)))
    (when buffer
      (let ((window (get-buffer-window buffer)))
        (if window
            t ; Buffer is visible
          nil))))) ; Buffer is not visible

(cl-defun periphery-run-test-parser (input succesCallback)
  (setq periphery-errorList nil)
  (dolist (line (split-string input "\n"))
    (let* ((compilation-entry (periphery--parse-output-line (string-trim-left line)))
           (test-entry (periphery--parse-xctest-putput (string-trim-left line))))
      (when compilation-entry
        (push compilation-entry periphery-errorList))
      (when test-entry
        (push test-entry periphery-errorList))))
  (if periphery-errorList
      (periphery--listing-command (delete-dups periphery-errorList))
    (funcall succesCallback)))

(defun periphery-kill-buffer ()
  "Kill the periphery buffer."
  (interactive)
  (when (get-buffer periphery-buffer-name)
    (kill-buffer periphery-buffer-name)))

(defun periphery-toggle-buffer ()
  "Toggle visibility of the Periphery buffer window."
  (interactive)
  (let ((buffer (get-buffer periphery-buffer-name)))
    (if (not buffer)
        (message "Buffer %s does not exist" periphery-buffer-name)
      (if (get-buffer-window buffer)
          (delete-window (get-buffer-window buffer))
        (display-buffer buffer)))))

;;; - Bartycrouch parsing
(defun periphery--clean-up-comments (text)
  "Cleanup comments from (as TEXT) fixmes and todos."
  (save-match-data
    (and (string-match todos-clean-regex text)
         (if-let* ((keyword (match-string 1 text))
                (comment (match-string 2 text)))
             (list keyword comment)))))

(cl-defun periphery-message-with-count (&key tag &key text &key count &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES with Count."
  (if (not (string= text ""))
      (mode-line-hud:update :message (format "%s %s '%s'" tag count (propertize text 'face attributes)))
    (mode-line-hud:update :message (format "%s %s" tag count ))))

(defun parse--search-query (text query)
  "Parse error and notes (as TEXT) and QUERY."
  (setq default-length 8)
  (setq-local case-fold-search nil) ;; Make regex case sensitive
  (save-match-data
    (and (string-match periphery-parse-search text)
         (let* ((file (match-string 1 text))
                (line (match-string 2 text))
                (column (match-string 3 text))
                (message (match-string 4 text))
                (fileWithLine (format "%s:%s:%s" file line column)))

           (if-let ((todo (periphery--clean-up-comments message)))
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

(cl-defun periphery--build-list (&key path file line keyword result regex)
  "Build list from (as PATH FILE LINE KEYWORD RESULT REGEX)."
  (list path (vector
              (periphery--propertize-severity keyword (string-trim-left keyword))
              (propertize (file-name-sans-extension (file-name-nondirectory file)) 'face 'periphery-filename-face)
              (propertize line 'face 'periphery-linenumber-face)
              (periphery--mark-all-symbols
               :input (periphery--mark-all-symbols
                       :input (periphery--mark-all-symbols
                               :input (periphery--process-message result)
                               :regex mark-strings-regex
                               :property '(face highlight))
                       :regex mark-inside-parenteses
                       :property '(face periphery-warning-face))
               :regex regex
               :property '(face periphery-identifier-face)))))

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
              (periphery--propertize-severity keyword (string-trim-left keyword))
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
                               :regex mark-strings-regex
                               :property '(face highlight))
                       :regex mark-inside-parenteses
                       :property '(face periphery-warning-face))
               :regex regex
               :property '(face periphery-identifier-face)))))

(cl-defun periphery-parse-search-result (&key title &key text &key query)
  "Parse search result (as TITLE TEXT QUERY)."
  (setq default-length 8)
  (setq periphery-errorList '())
  (dolist (line (split-string text "\n"))
    (when-let ((entry (parse--search-query (string-trim-left line) query)))
      (push entry periphery-errorList)))

  (when periphery-errorList
    (progn
      (periphery--listing-command periphery-errorList)
      (if (proper-list-p tabulated-list-entries)
          (periphery-message-with-count
           :tag "Found"
           :text ""
           :count (format "%d" (length periphery-errorList))
           :attributes 'success))
      (switch-to-buffer-other-window periphery-buffer-name))))

;;;###autoload
(defun svg-color-from-tag (tag)
  "Get color from (as TAG)."
  (cond
   ((string-match-p "TODO" tag) 'periphery-todo-face-full)
   ((string-match-p "NOTE\\|enable" tag) 'periphery-note-face-full)
   ((string-match-p "HACK" tag) 'periphery-hack-face-full)
   ((string-match-p "PERF" tag) 'periphery-performance-face-full)
   ((string-match-p "FIXME\\|FIX\\|disable" tag) 'periphery-fix-face-full)
   ((string-match-p "MARK\\|swiftlint" tag) 'periphery-mark-face-full)
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
  '(("\\([;|\/]+\\W+\\w+\\b:.*\\)" . ((lambda (tag)
                                        (svg-tag-make (periphery--remove-leading-keyword tag)
                                                      :face (svg-color-from-tag tag)
                                                      :inverse t))))
    ("\\([;|\/]+\\W+\\w+\\b:\\)" . ((lambda (tag)
                                      (svg-tag-make (periphery--remove-comments tag)
                                                    :face (svg-color-from-tag tag)
                                                    :crop-right t))))
    ("// swiftlint:\\(enable\\|disable\\) .*" . ((lambda (tag)
                                                  (let* ((parts (split-string tag " " t))
                                                         (action (nth 1 parts))
                                                         (text (string-join (cddr parts) " ")))
                                                    (svg-tag-make (concat "SwiftLint: " action)
                                                                  :face (if (string= action "enable")
                                                                            'periphery-note-face-full
                                                                          'periphery-fix-face-full)
                                                                  :inverse t)
                                                    (svg-tag-make text
                                                                  :face 'periphery-mark-face-full
                                                                  :crop-left t)))))))

(provide 'periphery)

;;; periphery.el ends here
