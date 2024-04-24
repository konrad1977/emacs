;;; Periphery --- A simple package to parse output from compile commands
;;; -*- lexical-binding: t -*-

;;; Commentary: --- A simple package

;;; Code:
(require 'dash)
(require 'mode-line-hud)
(require 'periphery-helper)

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
  '((t (:foreground "#9399b2" :font-weight thin)))
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

(defface periphery-background-face
  '((t (:inherit default)))
  "Buffer background color."
  :group 'periphery)

(defvar periphery-mode-map nil
  "Keymap for periphery.")

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

(defun periphery--go-to-first-error ()
  "Go to the first error in the periphery-errorList."
  (interactive)
  (when (and periphery-errorList (> (length periphery-errorList) 0))
    (let* ((first-error (car periphery-errorList))
           (file (nth 1 first-error))
           (line (string-to-number (nth 2 first-error)))))
    (message "%s" file)))

(defun periphery--open-current-line ()
  "Open current current line."
  (interactive)
  (open-current-line-with (tabulated-list-get-id)))

(defun periphery--listing-command (errorList)
  "Create an ERRORLIST for the current mode."
  (let ((errors '())
        (warnings '()))
    ;; Separate errors from warnings
    (dolist (entry errorList)
      (let ((severity (aref (car (cdr entry)) 0)))
        (if (string-prefix-p "error" severity)
            (setq errors (cons entry errors))
          (setq warnings (cons entry warnings)))))
    ;; Sort errors and warnings separately and combine them
    (setq errors (sort errors (lambda (a b) (string< (aref (car (cdr a)) 1) (aref (car (cdr b)) 1)))))
    (setq warnings (sort warnings (lambda (a b) (string< (aref (car (cdr a)) 1) (aref (car (cdr b)) 1)))))
    (setq errorList (append errors warnings)))

  (save-selected-window
    (let* ((buffer (get-buffer-create periphery-buffer-name))
           (window (get-buffer-window buffer)))
      (pop-to-buffer buffer nil)
      (periphery-mode)

      (unless (equal (current-buffer) buffer)
        (select-window window))

      (setq tabulated-list-entries (nreverse (-non-nil errorList)))

      (tabulated-list-print t)
      ;; (periphery--go-to-first-error tabulated-list-entries)

      (if (proper-list-p tabulated-list-entries)
          (periphery-message-with-count
           :tag ""
           :text "Errors or warnings"
           :count (format "%d" (length tabulated-list-entries))
           :attributes 'error)))))

(cl-defun periphery--mark-all-symbols (&key input &key regex &key property)
  "Highlight all quoted symbols (as INPUT REGEX PROPERTY)."
  (save-match-data
    (let* ((position 0)
           (normalizedInput (replace-regexp-in-string "’" "'"  input)))
      (while (string-match regex normalizedInput position)
        (let* ((ref (match-string 1 normalizedInput))
               (startPosition (string-match regex normalizedInput position)))
          (setq position (match-end 1))
          (add-text-properties startPosition position property normalizedInput)))
  normalizedInput)))

(defun periphery--parse-xctest-putput (line)
  "Run regex for xctest case."
  (save-match-data
    (and (string-match xctest-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (type (match-string 3 line))
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

(defun periphery--is-list-empty-alt (lst)
  "Alternative method to check if the given list is empty."
  (and (listp lst) (null lst)))
(defun parse-compiler-errors-async (text callback)
  "Parse compiler error messages from LOG (as TEXT) asynchronously and call CALLBACK with the result."
  (async-start
   `(lambda ()
      ,(async-inject-variables "\\`tempList\\'")
      (let ((regex "\\(^/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)\\s+\\([^:]+\\):\\(.*\\)\\([^^|/]*\\)"))
        (while (string-match regex ,text)
          (let* ((path (match-string 1 ,text))
                 (line (match-string 2 ,text))
                 (column (match-string 3 ,text))
                 (error-type (match-string 4 ,text))
                 (msg-part1 (s-trim-left (match-string 5 ,text)))
                 (msg-part2 (s-trim-left (match-string 6 ,text)))
                 (msg (clean-up-newlines (format "%s: %s" msg-part1 msg-part2))))

            (push (periphery--build-list
                   :path (format "%s:%s:%s" path line column)
                   :file path
                   :line line
                   :keyword error-type
                   :result msg
                   :regex periphery-regex-mark-quotes)
                  tempList)
            ;; Update the text to the remaining unmatched portion
            (setq ,text (substring ,text (match-end 6))))))
      tempList)
   callback))

(defun parse-compiler-errors (text)
  "Parse compiler error messages from LOG (as TEXT)."
  (setq tempList nil)
  (let ((regex "\\(^\/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)\s+\\([^:]+\\):\\(.*\\)\\([^^|\/]*\\)"))
    (while (string-match regex text)
      (let* ((path (match-string 1 text))
             (line (match-string 2 text))
             (column (match-string 3 text))
             (error-type (match-string 4 text))
             (msg-part1 (s-trim-left (match-string 5 text)))
             (msg-part2 (s-trim-left (match-string 6 text)))
             (msg (clean-up-newlines (format "%s: %s" msg-part1 msg-part2))))

        (push (periphery--build-list
               :path (format "%s:%s:%s" path line column)
               :file path
               :line line
               :keyword error-type
               :result msg
               :regex periphery-regex-mark-quotes)
              tempList)
      ;; Update the text to the remaining unmatched portion
        (setq text (substring text (match-end 6))))))
  tempList)

(cl-defun periphery-run-parser (input)
  "Run parser (as INPUT as SUCCESSCALLBACK)."
  ;; Filter lines that don't start with "/" only if it doesn't contain "BUILD FAILED" or "BUILD INTERRUPTED"
  ;; (message input)
  (setq input (mapconcat #'identity (seq-filter (lambda (line) (string-match-p "^/" line)) (split-string input "\n")) "\n"))
  (setq periphery-errorList (delete-dups (parse-compiler-errors input)))

  (when (or (periphery--is-buffer-visible) periphery-errorList)
    (periphery--listing-command periphery-errorList)))

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


(cl-defun periphery--build-list (&key path &key file &key line &key keyword &key result &key regex)
  "Build list from (as PATH FILE LINE KEYWORD RESULT REGEX)."
  (list path (vector
              (periphery--propertize-severity keyword (string-trim-left keyword))
              (propertize (file-name-sans-extension (file-name-nondirectory file)) 'face 'periphery-filename-face)
              (propertize line 'face 'periphery-linenumber-face)
              (periphery--mark-all-symbols
               :input (periphery--mark-all-symbols
                       :input (periphery--mark-all-symbols
                               :input (propertize
                                       (string-trim-left result)
                                       'face
                                       'periphery-message-face)
                               :regex mark-strings-regex
                               :property '(face highlight))
                       :regex mark-inside-parenteses
                       :property '(face periphery-warning-face))
               :regex regex
               :property '(face periphery-identifier-face)))))

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

(defun svg-color-from-tag (tag)
  "Get color from (as TAG)."
  (cond
   ((string-match-p "TODO" tag) 'periphery-todo-face-full)
   ((string-match-p "NOTE" tag) 'periphery-note-face-full)
   ((string-match-p "HACK" tag) 'periphery-hack-face-full)
   ((string-match-p "PERF" tag) 'periphery-performance-face-full)
   ((string-match-p "FIXME" tag) 'periphery-fix-face-full)
   ((string-match-p "FIX" tag) 'periphery-fix-face-full)
   ((string-match-p "MARK" tag) 'periphery-mark-face-full)
   (t 'periphery-hack-face-full)))

(defun periphery--remove-leading-keyword (tag)
  "Remove leading keyword and C style -comment from (as TAG)."
  (replace-regexp-in-string "^[;|\/]+\\W?\\w+\\b:\\W?" "" tag))

(defun periphery--remove-comments (tag)
  "Remove comments from (as TAG)."
  (string-trim-left
    (replace-regexp-in-string ":" ""
                              (replace-regexp-in-string "^[;|\/]+\\W?" "" tag))))

;;; TODO: Add support for swiftlint:disable and swiftlint:enable
;;;###autoload
(defun periphery-svg-tags ()
  "Get svg tags."
  '(
    ("\\([;|\/]+\\W?\\w+\\b:.*\\)" . ((lambda (tag)
                                    (svg-tag-make (periphery--remove-leading-keyword tag)
                                                  :face (svg-color-from-tag tag)
                                                  :inverse t
                                                  ))))

    ("\\([;|\/]+\\W?\\w+\\b:\\)" . ((lambda (tag) (svg-tag-make
                                                  (periphery--remove-comments tag)
                                                   :face (svg-color-from-tag tag)
                                                   :crop-right t))))))


(provide 'periphery)

;;; periphery.el ends here
