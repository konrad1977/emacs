;;; Periphery --- A simple package to parse output from compile commands
;;; -*- lexical-binding: t -*-

;;; Commentary: --- A simple package

;;; Code:
(require 'dash)
(require 'transient)
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
  '((t (:foreground "#f9e2af" :background "#2E2A1E" :bold t :distant-foreground "#f9e2af" )))
  "Warning face."
  :group 'periphery)

(defface periphery-error-face
  '((t (:foreground "#f38ba8" :bold t)))
  "Error face."
  :group 'periphery)

(defface periphery-error-face-full
  '((t (:foreground "#f38ba8" :bold t :background "#2D1E28" :distant-foreground "#f38ba8")))
  "Error face."
  :group 'periphery)

(defface periphery-identifier-face
  '((t (:inherit periphery-error-face :weight semi-bold :background "#2D1E28")))
  "Identifier face."
  :group 'periphery)

(defface periphery-message-face
  '((t (:inherit font-lock-comment-face)))
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
  '((t (:inherit compilation-info :bold nil)))
  "Info face."
  :group 'periphery)

(defface periphery-note-face-full
  '((t (:foreground "#a6e3a1" :bold t :background "#1E2B2E" :distant-foreground "#a6e3a1")))
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
  '((t (:foreground "#252538" :bold t :background "#7373B5" :distant-foreground  "#575787")))
  "Performance face."
  :group 'periphery)

(defface periphery-background-face
  '((t (:inherit default)))
  "Buffer background color."
  :group 'periphery)

(defun periphery--buffer-setup-background ()
  (setq buffer-face-mode-face 'periphery-background-face)
  (buffer-face-mode 1))

(defconst periphery-buffer-name "*Periphery*")

(defvar periphery-mode-map nil "Keymap for periphery.")

(defconst default-length 9)

(defconst periphery-regex-parser "\\(\/[^:]+\\):\\([0-9]+\\)?:\\([0-9]+\\)?:?\w?\\([^:]+\\).\\(.[<>=:+-_,a-zA-Z0-9\(\)\?\\\s\'\"\.\&\|]*\\)"
  "Parse vimgrep like strings (compilation).")

(defconst periphery-parse-line-regex "^\\([^:]+\\):\\([0-9]+\\)?:\\(\\([0-9]+\\)\\)?"
   "Parse linenumber and columns.")

(defconst periphery-remove-unicode-regex "[^\x00-\x7F]+"
  "Remove unicode-characters.")

(defconst periphery-note-and-errors-regex "\\(^[^\s:]+\\):\s\\(.*\\)$"
  "When we fail because of other errors than compilation errors.")

(defconst mark-inside-parenteses "\\(\(.+?\)\\)"
  "Mark parenteses.")

(defconst mark-strings-regex "\\(\"[^\"]+\"\\)"
  "Mark all string.")

(defconst periphery-regex-mark-quotes "\\('[^']+'\\)"
  "Mark quotes in text.")

(defconst bartycrouch-regex-parser "\\(\/+[^:]+\\):\\([0-9]+\\):[^:]+.\s[^:]+.\s+\\([^']+\\)\\('[^']+'\\)\\([^:]+:\\)\s\\(\[[0-9]+\]\\)"
  "Parse bartycrouch regex.")

(defconst todos-clean-regex "\\(TODO\\|PERF\\|NOTE\\|FIXME\\|FIX\\|HACK\\|MARK\\)\s?:?\s?\\(.*\\)"
  "Parse Todos and hacks.")

(defconst periphery-parse-search "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).\\(.*\\)")

(defvar periphery-errorList '())
(defvar periphery-directory-root nil "DirectoryRoot for localizeable.")

(define-derived-mode periphery-mode tabulated-list-mode "Periphery-mode"
  "Periphery mode.  A mode to show compile errors like Flycheck."
  (setq tabulated-list-format [("File" 32 t)("Line" 5 nil)("Type" 9 nil)("Message" 100 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Line" nil))
  (use-local-map periphery-mode-map)
  (tabulated-list-init-header))

(defun periphery--get-all-errors (list)
  "Get all error entries from LIST."
  (--filter (string-match-p (regexp-quote "error") (aref (car (cdr it)) 2)) list))

(defun periphery--go-to-first-error (list)
  "Go to first error in LIST."
  (defvar errors-with-reference (--filter (> (length (aref (car(cdr it)) 1)) 0) (periphery--get-all-errors list)))
  (periphery--open-current-line-with (car (car errors-with-reference))))

(defun periphery--open-current-line-with (data)
  "Open current line with DATA."
  (if data
      (save-match-data
        (let* ((matched (string-match periphery-parse-line-regex data))
               (file (match-string 1 data))
               (linenumber (string-to-number (match-string 2 data)))
               (column (match-string 3 data)))
          (with-current-buffer (find-file file)
            (when (> linenumber 0)
              (goto-char (point-min))
              (forward-line (1- linenumber))
              (if column
                  (let ((columnnumber (string-to-number column)))
                    (when (> columnnumber 0)
                      (forward-char (1- columnnumber)))))))))))


(defun periphery--open-current-line ()
  "Open current current line."
  (interactive)
  (periphery--open-current-line-with (tabulated-list-get-id)))

(defun periphery--listing-command (errorList)
  "Create a ERRORLIST for current mode."
  (save-selected-window
    (let* ((buffer (get-buffer-create periphery-buffer-name))
           (window (get-buffer-window buffer)))
      (pop-to-buffer buffer nil)
      (periphery-mode)
      (periphery--buffer-setup-background)

      (unless (equal (current-buffer) buffer)
        (select-window window))

      (setq tabulated-list-entries (nreverse (-non-nil errorList)))
      (periphery--go-to-first-error tabulated-list-entries)
      (tabulated-list-print t)
      
      (if (proper-list-p tabulated-list-entries)
          (periphery-message-with-count
           :tag "[Done]"
           :text "Contains errors or warnings."
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
            :regex periphery-regex-mark-quotes)))))

(defun propertize-message (text)
  "Colorize TEXT based on type."
  (cond
   ((string-match-p (regexp-quote "Function") text)
    (propertize text 'face 'font-lock-function-name-face))
   ((string-match-p (regexp-quote "Class") text)
    (propertize text 'face 'font-lock-keyword-face))
   ((string-match-p (regexp-quote "Enum") text)
    (propertize text 'face 'font-lock-keyword-face))
   ((string-match-p (regexp-quote "Struct") text)
    (propertize text 'face 'font-lock-keyword-face))
   ((string-match-p (regexp-quote "Parameter") text)
    (propertize text 'face 'font-lock-type-face))
   ((string-match-p (regexp-quote "Property") text)
    (propertize text 'face 'font-lock-variable-name-face))
   ((string-match-p (regexp-quote "Initializer") text)
    (propertize text 'face 'font-lock-constant-face))
   ((string-match-p (regexp-quote "Protocol") text)
    (propertize text 'face 'font-lock-builtin-face))
  (t (propertize text 'face 'periphery-message-face))))
  

(defun periphery--propertize-severity (severity text)
  "Colorize TEXT using SEVERITY."
  (let ((type (upcase (string-trim-left severity))))
    (propertize (format " %s " (periphery--center-text type)) 'face (periphery--full-color-from-keyword severity))))

(defun periphery--center-text (word)
  "Center (as WORD)."
  (setq padding  (/ (- default-length (string-width word)) 2))
  (setq copy (concat (make-string padding ?\s) word))
  
  (while (< (string-width copy) default-length)
    (setq copy (concat copy " ")))
  copy)

(cl-defun periphery--full-color-from-keyword (keyword)
  "Get color from KEYWORD."
  (let ((type (upcase (string-trim-left keyword))))
    (cond
     ((string= type "WARNING") 'periphery-warning-face-full)
     ((string= type "ERROR") 'periphery-error-face-full)
     ((string= type "NOTE") 'periphery-note-face-full)
     ((or (string= type "FIX") (string= type "FIXME")) 'periphery-fix-face-full)
     ((or (string= type "PERF") (string= type "PERFORMANCE")) 'periphery-performance-face-full)
     ((string= type "TODO") 'periphery-todo-face-full)
     ((string= type "HACK") 'periphery-hack-face-full)
     (t 'periphery-info-face-full))))

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

;;;###autoload
(cl-defun periphery-run-parser (input &optional succesCallback)
  "Run parser (as INPUT, optional SUCCESSCALLBACK)."
  (setq periphery-errorList nil)
  (dolist (line (split-string input "\n"))
    (let* ((entry (periphery--parse-output-line (string-trim-left (replace-regexp-in-string periphery-remove-unicode-regex "" line))))
          (secondEntry (parse-xcodebuild-notes-and-errors (replace-regexp-in-string periphery-remove-unicode-regex "" line))))
      (if entry
          (push entry periphery-errorList))
      (unless entry (and secondEntry
          (push secondEntry periphery-errorList))
      )))
  (if periphery-errorList
      (periphery--listing-command periphery-errorList)
    (progn
      (periphery-kill-buffer)
      (setq periphery-errorList '())
      (funcall succesCallback)
      ;; (message-with-color :tag "[Complete]" :text "No errors or warnings found" :attributes '(:inherit success))
      )))

(defun periphery-mode-all ()
  "Show all."
  (interactive)
  (progn
    (setq tabulated-list-entries (-non-nil periphery-errorList))
    (tabulated-list-print t)))

(defun periphery-mode-build-filter (filter index)
  "Show only FILTER and INDEX."
  (interactive "P")
  (progn
    (setq tabulated-list-entries
          (--filter
           (string-match-p filter
                           (aref (car( cdr it)) index)) (-non-nil periphery-errorList)))

      (tabulated-list-print t)
      (if (proper-list-p tabulated-list-entries)
          (periphery-message-with-count
           :tag "[Matching]"
           :text filter
           :count (format "%d" (length tabulated-list-entries))
           :attributes 'success))))

(defvar periphery-mode-map nil
  "Keymap for periphery.")

(setq periphery-mode-map (make-sparse-keymap))
(define-key periphery-mode-map (kbd "a") 'periphery-mode-all)
(define-key periphery-mode-map (kbd "e") #'(lambda () (interactive) (periphery-mode-build-filter "error" 2)))
(define-key periphery-mode-map (kbd "t") #'(lambda () (interactive) (periphery-mode-build-filter "todo" 2)))
(define-key periphery-mode-map (kbd "h") #'(lambda () (interactive) (periphery-mode-build-filter "hack" 2)))
(define-key periphery-mode-map (kbd "n") #'(lambda () (interactive) (periphery-mode-build-filter "note" 2)))
(define-key periphery-mode-map (kbd "w") #'(lambda () (interactive) (periphery-mode-build-filter "warning" 2)))
(define-key periphery-mode-map (kbd "f") #'(lambda () (interactive) (periphery-mode-build-filter "Function\\|fix" 3)))
(define-key periphery-mode-map (kbd "u") #'(lambda () (interactive) (periphery-mode-build-filter "Unused" 3)))
(define-key periphery-mode-map (kbd "i") #'(lambda () (interactive) (periphery-mode-build-filter "Initializer" 3)))
(define-key periphery-mode-map (kbd "I") #'(lambda () (interactive) (periphery-mode-build-filter "Protocol" 3)))
(define-key periphery-mode-map (kbd "P") #'(lambda () (interactive) (periphery-mode-build-filter "Parameter" 3)))
(define-key periphery-mode-map (kbd "p") #'(lambda () (interactive) (periphery-mode-build-filter "Property\\|perf" 2)))
(define-key periphery-mode-map (kbd "RET") #'periphery--open-current-line)
(define-key periphery-mode-map (kbd "<return>") 'periphery--open-current-line)
(define-key periphery-mode-map (kbd "o") 'periphery--open-current-line)

;;;###autoload
(defun periphery-kill-buffer ()
  "Kill the periphery buffer."
  (interactive)
  (when (get-buffer periphery-buffer-name)
    (kill-buffer periphery-buffer-name)))

;;;###autoload
(defun periphery-show-buffer ()
  "Show current periphery buffer."
  (interactive)
  (periphery--listing-command periphery-errorList))

;;; - Bartycrouch parsing
(defun periphery--clean-up-comments (text)
  "Cleanup comments from (as TEXT) fixmes and todos."
  (save-match-data
    (and (string-match todos-clean-regex text)
         (if-let* ((keyword (match-string 1 text))
                (comment (match-string 2 text)))
             (list keyword comment)))))

(defun periphery--parse-bartycrouch-line (line)
  "Run regex over curent LINE."
  (save-match-data
    (and (string-match bartycrouch-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (message (match-string 3 line))
                (failingAttribute (match-string 4 line))
                (messageRest (match-string 5 line))
                (otherEntries (match-string 6 line))
                (fileWithLine (format "%s:%s:%s" file linenumber "0")))
           
             (list fileWithLine (vector
                                 (propertize
                                  (format "%s/%s"
                                          (file-name-sans-extension (file-name-nondirectory (directory-file-name (file-name-directory file))))
                                          (file-name-nondirectory file)
                                          ) 'face 'periphery-filename-face)
                                 (propertize linenumber 'face 'periphery-linenumber-face)
                                 (propertize "warning" 'face 'periphery-warning-face)
                                 (format "%s%s%s %s"
                                         (propertize message 'face 'periphery-message-face)
                                         (propertize failingAttribute 'face 'periphery-identifier-face)
                                         (propertize messageRest 'face 'periphery-message-face)
                                         (propertize otherEntries 'face 'periphery-linenumber-face)
                                         )
                                 ))))))

(defun periphery-run-bartycrouch-parser (input directory)
  "Run bartycrouchparsing as INPUT DIRECTORY."
  (setq periphery-directory-root directory)
  (setq periphery-errorList nil)
  (dolist (line (split-string input "\n"))
    (when-let ((entry (parse-bartycrouch-output-line (string-trim-left line))))
      (push entry periphery-errorList)))
  (when periphery-errorList
      (periphery--listing-command periphery-errorList)))
  
(defun parse-xcodebuild-notes-and-errors (line)
  "Parse error and notes (as LINE)."
  (setq default-length 7)
  (save-match-data
    (and (string-match periphery-note-and-errors-regex line)
         (let* ((note (match-string 1 line))
                (message (match-string 2 line)))
           (periphery--build-list
            :path ""
            :file ""
            :line ""
            :keyword note
            :result message
            :regex periphery-regex-mark-quotes)))))

(cl-defun periphery-message-with-count (&key tag &key text &key count &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES with Count."
  (interactive)
  (setq-local inhibit-message nil)
  (message
    "%s %s occurences found %s"
    (propertize tag 'face attributes)
    (propertize count 'face 'periphery-warning-face-full)
    (propertize text 'face 'periphery-identifier-face))
  (setq-local inhibit-message t))

(defun parse--search-query (text query)
  "Parse error and notes (as TEXT) and QUERY."
  (setq default-length 6)
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
              (propertize (file-name-nondirectory file) 'face 'periphery-filename-face)
              (propertize line 'face 'periphery-linenumber-face)
              (periphery--propertize-severity keyword (string-trim-left keyword))
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
              (propertize (file-name-nondirectory file) 'face 'periphery-filename-face)
              (propertize line 'face 'periphery-linenumber-face)
              (periphery--propertize-severity keyword (string-trim-left keyword))
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
  (setq default-length 7)
  (setq periphery-errorList '())
  (dolist (line (split-string text "\n"))
    (when-let ((entry (parse--search-query (string-trim-left line) query)))
      (push entry periphery-errorList)))

  (when periphery-errorList
    (progn
      (periphery--listing-command periphery-errorList)
      (if (proper-list-p tabulated-list-entries)
          (periphery-message-with-count
           :tag "[Matches]"
           :text query
           :count (format "%d" (length periphery-errorList))
           :attributes 'success))
      (switch-to-buffer-other-window periphery-buffer-name))))

(defun periphery--remove-leading-keyword (tag)
  "Remove leading keyword and C style -comment from (as TAG)."
  (string-trim-left
  (replace-regexp-in-string "\\/\\/\\W?\\w+\\b:" "" tag)))

(defun periphery--remove-comments-in-string (text)
  "Remove comments from (as TEXT)."
  (replace-regexp-in-string ":" "" (replace-regexp-in-string "\\/" "" text)))

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

;;;###autoload
(defun periphery-svg-tags ()
  "Get svg tags."
  '(("\\(\\/\\/\\W?\\w+\\b:.*\\)" . ((lambda (tag) (svg-tag-make (periphery--remove-leading-keyword tag)
                                                                 :face (svg-color-from-tag tag)
                                                                 :inverse t
                                                                 :crop-left t))))
    
    ("\\(\\/\\/\\W?\\w+\\b:\\)" . ((lambda (tag)
                                     (svg-tag-make (periphery--remove-comments-in-string tag)
                                                   :face (svg-color-from-tag tag)
                                                   :inverse nil
                                                   :margin 0
                                                   :crop-right nil))))

    ("\\/\\/\\W?swiftlint:disable" . ((lambda (tag) (svg-tag-make "SWIFTLINT|DISABLE" :face 'periphery-hack-face-full :margin 0))))
    ("swiftlint:disable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'periphery-hack-face-full :crop-left t :inverse t))))

    ("\\/\\/\\W?swiftlint:enable" . ((lambda (tag) (svg-tag-make "SWIFTLINT|ENABLE" :face 'periphery-note-face-full :margin 0))))
    ("swiftlint:enable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'periphery-note-face-full :crop-left t :inverse t))))
    ))

(provide 'periphery)

;;; periphery.el ends here
