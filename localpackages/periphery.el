;;; Periphery --- A simple package to parse output from compile commands
;; -*- lexical-binding: t -*-

;;; Commentary: --- A simple package

;;; Code:
(require 'dash)
(require 'transient)
(require 'evil)

(defface periphery-info-face
  '((t (:inherit compilation-info :bold t)))
  "Warning."
  :group 'periphery)

(defface periphery-warning-face
  '((t (:inherit warning :bold t)))
  "Warning."
  :group 'periphery)

(defface periphery-error-face
  '((t (:inherit error :bold t)))
  "Warning."
  :group 'periphery)

(defface periphery-filename-face
  '((t (:inherit font-lock-builtin-face)))
  "Warning."
  :group 'periphery)

(defface periphery-linenumber-face
  '((t (:inherit line-number)))
  "Warning."
  :group 'periphery)

(defface periphery-identifier-face
  '((t (:inherit font-lock-constant-face :italic t :weight semi-bold)))
  "Warning."
  :group 'periphery)

(defface periphery-message-face
  '((t (:inherit font-lock-comment-face :italic nil)))
  "Warning."
  :group 'periphery)

(defconst periphery-buffer-name "*Periphery*")

(defvar periphery-mode-map nil "Keymap for periphery.")
(setq periphery-mode-map (make-sparse-keymap))

(define-key periphery-mode-map (kbd "RET") #'periphery--open-current-line)
(define-key periphery-mode-map (kbd "<return>") #'periphery--open-current-line)
(define-key periphery-mode-map (kbd "o") #'periphery--open-current-line)

(defconst periphery-regex-parser "\\(^\/[^:]+\\):\\([0-9]+\\)?:\\([0-9]+\\)?:?\w?\\([^:]+\\).\\(.*\\)")
(defconst periphery-parse-line-regex "^\\([^:]+\\):\\([0-9]+\\)?:\\(\\([0-9]+\\)\\)?")
(defconst periphery-remove-unicode-regex "[^\x00-\x7F]+"
  "Remove unicode-characters.")

(defconst periphery-note-and-errors-regex "\\(^[^\s:]+\\):\s\\(.+\\)$"
  "When we fail because of other errors than compilation errors.")

(defconst periphery-regex-mark-quotes "\\('[^']+'\\)")

(defvar periphery-errorList '())
(defvar periphery-directory-root nil "DirectoryRoot for localizeable.")

(define-derived-mode periphery-mode tabulated-list-mode "Periphery-mode"
  "Periphery mode.  A mode to show compile errors like Flycheck."
  (setq tabulated-list-format [
                               ("File" 30 t)
                               ("Line" 5 nil)
                               ("Type" 8 nil)
                               ("Message" 80 nil)
                               ])
  (setq tabulated-list-padding 1)
  (turn-off-evil-mode)
  (setq tabulated-list-sort-key (cons "Line" nil))
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

(defun periphery-listing-command (errorList)
 "Create a ERRORLIST for current mode."
  (pop-to-buffer periphery-buffer-name nil)
  (periphery-mode)
  (setq tabulated-list-entries (-non-nil errorList))
  (tabulated-list-print t))


(defun mark-all-symbols (input regex)
  "Highlight all quoted symbols (as INPUT) and REGEX."
  (save-match-data
    (let* ((position 0)
           (normalizedInput (replace-regexp-in-string "’" "'"  input)))
      (while (string-match regex normalizedInput position)
        (let* ((ref (match-string 1 normalizedInput))
               (startPosition (string-match regex normalizedInput position)))
          (setq position (match-end 1))
          (put-text-property startPosition position 'face 'periphery-identifier-face normalizedInput)))
  normalizedInput)))

(defun parse-periphery-output-line (line)
  "Run regex over curent LINE."
  (save-match-data
    (and (string-match periphery-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (column (match-string 3 line))
                (type (match-string 4 line))
                (message (match-string 5 line))
                (fileWithLine (format "%s:%s:%s" file linenumber column)))
               (list fileWithLine (vector
                                   (propertize (file-name-sans-extension (file-name-nondirectory file)) 'face 'periphery-filename-face)
                                   (propertize linenumber 'face 'periphery-linenumber-face)
                                   (propertize-severity type (string-trim-left type))
                                   (mark-all-symbols
                                    (propertize (string-trim-left message) 'face 'periphery-message-face)
                                    periphery-regex-mark-quotes)
                                   ))
                                 ))))


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
  

(defun propertize-severity (severity text)
  "Colorize TEXT using SEVERITY."
  (let ((type (string-trim-left severity)))
    (cond
     ((string= type "info")
      (propertize text 'face 'compilation-info-face))
     ((string= type "note")
      (propertize text 'face 'periphery-info-face))
     ((string= type "warning")
      (propertize text 'face 'periphery-warning-face))
     ((string= type "error")
      (propertize text 'face 'periphery-error-face))
     (t (propertize text 'face 'periphery-info-face)))))

(defun periphery-run-parser (input)
  "Run parser (as INPUT)."
  (setq periphery-errorList nil)
  (dolist (line (split-string input "\n"))
    (let* ((entry (parse-periphery-output-line (string-trim-left (replace-regexp-in-string periphery-remove-unicode-regex "" line))))
          (secondEntry (parse-xcodebuild-notes-and-errors (replace-regexp-in-string periphery-remove-unicode-regex "" line))))
      (if entry
          (push entry periphery-errorList))
      (unless entry (and secondEntry
          (push secondEntry periphery-errorList))
      )))
  (if periphery-errorList
      (progn
        (periphery-listing-command periphery-errorList)
        (periphery--go-to-first-error periphery-errorList))
    (periphery-message :tag "[Complete]" :text "No errors or warnings found" :attributes '(:inherit success))))

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
           (string-match-p (regexp-quote filter)
                           (aref (car( cdr it)) index)) (-non-nil periphery-errorList)))
    (tabulated-list-print t)))

(defun periphery-mode-list-errors ()
  "Filter on errors."
  (interactive)
  (periphery-mode-build-filter "error" 2))

(defun periphery-mode-list-warnings ()
  "Filter on warnings."
  (interactive)
  (periphery-mode-build-filter "warning" 2))

(defun periphery-mode-list-functions ()
  "Filter on fucntions."
  (interactive)
  (periphery-mode-build-filter "Function" 3))

(defun periphery-mode-list-unused ()
  "Filter on fucntions."
  (interactive)
  (periphery-mode-build-filter "unused" 3))

(defun periphery-mode-list-initializer ()
  "Filter on fucntions."
  (interactive)
  (periphery-mode-build-filter "Initializer" 3))

(defun periphery-mode-list-protocol ()
  "Filter on protocol."
  (interactive)
  (periphery-mode-build-filter "Protocol" 3))

(defun periphery-mode-list-parameter ()
  "Filter on parameter."
  (interactive)
  (periphery-mode-build-filter "Parameter" 3))

(defun periphery-mode-list-property ()
  "Filter on property."
  (interactive)
  (periphery-mode-build-filter "Property" 3))

(defvar periphery-mode-map nil
  "Keymap for periphery.")

(setq periphery-mode-map (make-sparse-keymap))
(define-key periphery-mode-map (kbd "?") 'periphery-mode-help)
(define-key periphery-mode-map (kbd "a") 'periphery-mode-all)
(define-key periphery-mode-map (kbd "e") 'periphery-mode-list-errors)
(define-key periphery-mode-map (kbd "w") 'periphery-mode-list-warnings)
(define-key periphery-mode-map (kbd "f") 'periphery-mode-list-functions)
(define-key periphery-mode-map (kbd "u") 'periphery-mode-list-unused)
(define-key periphery-mode-map (kbd "i") 'periphery-mode-list-initializer)
(define-key periphery-mode-map (kbd "I") 'periphery-mode-list-protocol)
(define-key periphery-mode-map (kbd "P") 'periphery-mode-list-parameter)
(define-key periphery-mode-map (kbd "p") 'periphery-mode-list-property)
(define-key periphery-mode-map (kbd "<return>") 'periphery--open-current-line)
(define-key periphery-mode-map (kbd "o") 'periphery--open-current-line)

(transient-define-prefix periphery-mode-help ()
"Help for periphery mode."
["Periphery mode help"
    ("a" "All" periphery-mode-all)
    ("e" "Errors" periphery-mode-list-errors)
    ("w" "Warnings" periphery-mode-list-warnings)
    ("f" "Functions" periphery-mode-list-functions)
    ("u" "Unused" periphery-mode-list-unused)
    ("i" "Initializer" periphery-mode-list-initializer)
    ("I" "Protocol" periphery-mode-list-protocol)
    ("P" "Parameter" periphery-mode-list-parameter)
    ("p" "Property" periphery-mode-list-property)
    ("o" "Open" periphery--open-current-line)
 ])

(defun periphery-kill-buffer ()
  "Kill the periphery buffer."
  (interactive)
  (when (get-buffer periphery-buffer-name)
    (kill-buffer periphery-buffer-name)))

(defun periphery-show-buffer ()
  "Show current periphery buffer."
  (interactive)
  (periphery-listing-command periphery-errorList))

;;; - Bartycrouch parsing
(defconst bartycrouch-regex-parser "\\(\/+[^:]+\\):\\([0-9]+\\):[^:]+.\s[^:]+.\s+\\([^']+\\)\\('[^']+'\\)\\([^:]+:\\)\s\\(\[[0-9]+\]\\)")

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
      (periphery-listing-command periphery-errorList)))
  
(defun parse-xcodebuild-notes-and-errors (line)
  "Parse error and notes (as LINE)."
  (save-match-data
    (and (string-match periphery-note-and-errors-regex line)
         (let* ((note (match-string 1 line))
                (message (match-string 2 line)))
           (list "" (vector
                     (propertize "Buildinfo" 'face 'periphery-filename-face)
                     (propertize "" 'face 'periphery-message-face)
                     (propertize-severity (if note note "error") (string-trim-left note))
                     (propertize message 'face 'periphery-message-face)))))))


(cl-defun periphery-message (&key tag &key text &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES."
  (interactive)
  (setq-local inhibit-message nil)
  (message "%s %s" (propertize tag 'face attributes) text)

  ;; (periphery-message :tag "[Search result]:" :text (format "%d occurrences found.'" (length periphery-errorList) query) :attributes 'periphery-info-face)
  (setq-local inhibit-message t))

(cl-defun periphery-message-with-count (&key tag &key text &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES with Count"
  (interactive)
  (setq-local inhibit-message nil)
  (message
    "%s %s - %s occurances found."
    (propertize tag 'face attributes) text
    (propertize (format "%d" (length periphery-errorList)) 'face 'periphery-warning-face))

  (setq-local inhibit-message t))

(defconst periphery-parse-search "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).\\(.*\\)")

(defun parse--search-query (text query)
  "Parse error and notes (as TEXT) and QUERY."
  (save-match-data
    (and (string-match periphery-parse-search text)
         (let* ((file (match-string 1 text))
                (line (match-string 2 text))
                (column (match-string 3 text))
                (message (match-string 4 text))
                (fileWithLine (format "%s:%s:%s" file line column)))
           
             (list fileWithLine (vector
                                 (propertize (file-name-nondirectory file) 'face 'periphery-filename-face)
                                 (propertize line 'face 'periphery-linenumber-face)
                                 (propertize "match" 'face 'periphery-info-face)
                                 (mark-all-symbols
                                    (propertize (string-trim-left message) 'face 'periphery-message-face)
                                  (format "\\(%s\\)" query))))))))

(defun periphery-parse-search-result (text query)
  "Parse search result (as TEXT) and QUERY."
  (setq periphery-errorList '())
  (dolist (line (split-string text "\n"))
    (when-let ((entry (parse--search-query (string-trim-left line) query)))
      (push entry periphery-errorList)))
  (when periphery-errorList
    (progn
      (periphery-message :tag "[Search result]:" :text (format "%d occurrences found for '%s'" (length periphery-errorList) query) :attributes 'periphery-info-face)
      (periphery-listing-command periphery-errorList))))

(provide 'periphery)
;;; periphery.el ends here

