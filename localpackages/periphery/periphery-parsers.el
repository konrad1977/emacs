;;; periphery-parsers.el --- Parser implementations for periphery -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains the actual parser implementations for different types of output.

;;; Code:

(require 'periphery-config)
(require 'periphery-core)
(require 'cl-lib)

;; Ensure builtin patterns are available
(unless (boundp 'periphery-builtin-patterns)
  (load "periphery-config"))

;; Compiler Error Parser
;;;###autoload
(defun periphery-parser-compiler (line)
  "Parse compiler error/warning from LINE."
  (save-match-data
    ;; Try the standard compiler pattern
    (when (string-match (alist-get 'compiler periphery-builtin-patterns) line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (column (match-string 3 line))
             (severity (match-string 4 line))
             (raw-message (string-trim (or (match-string 5 line) "")))
             (path (if column
                       (format "%s:%s:%s" file line-num column)
                     (format "%s:%s" file line-num)))
             ;; Apply highlighting to the message
             (highlighted-message (periphery-parser--apply-highlighting raw-message)))
        (when (and file line-num severity)
          (periphery-core-build-entry
           :path path
           :file file
           :line line-num
           :column column
           :severity severity
           :message highlighted-message
           :face-fn #'periphery-parser--severity-face))))))

;; XCTest Parser
;;;###autoload
(defun periphery-parser-xctest (line)
  "Parse XCTest output from LINE."
  (save-match-data
    (when (string-match (alist-get 'xctest periphery-builtin-patterns) line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (test-name (match-string 4 line))
             (message (match-string 5 line))
             (path (format "%s:%s" file line-num)))
        (periphery-core-build-entry
         :path path
         :file file
         :line line-num
         :severity "Failed"
         :message (format "%s: %s" test-name message)
         :face-fn #'periphery-parser--severity-face)))))

;; Search Result Parser
;;;###autoload
(defun periphery-parser-search (line &optional query)
  "Parse search result from LINE with optional QUERY highlighting."
  (save-match-data
    (when (string-match (alist-get 'search periphery-builtin-patterns) line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (column (match-string 3 line))
             (content (string-trim-left (match-string 4 line)))
             (path (format "%s:%s:%s" file line-num column)))
        
        ;; Check if it's a TODO/FIXME
        (if (string-match (alist-get 'todos periphery-builtin-patterns) content)
            (periphery-parser-todo-from-content content path file line-num)
          ;; Regular search match
          (periphery-core-build-entry
           :path path
           :file file
           :line line-num
           :column column
           :severity "Match"
           :message (if query 
                        (periphery-parser--highlight-match content query)
                      content)
           :face-fn #'periphery-parser--match-face))))))

;; TODO/FIXME Parser
;;;###autoload
(defun periphery-parser-todo (line)
  "Parse TODO/FIXME from LINE."
  (save-match-data
    (when (string-match (alist-get 'todos periphery-builtin-patterns) line)
      (let* ((keyword (match-string 1 line))
             (comment (string-trim (match-string 2 line))))
        (list keyword comment)))))

(defun periphery-parser-todo-from-content (content path file line-num)
  "Parse TODO from CONTENT with PATH FILE and LINE-NUM."
  (when-let ((todo-data (periphery-parser-todo content)))
    (periphery-core-build-entry
     :path path
     :file file
     :line line-num
     :severity (car todo-data)
     :message (cadr todo-data)
     :face-fn #'periphery-parser--todo-face)))

;; Ktlint Parser
;;;###autoload
(defun periphery-parser-ktlint (line)
  "Parse ktlint output from LINE."
  (save-match-data
    (when (string-match (alist-get 'ktlint periphery-builtin-patterns) line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (column (match-string 3 line))
             (message (string-trim (match-string 4 line)))
             (rule (match-string 5 line))
             (path (format "%s:%s:%s" file line-num column)))
        (periphery-core-build-entry
         :path path
         :file file
         :line line-num
         :column column
         :severity "warning"
         :message (format "%s (%s)" message rule)
         :face-fn #'periphery-parser--severity-face)))))

;; SwiftLint Parser
;;;###autoload
(defun periphery-parser-swiftlint (line)
  "Parse SwiftLint output from LINE."
  (save-match-data
    (when (string-match "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.*\\)" line)
      (let* ((file (match-string 1 line))
             (line-num (match-string 2 line))
             (column (match-string 3 line))
             (severity (match-string 4 line))
             (message (match-string 5 line))
             (path (format "%s:%s:%s" file line-num column)))
        (periphery-core-build-entry
         :path path
         :file file
         :line line-num
         :column column
         :severity severity
         :message message
         :face-fn #'periphery-parser--severity-face)))))

;; Helper Functions
(defun periphery-parser--highlight-match (text query)
  "Highlight QUERY in TEXT."
  (if query
      (replace-regexp-in-string
       (regexp-quote query)
       (lambda (match)
         (propertize match 'face 'periphery-identifier-face))
       text t t)
    text))

(defun periphery-parser--severity-face (severity)
  "Get face for SEVERITY."
  (let ((type (upcase (string-trim severity))))
    (cond
     ((string-match-p "ERROR\\|FAILED" type) 'periphery-error-face-full)
     ((string-match-p "WARNING" type) 'periphery-warning-face-full)
     ((string-match-p "NOTE\\|INFO" type) 'periphery-note-face-full)
     ((string-match-p "MATCH" type) 'periphery-warning-face-full)
     (t 'periphery-info-face-full))))

(defun periphery-parser--todo-face (keyword)
  "Get face for TODO KEYWORD."
  (let ((type (upcase keyword)))
    (cond
     ((string= type "TODO") 'periphery-todo-face-full)
     ((string-match-p "FIX\\|FIXME" type) 'periphery-fix-face-full)
     ((string= type "HACK") 'periphery-hack-face-full)
     ((string= type "NOTE") 'periphery-note-face-full)
     ((string= type "PERF") 'periphery-performance-face-full)
     ((string= type "MARK") 'periphery-mark-face-full)
     (t 'periphery-info-face-full))))

(defun periphery-parser--match-face (_)
  "Get face for search match."
  'periphery-warning-face-full)

(defun periphery-parser--apply-highlighting (message)
  "Apply syntax highlighting to MESSAGE for strings, parentheses, etc."
  (require 'periphery)  ; For periphery--mark-all-symbols
  (when periphery-debug
    (message "Applying highlighting to: %s" message))
  (let ((highlighted message))
    ;; Highlight strings and quotes with different faces
    (when (boundp 'periphery-highlight-patterns)
      ;; Apply highlighting in the right order: content first, then delimiters
      
      ;; Highlight content inside quotes
      (let ((quote-content-regex (alist-get 'quote-content periphery-highlight-patterns))
            (quote-content-face (alist-get 'quote-content periphery-syntax-faces)))
        (setq highlighted (periphery--mark-all-symbols
                           :input highlighted
                           :regex quote-content-regex
                           :property `(face ,quote-content-face))))
      
      ;; Highlight content inside strings  
      (let ((string-content-regex (alist-get 'string-content periphery-highlight-patterns))
            (string-content-face (alist-get 'string-content periphery-syntax-faces)))
        (setq highlighted (periphery--mark-all-symbols
                           :input highlighted
                           :regex string-content-regex
                           :property `(face ,string-content-face))))
      
      ;; Highlight quote marks
      (let ((quote-marks-regex (alist-get 'quote-marks periphery-highlight-patterns))
            (quote-marks-face (alist-get 'quote-marks periphery-syntax-faces)))
        (setq highlighted (periphery--mark-all-symbols
                           :input highlighted
                           :regex quote-marks-regex
                           :property `(face ,quote-marks-face))))
      
      ;; Highlight string marks
      (let ((string-marks-regex (alist-get 'string-marks periphery-highlight-patterns))
            (string-marks-face (alist-get 'string-marks periphery-syntax-faces)))
        (setq highlighted (periphery--mark-all-symbols
                           :input highlighted
                           :regex string-marks-regex
                           :property `(face ,string-marks-face))))
      
      ;; Highlight parentheses
      (let ((parens-regex (alist-get 'parentheses periphery-highlight-patterns))
            (parens-face (alist-get 'parentheses periphery-syntax-faces)))
        (setq highlighted (periphery--mark-all-symbols
                           :input highlighted
                           :regex parens-regex
                           :property `(face ,parens-face)))))
    ;; Apply base message face only to parts that don't have face properties
    (when periphery-debug
      (message "Final highlighted: %s" highlighted))
    ;; Don't overwrite existing face properties
    highlighted))

;; Register default parsers
;;;###autoload
(defun periphery-parsers-initialize ()
  "Initialize and register all default parsers."
  
  (periphery-register-parser
   'compiler
   :name "Compiler"
   :regex (alist-get 'compiler periphery-builtin-patterns)
   :type :compiler
   :priority 100
   :parse-fn #'periphery-parser-compiler
   :face-fn #'periphery-parser--severity-face)
  
  (periphery-register-parser
   'xctest
   :name "XCTest"
   :regex (alist-get 'xctest periphery-builtin-patterns)
   :type :test
   :priority 90
   :parse-fn #'periphery-parser-xctest
   :face-fn #'periphery-parser--severity-face)
  
  (periphery-register-parser
   'ktlint
   :name "Ktlint"
   :regex (alist-get 'ktlint periphery-builtin-patterns)
   :type :linter
   :priority 60
   :parse-fn #'periphery-parser-ktlint
   :face-fn #'periphery-parser--severity-face)
  
  (periphery-register-parser
   'swiftlint
   :name "SwiftLint"
   :regex "\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.*\\)"
   :type :linter
   :priority 65
   :parse-fn #'periphery-parser-swiftlint
   :face-fn #'periphery-parser--severity-face))

;; Package/Build Error Parsers
;;;###autoload  
(defun periphery-parser-package-error (input)
  "Parse package dependency errors from INPUT."
  (when (string-match-p "^xcodebuild: error: Could not resolve package dependencies:" input)
    (periphery-core-build-entry
     :path "Package Dependencies"
     :file "Package.swift"
     :line "1"
     :severity "error"
     :message input
     :face-fn #'periphery-parser--severity-face)))

;;;###autoload
(defun periphery-parser-build-failure (input)
  "Parse build failures from INPUT."
  (when (string-match-p "^The following build commands failed:" input)
    (periphery-core-build-entry
     :path "_Build Failure"
     :file "_Build Failure"
     :line "999"
     :severity "error"
     :message input
     :face-fn #'periphery-parser--severity-face)))

;; Auto-initialize on load
(periphery-parsers-initialize)

(provide 'periphery-parsers)
;;; periphery-parsers.el ends here