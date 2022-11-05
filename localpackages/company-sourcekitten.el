;;; company-sourcekittenten.el --- company-mode completion backend for SourceKit/Sourcekitten  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Natan Kot, Mikael Konradsson
;; This is heavly based on company-sourcekit
;; https://github.com/nathankot/company-sourcekit

;; Author: Mikael Konradsson <mikael.konradson@gmail.com>
;; Keywords: sourcekit swift
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A company-mode backend for swift projects. It communicates with Sourcekitten

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'json)
(require 'dash)
(require 'projectile)

(defgroup company-sourcekitten nil
  "Completion backend that uses sourcekitten."
  :group 'company)

(defcustom company-sourcekitten-use-yasnippet
  (fboundp 'yas-minor-mode)
  "Should Yasnippet be used for completion expansion."
  :type 'boolean
  :group 'company-sourcekitten)

(defcustom company-sourcekitten-verbose nil
  "Should log with verbosity to the messages buffer."
  :type 'boolean
  :group 'company-sourcekitten)

(defvar-local company-sourcekitten--tmp-file 'unknown)
(defun company-sourcekitten--tmp-file ()
  (when (eq company-sourcekitten--tmp-file 'unknown)
    (setq company-sourcekitten--tmp-file (make-temp-file "sourcekitten")))
  company-sourcekitten--tmp-file)

(defun company-sourcekitten--meta (candidate)
  "Gets the meta for the completion (as CANDIDATE)."
  (get-text-property 0 'description candidate))

(defun company-sourcekitten--annotation (candidate)
  "Gets the type of the completion (as CANDIDATE)."
  (format " %s" (get-text-property 0 'type candidate)))

;; -- Privates 
(defun sourcekitten--number-of-cores ()
  "Fetch number of available cores."
  (if-let ((cores (replace-regexp-in-string "\n$" "" (shell-command-to-string "sysctl -n hw.ncpu"))))
      cores
    2))

(defun clean-text-from-errors (text)
  "Clean up text (as TEXT)."
  (if (string-prefix-p "[" text)
    (progn
      (message "Clean sourcekitten works.")
      text
    )
    (progn
      (if (string-match "\\[\\(.*\\)\\]" text)
          (format "[%s]" (match-string 1 text))
        "[]"
        ))))

(defun sourcekitten--convert-list-to-string (list)
  "Convert LIST to string."
  (let* ((string-with-parenthesis (format "%S" list))
     (end (- (length string-with-parenthesis) 2)))
    (substring string-with-parenthesis 2 end)))

(defun sourcekitten--sourcefiles ()
    "Get sourcefiles."
    (if-let ((files (directory-files-recursively (projectile-project-root) "\\.swift$" 't)))
        files
     nil
    ))

(cl-defun fetch-result-from-sourcekitten-as-json (&key file &key offset &key sdk &key target &key callback)
  "Fetch request as json directly."

  (setq requestCommand 
        (concat 
         "sourcekitten complete "
         (format "--file %s " file)
         (format "--offset %d -- " offset)
         (format "\-sdk %s " sdk)
         (format "-j%s " (sourcekitten--number-of-cores))
         (format "%s" (sourcekitten--convert-list-to-string (sourcekitten--sourcefiles)))
         ))
  (let* ((json (json-read-from-string (clean-text-from-errors (shell-command-to-string requestCommand)))))
    (funcall callback json)))

(defun company-sourcekitten--process-json (return-json)
  "Given json returned from sourcekitten, turn it into a list compatible with company-mode"
  (append (mapcar
           (lambda (l)
             (let* (
                    (name (cdr (assoc 'name l)))
                    (desc (cdr (assoc 'descriptionKey l)))
                    (src (cdr (assoc 'name l)))
                    (type (cdr (assoc 'typeName l))))
               (propertize (company-sourcekitten--normalize-source-text src)
                           'sourcetext src
                           'description desc
                           'type type)))
           return-json) nil))

(defun company-sourcekitten--meta (candidate)
  "Gets the meta for the completion (as CANDIDATE)."
  (get-text-property 0 'description candidate))

(defun company-sourcekitten--annotation (candidate)
  "Gets the type of the completion candidate."
  (format " %s" (get-text-property 0 'type candidate)))

;;;###autoload
(defun company-sourcekitten (command &optional arg &rest ignored)
   "Code completion from sourcekitten."
   (interactive (list 'interactive))
   (cl-case command
     (interactive (company-begin-backend 'company-sourcekitten))
     (prefix (company-sourcekit--prefix))
     (candidates (cons :async (lambda (cb) (company-sourcekitten--candidates arg cb))))
     (annotation (company-sourcekitten--annotation arg))
     (meta (company-sourcekitten--meta arg))
     ))

(defun clean-up-newlines (text)
  "Clean up new lines (as TEXT)."
  (string-trim-left
   (replace-regexp-in-string "\n$" "" text)))

(defun company-sourcekitten--simulator-sdk-version ()
  "Get the current simulator sdk-version."
  (clean-up-newlines (shell-command-to-string "xcrun --sdk iphonesimulator --show-sdk-version")))

(defun company-sourcekitten--simulator-sdk-path ()
  "Get the current simulator sdk-path."
  (clean-up-newlines (shell-command-to-string "xcrun --show-sdk-path --sdk iphonesimulator")))

(defun company-sourcekitten--current-arch ()
  "Get the current arch."
  (clean-up-newlines (shell-command-to-string "clang -print-target-triple")))

(defun company-sourcekitten--simulator-target ()
  "Get the current simulator sdk."
  (let* ((target-components (split-string (company-sourcekitten--current-arch) "-"))
         (arch (nth 0 target-components))
         (vendor (nth 1 target-components))
         (version (company-sourcekitten--simulator-sdk-version)))
    (format "%s-%s-ios%s-simulator" arch vendor version)))

(defun company-sourcekitten--candidates (prefix callback)
  "Use sourcekitten to get a list of completion (as PREFIX) (as CALLBACK)."

  (let ((tmpfile (company-sourcekitten--tmp-file)))
    (write-region (point-min) (point-max) tmpfile nil 'silent)
    (fetch-result-from-sourcekitten-as-json
        :file tmpfile
        :offset (1- (position-bytes (point)))
        :sdk (company-sourcekitten--simulator-sdk-path)
        :target (company-sourcekitten--simulator-target)
        :callback (company-sourcekitten--make-callback callback))))

(defun company-sourcekitten--make-callback (callback)
  "The handler for process output (as CALLBACK)."
  (lambda (json)
    (let ((completions (-filter
                        (lambda (candidate) (eq 0 (string-match-p company-prefix candidate)))
                        (company-sourcekitten--process-json json))))
      (funcall callback completions))))

(defun company-sourcekitten--normalize-source-text (sourcetext)
  "Make a more readable completion candidate out of one with placeholders."
  (replace-regexp-in-string
   "<#T##\\(.*?\\)#>"
   (lambda (str)
     ;; <#T##Int#> - No label, argument only
     (save-match-data
       (string-match "<#T##\\(.*?\\)#>" str)
       (format "%s" (car (split-string (match-string 1 str) "#")))))
   sourcetext))
;; (add-to-list 'company-backends 'company-sourcekitten)

(defun company-sourcekit--prefix ()
  "In our case, the prefix acts as a cache key for company-mode.
It never actually gets sent to the completion engine."
  (and
    (eq major-mode 'swift-mode)
    (not (company-in-string-or-comment))
    (or
      ;; Fetch prefix during import statements:
      ;;
      ;; Given: "import |"
      ;; Prefix: ""
      ;; Offset: 7
      ;;
      ;; Given: "import Found|"
      ;; Prefix: "Found"
      ;; Offset: 7
      (-when-let* ((x (company-grab-symbol-cons "import ")) (_ (listp x))) x)

      ;; Fetch prefix for method calls:
      ;;
      ;; Given: "self.|"
      ;; Prefix: ""
      ;; Offset: 5
      ;;
      ;; Given: "self.hel|"
      ;; Prefix: "hel"
      ;; Offset: 5
      (let ((r (company-grab-symbol-cons "\\.")))
        (when (consp r) r))

      ;; Fetch prefix for function calls:
      ;;
      ;; Given: "CGRect(|)"
      ;; Prefix: ""
      ;; Offset: 7
      ;;
      ;; Given: "CGRect(x:|)"
      ;; Prefix: "x:"
      ;; Offset: 7
      (-if-let (x (company-grab "\_*(\\([\w\_:]*?\\)" 1 (line-beginning-position)))
        (cons x t))

      ;; Fetch prefix for symbols:
      ;;
      ;; Given: "let r = CGRe|"
      ;; Prefix: ""
      ;; Offset: 12
      ;;
      ;; Given: "let r = CGRec|"
      ;; Prefix: ""
      ;; Offset: 13
      (-if-let (x (company-grab-symbol))
        (when (> (length x) 0) (cons "" t))))))

(provide 'company-sourcekitten)

;;; company-sourcekitten.el ends here
