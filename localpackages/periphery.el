;;; Periphery --- A simple package to parse output from compile commands

;;; Commentary: --- A simple package


;;; Code:
(require 'dash)
(defface periphery--red-face
  '((((class color) (background light)) :foreground "#FF5D62" :weight semi-bold)
    (((class color) (background dark)) :foreground "#FF5D62" :weight semi-bold))
  "Face of red."
  :group 'periphery)

(defface periphery--gray-face
  '((((class color) (background light)) :foreground "#717C7C")
    (((class color) (background dark)) :foreground "#717C7C"))
  "Face of gray."
  :group 'periphery)

(defface periphery--blue-face
  '((((class color) (background light)) :foreground "#7E9CD8")
    (((class color) (background dark)) :foreground  "#7E9CD8"))
  "Face of blue."
  :group 'periphery)

(defface periphery--yellow-face
  '((((class color) (background light)) :foreground "#DCA561")
    (((class color) (background dark)) :foreground  "#DCA561"))
  "Face of yellow."
  :group 'periphery)

(defvar periphery-mode-map nil "Keymap for periphery.")
(setq periphery-mode-map (make-sparse-keymap))

(define-key periphery-mode-map (kbd "RET") #'open-current-line-id)
(define-key periphery-mode-map (kbd "<return>") #'open-current-line-id)
(define-key periphery-mode-map (kbd "o") #'open-current-line-id)

(defconst periphery-regex-parser "\\(^\/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\w?\\([^:]+\\).\\(.+\\)")
(defconst periphery-parse-line-regex "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$")
(defconst periphery-remove-unicode-regex "[^\x00-\x7F]+")

(defconst periphery-buffer-name "*Periphery*")
(defvar periphery-errorList '())

(define-derived-mode periphery-mode tabulated-list-mode "Periphery-mode"
  "Periphery mode.  A mode to show compile errors like Flycheck."
  (setq tabulated-list-format [
                               ("File" 30 t)
                               ("Line" 5 nil)
                               ("Severity" 8 nil)
                               ("Message" 80 nil)
                               ])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "File" nil))
  (tabulated-list-init-header))

(defun open-current-line-id ()
  "Open current row."
  (interactive)
  (save-match-data
    (let* ((data (tabulated-list-get-id))
           (matched (string-match periphery-parse-line-regex data))
           (file (match-string 1 data))
           (linenumber (string-to-number (match-string 2 data)))
           (column (string-to-number (match-string 3 data))))
        (with-current-buffer (find-file file)
          (when (> linenumber 0)
            (goto-char (point-min))
            (forward-line (1- linenumber))
            (when (> column 0)
              (forward-char (1- column))))))))

(defun periphery-listing-command (errorList)
 "Create a ERRORLIST for current mode."
  (pop-to-buffer periphery-buffer-name nil)
  (periphery-mode)
  (setq tabulated-list-entries (-non-nil errorList))
  (tabulated-list-print t))

(defun parse-periphery-output-line (line)
  "Run regex over cuurent LINE."
  (save-match-data
    (and (string-match periphery-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (column (match-string 3 line))
                (type (match-string 4 line))
                (message (match-string 5 line))
                (fileWithLine (format "%s:%s:%s" file linenumber column)))
             (list fileWithLine (vector
                                 (file-name-sans-extension (file-name-nondirectory file))
                                 (propertize linenumber 'face 'periphery--gray-face)
                                 (propertize-severity type (string-trim-left type))
                                 (propertize-message (string-trim-left message))
                                 ))))))


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
  (t (propertize text 'face 'periphery--gray-face))))
  

(defun propertize-severity (severity text)
  "Colorize TEXT using SEVERITY."
  (let ((type (string-trim-left severity)))
    (cond
     ((string= type "info")
      (propertize text 'face 'compilation-info))
     ((string= type "warning")
      (propertize text 'face 'compilation-warning))
     ((string= type "error")
      (propertize text 'face 'compilation-error))
     (t (propertize text 'face 'compilation-info)))))

;; (mapc #'periphery-process-line (split-string (input) "\n"))

(defun periphery-run-parser (input)
  "Run parser (as INPUT)."
  (setq periphery-errorList nil)
  (dolist (line (split-string input "\n"))
    (let ((entry (parse-periphery-output-line (string-trim-left (replace-regexp-in-string periphery-remove-unicode-regex "" line)))))
      (if entry
          (push entry periphery-errorList))))
  (if periphery-errorList
      (periphery-listing-command periphery-errorList)))

(defun periphery-show-errors ()
  "Show current errors."
  (periphery-listing-command periphery-errorList))

(defun periphery-kill-buffer ()
  "Kill the periphery buffer."
  (when (get-buffer periphery-buffer-name)
    (kill-buffer periphery-buffer-name)))


(provide 'periphery)
;;; periphery.el ends here

