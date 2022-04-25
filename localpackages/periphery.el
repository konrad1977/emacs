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
(define-derived-mode periphery-mode tabulated-list-mode "Periphery-mode"
  "Periphery mode.  A mode to show compile errors like Flycheck."
  (setq tabulated-list-format [
                               ("File" 24 t)
                               ("Line" 5 nil)
                               ("Severity" 10 nil)
                               ("Message" 80 nil)
                               ])
  (setq tabulated-list-padding 2)
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
        (message (format "%s %d %d" file linenumber column))
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
           (unless (< (length type) 0)
               (setq type "error"))
             (list fileWithLine (vector
                                 (file-name-sans-extension (file-name-nondirectory file))
                                 (propertize linenumber 'face 'periphery--blue-face)
                                 (propertize-severity type (string-trim-left type))
                                 (propertize-severity type (string-trim-left message))))))))


(defun propertize-severity (severity text)
  "Colorize TEXT using SEVERITY."
  (let ((type (string-trim-left severity)))
    (cond
     ((string= type "info")
      (propertize text 'face 'periphery--gray-face))
     ((string= type "warning")
      (propertize text 'face 'periphery--yellow-face))
     ((string= type "error")
      (propertize text 'face 'periphery--red-face))
     (t (propertize text 'face 'periphery--gray-face)))))

;; (mapc #'periphery-process-line (split-string (input) "\n"))

(defun periphery-run-parser (input)
  "Run parser (as INPUT)."
  (setq errorList '())
  (dolist (line (split-string input "\n"))
    (let ((entry (parse-periphery-output-line (string-trim-left (replace-regexp-in-string periphery-remove-unicode-regex "" line)))))
      (if entry
          (push entry errorList))))
  (periphery-listing-command errorList))

(provide 'periphery)
;;; periphery.el ends here

