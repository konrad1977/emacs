;;; periphery-quick --- A small package for quickly finding symbols -*- lexical-binding: t -*-
;;; Code:

(require 'periphery-helper)
(require 'projectile)

(defconst periphery-quick-regex-parser "\\([^:]+\\):\\([0-9]+\\)?:\\([0-9]+\\):\s?\\(.+\\)"
  "Parse vimgrep like strings (compilation).")

(defun periphery-quick:parse (input)
  "Parse as (INPUT)."
  (setq list '())
  (dolist (line (split-string input "\n"))
    (let* ((entry (periphery-quick:parse-line line)))
      (if entry
          (push entry list))))
  (open-current-line-with
   (periphery-quick:showmenu-with-title
    :title "Result "
    :list list)))

(defun periphery-quick:parse-line (line)
  "Parse (as LINE)."
  (save-match-data
    (and (string-match periphery-quick-regex-parser line)
         (let* (
                (file (match-string 1 line))
                (linenumber (match-string 2 line))
                (column (match-string 3 line))
                (text (string-trim-left (match-string 4 line)))
                (fileWithLine (format "%s:%s:%s" file linenumber column)))
           (list (format "%s:\t\t %s" 
                         (propertize
                          (file-name-sans-extension
                           (file-name-nondirectory file)) 'face 'periphery-filename-face)
                         text) fileWithLine)))))

(cl-defun periphery-quick:run-query (query)
  "Run query (as QUERY)."
  (let ((default-directory (projectile-project-root)))
    (async-shell-command-to-string
     :process-name "periphery quick"
     :command (format "rg -w %s --vimgrep" query)
     :callback #'periphery-quick:parse)))

(defun periphery-quick:find ()
  "Quick find something in project."
  (interactive)
  (if-let ((query (thing-at-point 'symbol)))
      (periphery-quick:run-query query)
      (periphery-quick:run-query (read-string "Query: ")))
      )

(cl-defun periphery-quick:showmenu-with-title (&key title &key list)
  "Build menu with (TITLE LIST)."
  (let* ((choices (seq-map (lambda (item) item) list))
         (choice (completing-read title choices)))
    (car (cdr (assoc choice choices)))))

(provide 'periphery-quick)
;;; periphery-quickel ends here


