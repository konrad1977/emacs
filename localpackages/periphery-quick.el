;;; periphery-quick.el --- A small package for quickly finding symbols -*- lexical-binding: t -*-
;;; Code:

(require 'periphery-helper)
(require 'project)

(defconst periphery-quick-regex-parser "\\([^:]+\\):\\([0-9]+\\)?:\\([0-9]+\\):\s?\\(.+\\)"
  "Parse vimgrep like strings (compilation).")

(defun periphery-quick:parse (input)
  "Parse as (INPUT)."
  (setq list '())
  (dolist (line (split-string input "\n"))
    (let ((entry (periphery-quick:parse-line line)))
      (if entry
          (push entry list))))
  (open-current-line-with
   (periphery-quick:showmenu-with-title
    :title "Result "
    :list list)))

(defun add-right-padding-up-to (word max-length)
  "Add padding to (as WORD) if smaller then (as MAX-LENGTH)."
  (if (> (length word) max-length)
      (concat (substring word 0 max-length) "")
    (progn
      (setq copy word)
      (while (< (string-width copy) max-length)
        (setq copy (concat copy " "))))
    copy))

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
           (list (format "%s %s"
                         (propertize
                          (add-right-padding-up-to
                           (file-name-sans-extension
                            (file-name-nondirectory file))
                            24
                           )
                          'face 'periphery-filename-face)
                         text) fileWithLine)))))

;;;###autoload
(cl-defun periphery-quick:run-query (query)
  "Run query (as QUERY)."
  (let ((default-directory (periphery-helper:project-root-dir)))
    (async-start-command-to-string
     :command (format "rg -wse \'%s\' --color=never --no-heading --with-filename --line-number --column --sort path" query)
     :callback (lambda (output) (periphery-quick:parse output)))))

(defun periphery-quick:run-query-file (query file)
  "Run query (as QUERY) on file."
  (let ((file file)
        (query query))
    (async-start-command-to-string
     :command (format "rg -wse \'%s\' %s --color=never --no-heading --with-filename --line-number --column" query file)
     :callback (lambda (output)
                  (periphery-quick:parse output)))))

(defun periphery-quick:find-in-file ()
  "Quick find in file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if-let ((query (thing-at-point 'symbol)))
        (periphery-quick:run-query-file query file)
      (periphery-quick:run-query-file (read-string "Query: ") file))))

(defun periphery-quick:find-ask ()
  "Quickfind but ask user for input."
  (interactive)
  (let ((default-directory (periphery-helper:project-root-dir))
        (query (read-string "Query: ")))
    (when (> (length query) 0)
      (async-start-command-to-string
       :command (format "rg -S \'%s\' --color=never --no-heading --with-filename --line-number --column" query)
       :callback (lambda (output)
                   (periphery-quick:parse output))))))

(defun periphery-quick:find ()
  "Quick find something in project."
  (interactive)
  (if-let ((query (thing-at-point 'symbol)))
      (periphery-quick:run-query query)
    (periphery-quick:run-query (read-string "Query: "))))

(defun periphery-quick:todos ()
  "Find the todos in the project."
  (interactive)
  (periphery-quick:run-query (regexp-quote "(NOTE|FIXME|FIX|TODO|HACK|PERF):")))

(cl-defun periphery-quick:showmenu-with-title (&key title &key list)
  "Build menu with (TITLE LIST)."
  (let* ((choices (seq-map (lambda (item) item) list))
         (choice (completing-read title choices)))
    (car (cdr (assoc choice choices)))))

(provide 'periphery-quick)
;;; periphery-quick.el ends here


