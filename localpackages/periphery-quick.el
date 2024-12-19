;;; periphery-quick.el --- A small package for quickly finding symbols -*- lexical-binding: t -*-
;;; Code:

(require 'periphery-helper)
(require 'project)

(defconst periphery-quick-regex-parser "\\([^:]+\\):\\([0-9]+\\)?:\\([0-9]+\\):\s?\\(.+\\)"
  "Parse vimgrep like strings (compilation).")

(defvar periphery-quick:debug nil
  "Enable debug logging.")

(defun periphery-quick:debug-log (msg &rest args)
  "Log debug MSG with ARGS."
  (when periphery-quick:debug
    (apply #'message (concat "DEBUG: " msg) args)))

(defun periphery-quick:parse (input)
  "Parse as (INPUT)."
  (periphery-quick:debug-log "Starting parse with input: %s" (substring input 0 (min 100 (length input))))
  (setq list '())
  (dolist (line (split-string input "\n"))
    (let ((entry (periphery-quick:parse-line line)))
      (if entry
          (push entry list)
        (periphery-quick:debug-log "Failed to parse line: %s" line))))
  (periphery-quick:debug-log "Parsed %d entries" (length list))
  (if (null list)
      (message "No results found.")
    (condition-case err
        (let* ((choices (seq-map (lambda (item) (car item)) list))
               (choice (completing-read "Select result: " choices))
               (selected (assoc choice list)))
          (periphery-quick:debug-log "Selected: %s" selected)
          (when selected
            (let* ((file-info (split-string (cadr selected) ":"))
                   (file (car file-info))
                   (line (string-to-number (cadr file-info))))
              (periphery-quick:debug-log "Opening file: %s at line %d" file line)
              (periphery-quick:goto-file-line file line))))
      (error (periphery-quick:debug-log "Error in parse: %s" err)
             (message "An error occurred: %s" err)))))


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
  (periphery-quick:debug-log "Parsing line: %s" line)
  (save-match-data
    (if (string-match periphery-quick-regex-parser line)
        (let* ((file (match-string 1 line))
               (linenumber (or (match-string 2 line) "1"))
               (column (or (match-string 3 line) "1"))
               (text (string-trim-left (or (match-string 4 line) "")))
               (fileWithLine (format "%s:%s:%s" file linenumber column))
               (display (format "%s %s"
                                (propertize
                                 (add-right-padding-up-to
                                  (file-name-sans-extension
                                   (file-name-nondirectory file))
                                  24)
                                 'face 'periphery-filename-face)
                                text)))
          (periphery-quick:debug-log "Parsed line: %s" display)
          (list display fileWithLine))
      (periphery-quick:debug-log "Failed to match line")
      nil)))

(defun periphery-quick:goto-file-line (file line)
  "Go to LINE in FILE, handling the case where FILE is the current buffer."
  (let* ((current-file (buffer-file-name))
         (project-root (periphery-helper:project-root-dir))
         (full-file-path (expand-file-name file project-root)))
    (periphery-quick:debug-log "Full file path: %s" full-file-path)
    (if (and current-file (file-equal-p full-file-path current-file))
        (progn
          (periphery-quick:debug-log "Navigating within current file to line %d" line)
          (goto-char (point-min))
          (forward-line (1- line)))
      (if (file-exists-p full-file-path)
          (progn
            (find-file full-file-path)
            (goto-char (point-min))
            (forward-line (1- line)))
        (message "File not found: %s" full-file-path)))))

;;;###autoload
(defun periphery-quick:run-query (query)
  "Run query (as QUERY)."
  (periphery-quick:debug-log "Running query: %s" query)
  (let ((default-directory (periphery-helper:project-root-dir)))
    (periphery-quick:debug-log "Default directory: %s" default-directory)
    (async-start-command-to-string
     :command (format "rg -wse '%s' --color=never --no-heading --with-filename --line-number --column --sort path" query)
     :callback (lambda (output)
                 (periphery-quick:debug-log "Query output received, length: %d" (length output))
                 (periphery-quick:parse output)))))

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
    (if-let* ((query (thing-at-point 'symbol)))
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
  (if-let* ((query (thing-at-point 'symbol)))
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


