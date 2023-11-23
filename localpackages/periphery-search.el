;;; periphery-search.el --- Search using Ag/Rg and show the result as flycheck list.  -*- lexical-binding: t; -*-
;;; Commentary: Package for showing search as result in a tabulated list

;;; Code:

(require 'periphery-helper)
(require 'periphery)
(require 'thingatpt)

(defvar current-query "")
(defvar current-title "Search")

(defun send-search-result-to-periphery (text)
  "Send result (as TEXT) to periphery."
  (periphery-parse-search-result :title current-title :text text :query current-query))

(defun setup-search-title ()
  "Default search title."
  (setq current-title "Search"))

(defun periphery--search-thing-at-point ()
  "Search thing at point."
  (let ((extension (file-name-extension (buffer-file-name))))
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (when (> (length text) 1)
            (periphery-run-query (format "rg -g '*.%s' -s -e" extension) (escape-string text))))
          (periphery-run-query (format "rg -g '*.%s' -w -s -e" extension) (thing-at-point 'symbol)))))

(defun escape-string (text)
  "Escape TEXT."
  (setq str (replace-regexp-in-string "\{" "\\\\{" text))
  (setq str (replace-regexp-in-string "\}" "\\\\}" str))
  (setq str (replace-regexp-in-string "\(" "\\\\(" str))
  (setq str (replace-regexp-in-string "\)" "\\\\)" str))
  str)

(defun periphery-run-query (searcher text)
  "Search using (SEARCHER) with (TEXT)."
  (setq current-query nil)
  (if (executable-find "rg")
      (progn
        (let ((default-directory (periphery-helper:project-root-dir)))
          (setq current-query (regexp-quote text))
          (message-with-color :tag "[SEARCHING]" :text (format "for %s" current-query) :attributes 'warning)
          (async-start-command-to-string
           :command (format "%s \"%s\" --color=never --no-heading --with-filename --line-number --column --sort path" searcher current-query)
           :callback '(lambda (result) (send-search-result-to-periphery result)))))
    (message-with-color :tag "[FAILED]" :text (format "Install %s to use this command." searcher) :attributes 'warning)))

(defun periphery--search-for (searcher)
  "Search using (as SEARCHER)."
  (setup-search-title)
  (periphery-run-query searcher (read-string "Query: ")))

;;;###autoload
(defun periphery-search-rg ()
  "Search using RG (Ripgrep)."
  (interactive)
  (setup-search-title)
  (periphery--search-for "rg -wS"))

;;;###autoload
(defun periphery-query-todos-and-fixmes ()
  "Query todos and fixmes in the project."
  (interactive)
  (setq current-title "Fixme and todos")
  (periphery-run-query "rg -e" "(NOTE|FIX|FIXME|TODO|HACK|PERF):"))

;;;###autoload
(defun periphery-query-marks ()
  "Query mark in the project."
  (interactive)
  (setq current-title "Marks")
  (periphery-run-query "rg -w" "\'MARK'"))

;;;###autoload
(defun periphery-search-dwiw-rg ()
  "Search using rg (ripgrep)."
  (interactive)
  (setup-search-title)
  (periphery--search-thing-at-point))

(provide 'periphery-search)
;;; periphery-search.el ends here.

