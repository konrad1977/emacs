;;; Periphery-search --- Search using Ag/Rg and show the result as flycheck list.  -*- lexical-binding: t; -*-

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

(defun periphery-search-dwiw-ag ()
  "Search using ag (Silver searcher)."
  (interactive)
  (setup-search-title)
  (periphery--search-thing-at-point "ag"))

(defun periphery-search-dwiw-rg ()
  "Search using rg (ripgrep)."
  (interactive)
  (setup-search-title)
  (periphery--search-thing-at-point "rg"))

(defun periphery--search-thing-at-point (searcher)
  "Search thing at point using (SEARCHER)."
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (when (> (length text) 1)
          (message text)
          (periphery-run-query searcher text)))
    (periphery-run-query searcher (thing-at-point 'symbol))))
 
(defun periphery-search-rg ()
  "Search using RG (Ripgrep)."
  (interactive)
  (setup-search-title)
  (periphery--search-for "rg"))

(defun periphery-search-ag ()
  "Search using AG (The Silver Searcher)."
  (interactive)
  (setup-search-title)
  (periphery--search-for "ag"))

(defun periphery-run-query (searcher text &optional args)
  "Search using (SEARCHER) with (TEXT)."
  (setq current-query nil)
  (if (executable-find searcher)
      (progn
        (let ((default-directory (vc-root-dir)))
          (setq current-query text)
          (async-shell-command-to-string
           :process-name searcher
           :command (format "%s --vimgrep -w %s" searcher text)
           :callback #'send-search-result-to-periphery)))
    (message-with-color :tag "[Failed]" :text (format "Install %s to use this command." searcher) :attributes 'warning)))

(defun periphery--search-for (searcher)
  "Search using (as SEARCHER)."
  (setup-search-title)
  (periphery-run-query searcher (read-regexp "Query: ")))

(defun periphery-query-todos-and-fixmes ()
  "Query todos and fixmes in the project."
  (interactive)
  (setq current-title "Fixme and todos")
  (periphery-run-query "rg" "\'FIXME:|TODO:\' --sort path"))

(provide 'periphery-search)
;;; periphery-search.el ends here.

