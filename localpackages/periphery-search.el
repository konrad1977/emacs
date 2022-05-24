;;; Periphery-search --- Search using Ag/Rg and show the result as flycheck list.

;;; Commentary: Package for showing search as result in a tabulated list

;;; Code:
(require 'periphery)

(defvar current-query "")

(defun async-shell-command-to-string (process-name command callback)
  "Execute shell command COMMAND asynchronously in the background.
PROCESS-NAME is the name of the process."

  (let ((output-buffer (generate-new-buffer process-name))
        (callback-fun callback))
    (set-process-sentinel
     (start-process process-name output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun send-search-result-to-periphery (text)
  "Send result (as TEXT) to periphery."
   (periphery-parse-search-result text current-query))

(defun periphery-search-thing-at-point-ag ()
  "Search using ag (Silver searcher)."
  (interactive)
  (periphery--search-thing-at-point "ag"))

(defun periphery-search-thing-at-point-rg ()
  "Search using rg (ripgrep)."
  (interactive)
  (periphery--search-thing-at-point "rg"))

(defun periphery--search-thing-at-point (searcher)
  "Search thing at point using (SEARCHER)."
  (periphery-run-query searcher (thing-at-point 'word)))

(defun periphery-search-rg ()
  "Search using RG (Ripgrep)."
  (interactive)
  (periphery--search-for "rg"))

(defun periphery-search-ag ()
  "Search using AG (The Silver Searcher)."
  (interactive)
  (periphery--search-for "ag"))

(defun periphery-run-query (searcher text)
  "Search using (SEARCHER) with (TEXT)."
  (message text)
  (setq current-query nil)
  (if (executable-find searcher)
      (progn
        (let ((default-directory (vc-root-dir)))
          (setq current-query text)
          (async-shell-command-to-string searcher (format "%s --vimgrep -w %s" searcher text) #'send-search-result-to-periphery)))
    (message (format "Install %s to use this command." searcher))))

(defun periphery--search-for (searcher)
  "Search using (as SEARCHER)."
  (periphery-run-query searcher (read-regexp "Type search query: ")))

(provide 'perihery-search)
;;; periphery-search.el ends here.

