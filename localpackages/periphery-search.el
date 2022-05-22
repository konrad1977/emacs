;;; Periphery-search --- Package for showing search as result in a tabulated list

;;; Code:
(require 'periphery)

(defconst ag-searcher "ag")
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
  "Search thing at point using (SEARCHER)"
  (setq current-query nil)
  (if (executable-find ag-searcher)
      (progn
        (let* ((word (thing-at-point 'word))
               (default-directory (projectile-project-root)))
          (setq current-query word)
          (async-shell-command-to-string searcher (format "%s --vimgrep %s" searcher word) #'send-search-result-to-periphery)))
  (message (format "Install %s to use this command." searcher))))

(provide 'perihery-search)

