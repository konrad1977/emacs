;;; periphery-helper.el --- Process and text helping package ;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Utility package for handling xcode-projects and json data.

;;; Code:

(require 'async)
(require 'json)
(require 'project)
(require 'cl-lib)

(defconst periphery-parse-line-regex "^\\([^:]+\\):\\([0-9]+\\)?:\\(\\([0-9]+\\)\\)?"
   "Parse linenumber and columns.")

;;;###autoload
(defun periphery-helper:project-root-dir ()
  "Get the root directory of the current project."
  (let ((project (project-current)))
    (when project
      (project-root project))))


(defun periphery-helper:filter-xcworkspace (lst)
  "Filter out '.xcworkspace' paths that are inside '.xcodeproj' (as LST)."
  (cl-remove-if (lambda (path)
                  (and (string-match-p "\.xcworkspace$" path)
                       (string-match-p "\.xcodeproj/" path)))
                lst))


(cl-defun async-start-shell-command-to-json (&key command &key callback)
  "Async shell command to JSON run async (as COMMAND CALLBACK)."
  (async-start-command-to-string
   :command command
   :callback (lambda (result)
               (let* ((json-object (json-read-from-string result)))
                 (funcall ,callback json-object)))))

(cl-defun async-start-command-to-string (&key command &key callback)
  "Async shell command to JSON run async (as COMMAND) and parse it json and call (as CALLBACK)."
  (async-start
   `(lambda ()
      (shell-command-to-string ,command))
   `(lambda (result)
      (funcall ,callback result))))

(cl-defun async-start-command (&key command &key callback)
  "Async shell command run async (as COMMAND) and call (as CALLBACK)."
  (async-start
   `(lambda ()
      (shell-command ,command))
   `(lambda (result)
      (funcall ,callback))))

(defun clean-up-newlines (text)
  "Clean up new lines (as TEXT)."
  (string-trim-left
   (replace-regexp-in-string "\n" "" text)))

(cl-defun message-with-color (&key tag &key text &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES."
  (message "%s %s" (propertize tag 'face attributes) text))

;;; Processes
(defun command-string-to-list (cmd)
  "Split the CMD unless it is a list.  This function respects quotes."
  (if (listp cmd) cmd (split-string-and-unquote cmd)))

(cl-defun run-async-command-in-buffer (&key command &key buffername)
  "Run async-command in xcodebuild buffer (as COMMAND and BUFFERNAME)."
  (inhibit-sentinel-messages #'async-shell-command command buffername))

(defun inhibit-sentinel-messages (fun &rest args)
  "Inhibit messages in all sentinels started by FUN and ARGS."
  (cl-letf* ((old-set-process-sentinel (symbol-function 'set-process-sentinel))
         ((symbol-function 'set-process-sentinel)
          (lambda (process sentinel)
        (funcall
         old-set-process-sentinel
         process
         `(lambda (&rest args)
            (cl-letf (((symbol-function 'message) #'ignore))
              (apply (quote ,sentinel) args)))))))
    (apply fun args)))

(defun periphery-helper:filter-keep-beginning-paths (text)
  "Filter lines starting with '/' from TEXT."
  (with-temp-buffer
    (insert text)
    (keep-lines "^/")
    (buffer-string)))

(defun open-current-line-with (data)
  "Open current line with DATA."
  (when data
  (save-match-data
    (and (string-match periphery-parse-line-regex data)
         (when-let ((file (match-string 1 data))
                    (linenumber (string-to-number (match-string 2 data)))
                    (column (match-string 3 data)))
         (with-current-buffer (find-file file)
             (when (> linenumber 0)
               (goto-char (point-min))
               (forward-line (1- linenumber))
               (if-let ((columnnumber (string-to-number column)))
                   (when (> columnnumber 0)
                     (forward-char (1- columnnumber)))))))))))

(cl-defun async-shell-command-to-string (&key process-name &key command &key callback)
  "Execute shell command COMMAND asynchronously in the background.
PROCESS-NAME is the name of the process."
  (let* ((output-buffer (generate-new-buffer process-name))
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

(defun do-call-process (executable infile destination display args)
  "Wrapper for `call-process'.

EXECUTABLE may be a string or a list.  The string is split by spaces,
then unquoted.
For INFILE, DESTINATION, DISPLAY, see `call-process'.
ARGS are rest arguments, appended to the argument list.
Returns the exit status."
  (let ((command-list
         (append (command-string-to-list executable) args)))
    (apply 'call-process
           (append
            (list (car command-list))
            (list infile destination display)
            (cdr command-list)))))

(defun call-process-to-json (executable &rest args)
  "Call EXECUTABLE synchronously in separate process.

The output is parsed as a JSON document.
EXECUTABLE may be a string or a list.  The string is split by spaces,
then unquoted.
ARGS are rest arguments, appended to the argument list."
  (with-temp-buffer
    (unless (zerop
             (do-call-process executable
                              nil
                              ;; Disregard stderr output, as it
                              ;; corrupts JSON.
                              (list t nil)
                              nil
                              args))
      (error "%s: %s %s %s" "Cannot invoke executable" executable (buffer-string) default-directory))
    (goto-char (point-min))
    (json-read)))

(provide 'periphery-helper)

;;; periphery-helper.el ends here
