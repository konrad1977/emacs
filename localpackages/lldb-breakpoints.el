;;; lldb-breakpoints.el --- A small package for interacting with lldb-breakpoints -*- lexical-binding: t -*-
;;; Code:

(require 'tabulated-list)
(require 'project)
(require 'lldb-comint)
(require 'ios-simulator)

(defconst lldb-breakpoints-buffer-name "*LLDB-breakpoints*")
(defconst breakpoint-regex-parser "\\(\\/[^:]+\\):\\([0-9]+\\)$"
  "Regex for parsing breakpoints")

(defvar lldb-current-project nil)
(defvar breakpoints '())

(defvar lldb-breakpoint-mode-map nil
  "Keymap for lldb-breakpoints")

(setq lldb-breakpoint-mode-map (make-sparse-keymap))
(define-key lldb-breakpoint-mode-map (kbd "<return>") #'lldb-breakpoints:go-to-breakpoint)

(defun lldb-breakpoints:go-to-breakpoint ()
  "Go to breakpoint."
  (interactive)
  (lldb-breakpoints:open-current-line-with (tabulated-list-get-id)))

(defun lldb-breakpoints:open-current-line-with (data)
  "Data"
  (when data
  (save-match-data
    (and (string-match breakpoint-regex-parser data)
         (when-let ((file (match-string 1 data))
                    (linenumber (string-to-number (match-string 2 data))))
           (with-current-buffer (find-file file)
               (goto-char (point-min))
               (forward-line (1- linenumber))
             ))))))

(define-derived-mode lldb-breakpoint-mode tabulated-list-mode "lldb-breakpoint-mode"
  "Periphery mode.  A mode to show compile errors like Flycheck."
  (setq tabulated-list-format [
                               ("Line" 6 nil)
                               ("File" 100 t)
                               ]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Line" nil))
  ;; (use-local-map lldb-breakpoint-mode-map)
  (tabulated-list-init-header))

(defun lldb-breakpoints:show (list)
  "Show active breakpoints."
  (save-selected-window
    (let* ((buffer (get-buffer-create lldb-breakpoints-buffer-name))
           (window (get-buffer-window buffer)))
      (pop-to-buffer buffer nil)
      (lldb-breakpoint-mode)

      (unless (equal (current-buffer) buffer)
        (select-window window))

      (setq tabulated-list-entries list)
      (tabulated-list-init-header)
      (tabulated-list-print t))))

(defun lldb-breakpoints:extract-info-from-text (text)
  "Extracts file name and line number from breakpoint commands in LLDB text."
  (if (string-empty-p text)
      nil
      (progn
        (setq list '())
        (dolist (line (split-string text "\n"))
          (let* ((regex breakpoint-regex-parser))
            (save-match-data
              (and (string-match regex line))
              (let* ((file (match-string 1 line))
                     (linenumber (string-to-number (match-string 2 line)))
                     (pair (cons file linenumber)))
                (push pair list)))))
        list)))

(defun lldb-breakpoints:read-file-contents (file-path)
  "Read the contents of the file at FILE-PATH and return it as a string."
  (if (file-exists-p file-path)
      (progn
        (with-temp-buffer
          (insert-file-contents file-path)
          (buffer-substring-no-properties (point-min) (point-max))))
    nil))

(defun lldb-breakpoints:check-saved-breakpoints ()
  "Checks saved breakpoints."
  (if-let* ((breakpoint-file (expand-file-name "breakpoints.bp" (lldb-breakpoints:project-root)))
            (content (lldb-breakpoints:read-file-contents breakpoint-file))
            (breakpoint-data (lldb-breakpoints:extract-info-from-text content)))
      (setq breakpoints breakpoint-data)))

(defun lldb-breakpoints:project-root ()
  "Get the ios-project root."
  (unless lldb-current-project
    (setq lldb-current-project (cdr (project-current))))
  lldb-current-project)

(cl-defun lldb-breakpoints:toggle-path-and-number (&key path number)
  "Toggle the presence of (PATH . NUMBER) in breakpoints."
  (let ((pair (cons path number)))
    (if (member pair breakpoints)
        (setq breakpoints (remove pair breakpoints))
      (setq breakpoints (cons pair breakpoints)))))

(defun lldb-breakpoints:clear-all ()
  "Remove all breakpoints from list."
  (interactive)
  (setq breakpoints '())
  (lldb-breakpoints:generate-breakpoint-file (lldb-breakpoints:project-root)))

(defun lldb-breakpoints:show-current-breakpoints ()
  "Show currenet breakpoints for project."
  (interactive)
  (lldb-breakpoints:check-saved-breakpoints)
  (lldb-breakpoints:update-breakpoints))

(defun lldb-breakpoints:toggle-show-breakpoints ()
  "Toggle visible state of breakpoints."
  (interactive)
  (let ((buffer (get-buffer lldb-breakpoints-buffer-name)))
    (if (not buffer)
        (lldb-breakpoints:show-current-breakpoints)
      (if (get-buffer-window buffer)
          (delete-window (get-buffer-window buffer))
        (display-buffer buffer)))))

(defun lldb-breakpoints:toggle-breakpoint ()
  "Add a breakpoint at location."
  (interactive)
  (lldb-breakpoints:show-current-breakpoints)
  (lldb-breakpoints:toggle-path-and-number
   :path buffer-file-name
   :number (line-number-at-pos))
  (lldb-breakpoints:update-breakpoints)
  (lldb-breakpoints:generate-breakpoint-file (lldb-breakpoints:project-root)))

(cl-defun lldb-breakpoints:generate-breakpoint-file (path)
  "Generate 'breakpoints.bp' file at (PATH) with the contents of breakpoints."
  (let ((breakpoint-file (expand-file-name "breakpoints.bp" path))
        (formatted-content
         (mapconcat (lambda (item)
                      (format "%s:%d" (car item) (cdr item)))
                    breakpoints
                    "\n")))
    (with-temp-file breakpoint-file
      (insert formatted-content))))

(defun lldb-breakpoints:identifier (text)
  "Parse json from (TEXT)."
  (when-let ((identifier (lldb-breakpoints:parse-json text)))
    (ios-simulator:kill-buffer)

    (ios-simulator:terminate-current-app)

    (ios-simulator:launch-wait-for-debugger
     :identifier identifier)

    (lldb-breakpoints:generate-debug-file)
    (lldb-comint:runWith :path (lldb-breakpoints:project-root)
                         :args '("-s" "lldb.cmd"))
    (lldb-breakpoints:show-current-breakpoints)))

(defun lldb-breakpoints:parse-json (text)
  "Parse json from (TEXT)."
  (condition-case err
      (let ((json-output (json-read-from-string text)))
        (let-alist (seq-elt json-output 0)
          (message "%s" .buildSettings.PRODUCT_BUNDLE_IDENTIFIER)))
    (json-error
     (message "Error parsing JSON: %S" err))))

(defun lldb-breakpoints:app-identifier ()
  "Get the identifier."
  (interactive)
  (let ((default-directory (lldb-breakpoints:project-root)))
    (async-shell-command-to-string
     :process-name "AppIdentifier Fetcher"
     :command (concat "xcrun xcodebuild -showBuildSettings -configuration Debug -json" " 2>/dev/null")
     :callback 'lldb-breakpoints:identifier)))

(defun lldb-breakpoints:debug-ios-simulator-app ()
  "Debug ios app using LLDB."
  (interactive)
  (let ((default-directory (lldb-breakpoints:project-root)))
    (async-shell-command-to-string
     :process-name "AppIdentifier Fetcher"
     :command (concat "xcrun xcodebuild -showBuildSettings -configuration Debug -json" " 2>/dev/null")
     :callback 'lldb-breakpoints:identifier)))

(cl-defun lldb-breakpoints:generate-debug-file ()
   "Generate lldb.cmd file in the root of the project and add text to it."
  (let ((lldb-cmd-file (expand-file-name "lldb.cmd" (lldb-breakpoints:project-root)))
        (simulator (ios-simulator:load-simulator-id)))
    (with-temp-buffer
      (insert (format "attach -n '%s'\n" (ios-simulator:app-name-from :folder (swift-additions:get-build-folder))))
      (dolist (item breakpoints)
        (let ((path (file-name-nondirectory (car item)))
              (linenumber (cdr item)))
            (insert (format "breakpoint set -f %s -l%d \n" path linenumber))))
      (insert "process continue")
      (write-file lldb-cmd-file))))

(defun lldb-breakpoints:update-breakpoints ()
  "Update breakpoints and show them."
  (interactive)
  (setq list '())
  (dolist (item breakpoints)
    (let ((path (car item))
          (linenumber (cdr item)))
      (setq item (list (format "%s:%d" path linenumber)
                       (vector (propertize (format "%d" linenumber) 'face 'line-number)
                               (propertize (file-name-sans-extension (file-name-nondirectory path)) 'face 'link)
                               )))
      (push item list)))
  (lldb-breakpoints:show list))

(provide 'lldb-breakpoints)
;;; lldb-breakpoints.el ends here
