;;; swift-additions.el --- Package for compiling and running swift apps in Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Package for building and runnning iOS/macos apps from Emacs

;;; Code:

(require 'ansi-color)
(require 'dash)
(require 'cl-lib)
(require 'projectile)
(require 'flycheck)
(require 'swift-mode)

(defconst xcodebuild-buffer "*xcodebuild*"
  "Xcodebuild buffer.")

(defgroup swift-additions:xcodebuild nil
  "REPL."
  :tag "swift-additions:xcodebuild"
  :group 'swift-additions)

(defcustom xcode-scheme "SecoTools-dev"
  "Current xcode scheme."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(defcustom current-simulator-id "C1278718-C3C4-4AAD-AF0A-A51794D0F6BB"
  "Current simulator ID of choice."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(defcustom local-device-id nil ;"00008110-001E34EC2EE1801E"
  "Local device-id ID of choice."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(defcustom app-identifier "com.secotools.dev"
  "Current app-identifier of choice."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(defcustom build-configuration "Debug"
  "Build name from configuration."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(setq invoked-from-buffer "")


(defun command-string-to-list (cmd)
  "Split the CMD unless it is a list. This function respects quotes."
  (if (listp cmd) cmd (split-string-and-unquote cmd)))

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
      (error "%s: %s" "Cannot invoke executable" (buffer-string)))
    (goto-char (point-min))
    (json-read)))

(defun build-folder ()
  "Fetch build folder."
  (if local-device-id
      "build/Build/Products/Debug-iphoneos/"
    "build/Build/Products/Debug-iphonesimulator/"))

(defun swift-additions:simulator-log-command ()
    "Command to filter and log the simulator."
    (concat "xcrun simctl spawn booted log stream "
            "--level error "
            "--style compact "
            "--color always "
            "| grep -Ei "
            "\'[Cc]onstraint|%s\'" (swift-project-name)))
              
(defun swift-additions:show-ios-simulator-logs ()
  "Show simulator logs in a buffer."
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (async-shell-command (swift-additions:simulator-log-command) xcodebuild-buffer)
    (ansi-color-apply-on-region (point-min) (point-max))
    (auto-revert-tail-mode t)))

(defun swift-additions:run-async-command-in-xcodebuild-buffer (command)
"Run async-command in xcodebuild buffer (as COMMAND)."
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (async-shell-command command xcodebuild-buffer)
    (ansi-color-apply-on-region (point-min) (point-max))
    (auto-revert-tail-mode t)))

(defun swift-additions:find-app ()
  "Find app to install in simulator."
    (car
     (directory-files-recursively
      (projectile-project-root) "\\.app$")))

(defun swift-project-name ()
  "Get workspace name."
  (file-name-sans-extension
   (file-name-nondirectory
    (car
     (directory-files
      (projectile-project-root) t ".xcworkspace")))))

(defun get-connected-device-id ()
  "Get the id of the connected device."
  (let ((device-id
         (replace-regexp-in-string "\n$" ""
          (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
    (if (= (length device-id) 0)
        nil
      device-id)))

(defun current-sdk ()
  "Return the current SDK."
  (if local-device-id
      "iphoneos"
    "iphonesimulator"))

(defun build-app (simulator-id)
  "Xcodebuild with (as SIMULATOR-ID)."
      (concat
       "env /usr/bin/arch -x86_64 \\"
       "xcrun xcodebuild \\"
       (format "-scheme %s \\" xcode-scheme)
       (format "-workspace %s.xcworkspace \\" (swift-project-name))
       (format "-configuration %s \\" build-configuration)
       "-jobs 4 \\"
       (format "-sdk %s \\" (current-sdk))
       (if (not local-device-id)
           (format "-destination 'platform=iOS Simulator,id=%s' \\" simulator-id))
       "-derivedDataPath \\"
       "build | xcpretty \n"))

(defun swift-additions:install-and-run-simulator-command ()
  "Install and launch app."
  (concat
   "env /usr/bin/arch -x86_64 \\"
   (format "xcrun simctl install %s %s%s.app\n" current-simulator-id (build-folder) xcode-scheme)
   (format "xcrun simctl launch %s %s" current-simulator-id app-identifier)))

(defun start-simulator-when-done (process signal)
  "Launching simular when done building (as PROCESS SIGNAL)."
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer (get-buffer-create xcodebuild-buffer)
      (progn
        (if (swift-additions:buffer-contains-substring "BUILD FAILED")
            (progn
              (compilation-mode) 
              (message "Bummer build failed stupid!"))
              )
        (if (swift-additions:buffer-contains-substring "Build Succeeded")
            (let ((default-directory (projectile-project-root)))
              (call-process-shell-command (swift-additions:install-and-run-simulator-command))
              (swift-additions:run-async-command-in-xcodebuild-buffer (swift-additions:simulator-log-command))))))
      (shell-command-sentinel process signal)))

(defun install-and-launch-app-on-local-device-when-done (process signal)
  "Launching simular when done building (as PROCESS SIGNAL)."
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer (get-buffer-create xcodebuild-buffer)
      (progn
        (if (swift-additions:buffer-contains-substring "BUILD FAILED")
            (progn
              (compilation-mode)
              (message "Build failed stupid!"))
              )
        (if (swift-additions:buffer-contains-substring "Build Succeeded")
            (let ((default-directory (concat (projectile-project-root) (build-folder))))
              (swift-additions:run-async-command-in-xcodebuild-buffer (format "ios-deploy -b %s.app -d" xcode-scheme))))))
      (shell-command-sentinel process signal)))

(defun swift-additions:clear-xcodebuild-buffer ()
  "Clear the xcodebuild buffer."
  (interactive)
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (erase-buffer)))

(defun swift-additions:build-and-run-ios-app ()
  "Build project using xcodebuild and then run iOS simulator."
  (interactive)
  (save-some-buffers t)
  (setq local-device-id (get-connected-device-id))
  (setq invoked-from-buffer (current-buffer))
  (swift-additions:terminate-app-in-simulator)

  (if (get-buffer-process xcodebuild-buffer)
      (delete-process xcodebuild-buffer))
  
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq inhibit-message t)
    (let* ((default-directory (projectile-project-root))
           (proc (progn
                   (if local-device-id
                       (async-shell-command (build-app local-device-id) xcodebuild-buffer)
                     (async-shell-command (build-app current-simulator-id) xcodebuild-buffer))
                   (get-buffer-process xcodebuild-buffer))))
      (if (process-live-p proc)
          (progn
            (if local-device-id
                (set-process-sentinel proc #'install-and-launch-app-on-local-device-when-done)
            (set-process-sentinel proc #'start-simulator-when-done)))))
      (message "No process running.")))

(defun swift-additions:build-ios-app ()
  "Build project using xcodebuild."
  (interactive)
  (save-some-buffers t)
  (swift-additions:terminate-app-in-simulator)
  (if (get-buffer-process xcodebuild-buffer)
        (delete-process xcodebuild-buffer))
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (setq buffer-read-only nil)
    (setq inhibit-message t)
    (erase-buffer)
    (pop-to-buffer (current-buffer))
    (compilation-mode)
    (let ((default-directory (projectile-project-root)))
      (async-shell-command (build-app current-simulator-id) xcodebuild-buffer))))

(defun swift-additions:clean-build-folder ()
  "Clean app build folder."
  (interactive)
  (setq inhibit-message nil)
  (message "Cleaning build folder for %s. Standby..." xcode-scheme)
  (let ((default-directory (concat (projectile-project-root) "build")))
    (if (file-directory-p default-directory)
        (progn
          (message "Removing build folder %s" default-directory)
          (delete-directory default-directory t nil))
      (message "Build folder %s doesnt exist" default-directory)))
  (message "Done."))

(defun swift-additions:buffer-contains-substring (string)
  "Check if buffer contain (as STRING)."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun swift-additions:launch-app-in-simulator ()
  "Launch simulator and app."
  (interactive)
  (shell-command
   (concat
    "open -a simulator \n"
    (format "xcrun simctl launch %s %s" current-simulator-id app-identifier))))

(defun swift-additions:terminate-app-in-simulator ()
  "Terminate app."
  (interactive)
  (shell-command
   (concat
    (format "xcrun simctl terminate %s %s" current-simulator-id app-identifier))))

(defun swift-additions:functions-and-pragmas ()
  "Show swift file compressed functions and pragmas."
   (interactive)
  (let ((list-matching-lines-face nil))
    (occur "\\(func\\)\\|\\(#pragma mark\\)\\|\\(MARK:\\)")))

(defun swift-additions:print-thing-at-point ()
  "Print thing at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (end-of-line)
    (newline-and-indent)
    (insert (format "print(\"%s:\ \\(%s\)\")" word word))))

(defun swift-additions:insert-mark ()
  "Insert a mark at line."
  (interactive)
  (insert "// MARK: - ")
  (move-end-of-line nil))

(defun swift-additions:insert-todo ()
  "Insert a Todo."
  (interactive)
  (indent-for-tab-command)
  (insert "// TODO: ")
  (move-end-of-line nil))

(defun mk/toggle-flycheck-errors ()
  "Function to toggle flycheck errors."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (list-flycheck-errors)))

(provide 'swift-additions)

;;; swift-additions.el ends here


