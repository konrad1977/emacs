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

(defvar xcode-scheme nil)

(defcustom current-simulator-id "C1278718-C3C4-4AAD-AF0A-A51794D0F6BB"
  "Current simulator ID of choice."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(defcustom local-device-id nil
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

(defconst build-info-command "xcrun xcodebuild -list -json")

(defun command-string-to-list (cmd)
  "Split the CMD unless it is a list.  This function respects quotes."
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

(defun fetch-or-load-xcode-scheme ()
  "Get the xcode scheme if set otherwuse prompt user."
  (unless xcode-scheme
    (setq xcode-scheme (build-menu "Choose scheme:" (swift-additions:get-scheme-list))))
  xcode-scheme)

(defun build-folder ()
  "Fetch build folder."
  (if local-device-id
      "build/Build/Products/Debug-iphoneos/"
    "build/Build/Products/Debug-iphonesimulator/"))

(defun number-of-available-cores ()
  "Fetch number of available cores."
  (let ((cores
         (replace-regexp-in-string "\n$" ""
          (shell-command-to-string "sysctl -n hw.ncpu"))))
    (if (= (length cores) 0)
        1
      cores)))

(defun get-workspace-or-project ()
  "Check if there is workspace or project."
  (let ((workspace (workspace-name))
        (projectname (project-name)))
    (if workspace
        (format "-workspace %s.xcworkspace \\" workspace)
      (format "-project %s.xcodeproj \\" projectname))))

(defun build-app-command (simulator-id)
  "Xcodebuild with (as SIMULATOR-ID)."
      (concat
       "env /usr/bin/arch -x86_64 \\"
       "xcrun xcodebuild \\"
       (format "-scheme %s \\" (fetch-or-load-xcode-scheme))
       (get-workspace-or-project)
       (format "-configuration %s \\" build-configuration)
       (format "-jobs %s \\" (number-of-available-cores))
       (format "-sdk %s \\" (current-sdk))
       "-parallelizeTargets \\"
       ;; "-showBuildTimingSummary \\"
       (if (not local-device-id)
           (format "-destination 'platform=iOS Simulator,id=%s' \\" simulator-id))
       "-derivedDataPath \\"
       "build"))

(defun swift-additions:install-and-run-simulator-command ()
  "Install and launch app."
  (concat
   "env /usr/bin/arch -x86_64 \\"
   (format "xcrun simctl install %s %s%s.app\n" current-simulator-id (build-folder) (fetch-or-load-xcode-scheme))
   (format "xcrun simctl launch %s %s" current-simulator-id app-identifier)))

(defun swift-additions:terminate-app-in-simulator ()
  "Terminate app that is running in simulator."
  (interactive)
  (shell-command
   (concat
    (format "xcrun simctl terminate %s %s" current-simulator-id app-identifier))))

(defun swift-additions:simulator-log-command ()
    "Command to filter and log the simulator."
    (concat "xcrun simctl spawn booted log stream "
            "--level error "
            "--style compact "
            "--color always "
            "| grep -Ei "
            "\'[Cc]onstraint|%s\'" xcode-scheme))
              
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

(defun filename-by-extension (extension)
  "Get filename based on (as EXTENSION)."
  (let ((name (directory-files (projectile-project-root) t extension)))
    (if name
        (file-name-sans-extension (file-name-nondirectory (car name)))
      nil)))

(defun project-name ()
  "Get project name."
  (filename-by-extension ".xcodeproj"))

(defun workspace-name ()
  "Get workspace name."
  (filename-by-extension ".xcworkspace"))

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

(defun start-simulator-when-done (process signal)
  "Launching simular when done building (as PROCESS SIGNAL)."
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer (get-buffer-create xcodebuild-buffer)
      (progn
        (unless
            (swift-additions:buffer-contains-substring "BUILD FAILED")
          (let ((default-directory (projectile-project-root)))
            (progn
              (call-process-shell-command (swift-additions:install-and-run-simulator-command))
              (swift-additions:run-async-command-in-xcodebuild-buffer (swift-additions:simulator-log-command))
              )
            )
          )
        (first-error)))
    (shell-command-sentinel process signal)))

(defun install-and-launch-app-on-local-device-when-done (process signal)
  "Launching ios-deploy and install app when done building (as PROCESS SIGNAL)."
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer (get-buffer-create xcodebuild-buffer)
      (progn
        (unless
            (swift-additions:buffer-contains-substring "BUILD FAILED")
          (let ((default-directory (concat (projectile-project-root) (build-folder))))
            (swift-additions:run-async-command-in-xcodebuild-buffer (format "ios-deploy -b %s.app -d" (fetch-or-load-xcode-scheme)))))
        (first-error)))
    (shell-command-sentinel process signal)))

(defun check-for-errors (process signal)
  "Launching ios-deploy and install app when done building (as PROCESS SIGNAL)."
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer (get-buffer-create xcodebuild-buffer)
      (if (swift-additions:buffer-contains-substring "BUILD FAILED")
          (first-error)
        (message "Build successful")))
    (shell-command-sentinel process signal)))

(defun build-using-compilation-mode ()
  "Build using builtin compile and 'compilation-mode'."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (compile-command (build-app-command current-simulator-id)))
    (compile compile-command)))

(defun swift-additions:clear-xcodebuild-buffer ()
  "Clear the xcodebuild buffer."
  (interactive)
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (erase-buffer)))

(defun swift-additions:clear-settings ()
  "Clear current settings.  Change current configuration."
  (setq xcode-scheme nil))

(defun swift-additions:build-and-run-ios-app ()
  "Build project using xcodebuild and then run iOS simulator."
  (interactive)
  (save-some-buffers t)
  (setq local-device-id (get-connected-device-id))
  (swift-additions:terminate-app-in-simulator)

  (if (get-buffer-process xcodebuild-buffer)
      (delete-process xcodebuild-buffer))
  
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (setq inhibit-message t)
    (let* ((default-directory (projectile-project-root))
           (proc (progn
                   (if local-device-id
                       (async-shell-command (concat (build-app-command local-device-id) " | xcpretty") xcodebuild-buffer)
                     (async-shell-command (concat (build-app-command current-simulator-id) " | xcpretty") xcodebuild-buffer))
                   (progn
                     (compilation-minor-mode)
                     (setq-local buffer-read-only t)
                     (get-buffer-process xcodebuild-buffer)))))
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
    (setq inhibit-message t)
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (let* ((default-directory (projectile-project-root))
          (proc (progn
                  (async-shell-command (concat (build-app-command current-simulator-id) " | xcpretty") xcodebuild-buffer)
                  (compilation-minor-mode)
                  (setq-local buffer-read-only t)
                  (get-buffer-process xcodebuild-buffer))))
      (if (process-live-p proc)
          (set-process-sentinel proc #'check-for-errors)
        ))))

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
      (goto-char (point-max))
      (search-backward string nil t))))

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

(defun swift-additions:toggle-xcodebuild-buffer ()
  "Function to toggle xcodebuild-buffer."
  (interactive)
  (if (get-buffer xcodebuild-buffer)
      (bury-buffer xcodebuild-buffer)
      (pop-to-buffer xcodebuild-buffer)))

(defun swift-additions:get-bundle-identifier (config)
  "Get bundle identifier (as CONFIG)."
  (let* ((default-directory (projectile-project-root))
         (json (call-process-to-json "xcrun" "xcodebuild" "-showBuildSettings" "-configuration" config "-json")))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_BUNDLE_IDENTIFIER)))

(defun swift-additions:get-target-list ()
  "Get list of project targets."
  (let* ((default-directory (projectile-project-root))
         (json (call-process-to-json build-info-command))
         (project (assoc 'project json))
         (targets (cdr (assoc 'targets project))))
    targets))

(defun swift-additions:get-scheme-list ()
  "Get list of project schemes."
  (let* ((default-directory (projectile-project-root))
         (json (call-process-to-json build-info-command))
         (project (assoc 'project json))
         (result (cdr (assoc 'schemes project))))
    result))

(defun swift-additions:get-configuration-list ()
  "Get list of project configurations."
  (let* ((default-directory (projectile-project-root))
         (json (call-process-to-json build-info-command))
         (project (assoc 'project json))
         (result (cdr (assoc 'configurations project))))
    result))


(defun build-menu (title list)
  "Builds a widget menu from (as TITLE as LIST)."
  (let* ((choices (seq-map (lambda (item) (cons item item)) list)))
    (pcase (length list)
      (1 (car list))
      (0 nil)
      (_ (widget-choose title choices)))))

(provide 'swift-additions)

;;; swift-additions.el ends here


