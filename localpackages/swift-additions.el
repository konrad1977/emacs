;;; swift-additions.el --- Package for compiling and running swift apps in Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Package for building and runnning iOS/macos apps from Emacs

;;; Code:

(require 'ansi-color)
(require 'dash)
(require 'cl-lib)
(require 'flycheck)
(require 'swift-mode)
(require 'evil-states)

(require 'periphery)
(load "periphery")

(defgroup swift-additions:xcodebuild nil
  "REPL."
  :tag "swift-additions:xcodebuild"
  :group 'swift-additions)

(defconst xcodebuild-buffer "*xcodebuild*")
(defconst periphery-command "periphery scan")
(defconst notifier-command "terminal-notifier -sender \"org.gnu.Emacs\" -ignoreDnd")
(defconst build-info-command "xcrun xcodebuild -list -json")
(defconst list-simulators-command "xcrun simctl list devices -j")
(defconst get-booted-simulator-command
  "xcrun simctl list devices | grep -m 1 \"(Booted)\" | grep -E -o -i \"([0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12})\""
  "Get booted simulator id if any.")

(defvar current-xcode-scheme nil)
(defvar current-app-identifier nil)
(defvar current-project-root nil)
(defvar current-build-configuration nil)
(defvar current-environment-x86 t)
(defvar current-simulator-id nil)
(defvar secondary-simulator-id nil)
(defvar current-simulator-name nil)
(defvar asked-to-use-secondary-simulator nil)
(defvar local-device-id nil)

(defun get-booted-simulator ()
  "Get booted simulator if any."
  (let ((device-id (shell-command-to-string get-booted-simulator-command)))
    (if (not (string= "" device-id))
        (clean-up-newlines device-id)
      nil)))

(defun command-string-to-list (cmd)
  "Split the CMD unless it is a list.  This function respects quotes."
  (if (listp cmd) cmd (split-string-and-unquote cmd)))

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
      (error "%s: %s %s" "Cannot invoke executable" executable (buffer-string) default-directory))
    (goto-char (point-min))
    (json-read)))

(defun start-simulator-with-id (id)
  "Launch a specific simulator with (as ID)."
  (call-process-shell-command (format "open --background -a simulator --args -CurrentDeviceUDID %s" id)))

(defun boot-simuator-with-id (id)
  "Simulator app is running.  Boot simulator (as ID)."
  (call-process-shell-command (format "xcrun simctl boot %s" id)))

(defun is-simulator-app-running ()
  "Check if simulator is running."
  (let ((output (shell-command-to-string "ps ax | grep -v grep | grep Simulator.app")))
    (not (string= "" output))))

(defun get-simulator-name (id)
  "Get simulator name (as ID)."
  (clean-up-newlines
   (shell-command-to-string (format "xcrun simctl list devices | grep %s | awk -F \"(\" '{ print $1 }'" id))))

(defun setup-simulator-dwim (id)
  "Setup simulator dwim (as ID)."
  (if (not (is-simulator-app-running))
      (start-simulator-with-id id)
    (boot-simuator-with-id id)))

(defun fetch-simulator-name ()
  "Fetches simulator name."
  (unless current-simulator-name
    (let ((simulator-name (get-simulator-name current-simulator-id)))
      (if simulator-name
          (setq current-simulator-name (format "%s(simulator)" simulator-name))
        (setq current-simulator-name "Simulator (unknown)"))))
    current-simulator-name)

(defun fetch-targets ()
  "Select the target."
  (build-menu "Choose target" (swift-additions:get-target-list)))

(defun fetch-or-load-xcode-scheme ()
  "Get the xcode scheme if set otherwuse prompt user."
  (unless current-xcode-scheme
    (setq current-xcode-scheme (build-menu "Choose a scheme" (swift-additions:get-scheme-list))))
  current-xcode-scheme)

(defun fetch-or-load-build-configuration ()
  "Get the build configuration or promp user."
  (unless current-build-configuration
    (setq current-build-configuration (build-menu "Choose a configuration" (swift-additions:get-configuration-list))))
  current-build-configuration)
    
(defun fetch-or-load-app-identifier ()
  "Get the app identifier for the current configiration."
  (unless current-app-identifier
    (setq current-app-identifier (swift-additions:get-bundle-identifier (fetch-or-load-build-configuration))))
  current-app-identifier)
  
(defun fetch-or-load-simulator-id ()
  "Get the booted simulator id or fetch a suiting one."

  (if (not asked-to-use-secondary-simulator)
      (if (yes-or-no-p "Launch an additional simulator?")
          (let ((another-simulator-id (widget-choose "Choose secondrary simulator" (swift-additions:list-available-simulators))))
            (setup-simulator-dwim another-simulator-id)
            (setq secondary-simulator-id another-simulator-id))))
  (setq asked-to-use-secondary-simulator t)

  (if current-simulator-id
      (setup-simulator-dwim current-simulator-id)
    (progn
      (let ((device-id
             (or (get-booted-simulator)
                 (widget-choose "Choose a simulator:" (swift-additions:list-available-simulators)))))
        (progn
          (setup-simulator-dwim current-simulator-id)
          (setq current-simulator-id device-id)))))
  current-simulator-id)

(defun setup-current-project (project)
  "Check if we have a new project (as PROJECT).  If true reset settings."
  (unless current-project-root
    (setq current-project-root project))
  (if (not
       (string= current-project-root project))
       (progn
         (swift-additions:reset-settings)
         (setq current-project-root project))))

(defun xcodebuild-command ()
  "Use x86 environement."
  (if current-environment-x86
   "env /usr/bin/arch -x86_64 xcrun xcodebuild \\"
    "xcrun xcodebuild \\"))

(defun build-folder ()
  "Fetch build folder."
  (let ((config (fetch-or-load-build-configuration)))
    (if local-device-id
      (format "build/Build/Products/%s-iphoneos/" config)
      (format "build/Build/Products/%s-iphonesimulator/" config))))

(defun number-of-available-cores ()
  "Fetch number of available cores."
  (if-let ((cores
         (replace-regexp-in-string "\n$" ""
          (shell-command-to-string "sysctl -n hw.ncpu"))))
        cores)
      1)

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
   (xcodebuild-command)
   (format "-scheme %s \\" (fetch-or-load-xcode-scheme))
   (get-workspace-or-project)
   (format "-configuration %s \\" (fetch-or-load-build-configuration))
   (format "-jobs %s \\" (number-of-available-cores))
   (format "-sdk %s \\" (current-sdk))
   "-parallelizeTargets \\"
   ;; "-showBuildTimingSummary \\"
   (if (not local-device-id)
       (format "-destination 'platform=iOS Simulator,id=%s' \\" simulator-id))
   "-derivedDataPath \\"
   "build"))

(defun swift-additions:get-app-name (directory)
  "Get compiled app name from (DIRECTORY)."
  (when-let (binary-name (directory-files directory nil "\\.app$"))
    (file-name-sans-extension (car binary-name))))

(defun swift-additions:install-and-run-simulator-command (simulator-id)
  "Install and launch app (SIMULATOR-ID)."
  (message "launching %s" simulator-id)
  (let ((folder (build-folder)))
    (concat
     "env /usr/bin/arch -x86_64 \\"
     (format "xcrun simctl install %s %s%s.app\n" simulator-id folder (swift-additions:get-app-name folder))
     (format "xcrun simctl launch %s %s" simulator-id (fetch-or-load-app-identifier)))))

(defun swift-additions:terminate-all-running-apps ()
    "Terminate runnings apps."
    (interactive)
    (swift-additions:terminate-app-in-simulator current-simulator-id)
    (swift-additions:terminate-app-in-simulator secondary-simulator-id))

(defun swift-additions:terminate-app-in-simulator (simulator-id)
  "Terminate app that is running in simulator with SIMULATOR-ID."
  (if simulator-id
   (progn
     (message-with-color :tag "[Terminating]" :text current-xcode-scheme :attributes '(:inherit error))
     (shell-command
      (concat
       (format "xcrun simctl terminate %s %s" simulator-id (fetch-or-load-app-identifier)))))))

(defun get-index-store-path ()
  "Get the index store path."
  (let ((index-store-path (concat current-project-root "build/Index/DataStore")))
        (if (file-directory-p index-store-path)
            index-store-path
          nil)))

(cl-defun parse-errors-from (&key text)
  "Parse errors from TEXT."
  (message-with-color :tag "[Error]" :text "Build failed." :attributes '(:inherit error))
  (periphery-run-parser text))

(defun run-app ()
  "Run app.  Either in simulator or on physical device."
  (setq local-device-id (get-connected-device-id))
  (if local-device-id
      (install-app-on-device)
    (install-app-in-simulator)))
 
(defun run-parser (text)
  "Run periphery parser on TEXT."
  (if (or
       (string-match-p (regexp-quote "BUILD FAILED") text)
       (string-match-p (regexp-quote "error:") text)
       (string-match-p (regexp-quote "warning:") text))
      (progn
        (parse-errors-from :text text)
        (when (not (string-match-p (regexp-quote "BUILD FAILED") text))
          (run-app)))
    (run-app)))
       

(defun swift-additions:analyze-using-periphery ()
  "Analyze code base using periphery."
  (interactive)
  (setup-current-project (get-ios-project-root))
  (if (executable-find "periphery")
      (progn
        (let* ((default-directory current-project-root)
               (index-store-path (get-index-store-path))
               (command
                (concat
                 (format "%s \\" periphery-command)
                 (format "-%s" (get-workspace-or-project))
                 (format "--schemes %s \\" (fetch-or-load-xcode-scheme))
                 (format "--targets %s \\" (fetch-targets))
                 " --quiet \\"
                 (if index-store-path
                     (format "--index-store-path %s --skip-build" (get-index-store-path))))))
          (async-shell-command-to-string "periphery" command #'periphery-run-parser))
        (message-with-color :tag "[Analysing]" :text "Code base using \'periphery\'." :attributes '(:inherit warning)))
    (message-with-color :tag "[Missing binary]" :text "Periphery is not install. Run 'brew install periphery'" :attributes '(:inherit warning))))

(defun swift-additions:simulator-log-command ()
    "Command to filter and log the simulator."
    (concat "xcrun simctl spawn booted log stream "
            "--level error "
            "--style compact "
            "--color always "
            "| grep -Ei "
            "\'[Cc]onstraint|%s\'" current-xcode-scheme))

(defun swift-additions:run-async-command-in-xcodebuild-buffer (command)
  "Run async-command in xcodebuild buffer (as COMMAND)."
  (async-shell-command command xcodebuild-buffer)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun filename-by-extension (extension)
  "Get filename based on (as EXTENSION)."
  (let ((name (directory-files current-project-root t extension)))
    (if name
        (file-name-sans-extension (file-name-nondirectory (car name)))
      nil)))

(defun project-name ()
  "Get project name."
  (filename-by-extension ".xcodeproj"))

(defun workspace-name ()
  "Get workspace name."
  (filename-by-extension ".xcworkspace"))

(defun clean-up-newlines (text)
  "Clean up new lines (as TEXT)."
  (string-trim-left
   (replace-regexp-in-string "\n$" "" text)))

(cl-defun get-files-from (&key directory &key extension &key exclude)
  "Get files from DIRECTORY by EXTENSION and EXCLUDE."
  (let ((result '()))
    (mapcar (lambda (x)
              (cond
               ((not (string-match-p exclude (expand-file-name x directory)))
                (push x result))))
            (directory-files-recursively directory (format "\\%s$" extension) 't))
    result))

(cl-defun find-project-root-folder-with (&key extension)
  "Find project folder where it has its project files EXTENSION."
  (let* ((project-root (expand-file-name (vc-root-dir)))
         (root (directory-files project-root nil (format "\\%s$" extension)))
         (subroot (get-files-from :directory project-root :extension extension :exclude "build"))
         (workroot (or root subroot))
         (path (file-name-directory (car-safe workroot))))
    (if (and path (string-match-p (regexp-quote ".xcodeproj") path))
        (file-name-directory (directory-file-name path))
      path)))

(defun get-ios-project-root ()
  "Get the current root of the project."
  (let* ((workspace (find-project-root-folder-with :extension ".xcworkspace"))
         (xcodeproj (find-project-root-folder-with :extension ".xcodeproj")))
    (or workspace xcodeproj (expand-file-name (vc-root-dir)))))

(cl-defun show-notification (&key title &key message)
  "Show notification (as TITLE as MESSAGE)."
  (shell-command (format "%s -title \"%s\" -message \"%s\"" notifier-command title message)))

(defun get-connected-device-id ()
  "Get the id of the connected device."
  (let ((device-id
         (clean-up-newlines
          (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
    (if (= (length device-id) 0)
        nil
      device-id)))

(defun current-sdk ()
  "Return the current SDK."
  (if local-device-id
      "iphoneos"
    "iphonesimulator"))

(defun install-app-on-device ()
  "Install an app on device."
  (let* ((app-name (swift-additions:get-app-name (build-folder)))
         (default-directory (concat current-project-root (build-folder))))
    (message default-directory)
    (message-with-color :tag "[Installing]" :text (format "%s onto physical device. Will launch app when done." app-name) :attributes '(:inherit success))
    (swift-additions:run-async-command-in-xcodebuild-buffer (format "ios-deploy -b %s.app -d" app-name))))

(defun install-app-in-simulator ()
  "Install the app in the simulator."
  (let* ((default-directory current-project-root)
         (simulator-id (fetch-or-load-simulator-id)))
    
    (swift-additions:terminate-app-in-simulator simulator-id)
    
    (message-with-color :tag "[Installing]" :text (format "%s onto %s. Will launch app when done." (swift-additions:get-app-name (build-folder)) (fetch-simulator-name)) :attributes '(:inherit success))
    (call-process-shell-command (swift-additions:install-and-run-simulator-command simulator-id))
    
    (let ((secondary-id secondary-simulator-id))
      (setup-simulator-dwim secondary-simulator-id)
      (call-process-shell-command (swift-additions:install-and-run-simulator-command secondary-id)))
    (swift-additions:run-async-command-in-xcodebuild-buffer (swift-additions:simulator-log-command))))

(defun check-for-errors (process signal)
  "Launching ios-deploy and install app when done building (as PROCESS SIGNAL)."
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer (get-buffer-create xcodebuild-buffer)
      (run-parser (buffer-string))))
    (shell-command-sentinel process signal))

(defun build-using-compilation-mode ()
  "Build using builtin compile and 'compilation-mode'."
  (interactive)
  (let* ((default-directory (current-project-root))
         (compile-command (build-app-command (fetch-or-load-simulator-id))))
    (compile compile-command)))

(defun swift-additions:reset-settings ()
  "Reset current settings.  Change current configuration."
  (interactive)
  (setq current-xcode-scheme nil)
  (setq current-app-identifier nil)
  (setq current-project-root nil)
  (setq current-build-configuration nil)
  (setq current-simulator-id nil)
  (setq current-simulator-name nil)
  (message-with-color :tag "[Resetting]" :text "Build configiration" :attributes 'warning))

(defun setup-default-buffer-state ()
  "Setup buffer default state."
  (setq-local buffer-read-only nil)
  (erase-buffer))

(defun reset-default-buffer-state ()
  "Reset buffer default state."
  (setq-local buffer-read-only t))

(defun current-device-type ()
  "Function that check we should run on simulator or device."
  (if local-device-id
      "physical device"
    (fetch-simulator-name)))

(defun swift-additions:build-and-run-ios-app ()
  "Build project using xcodebuild and then run iOS simulator."
  (interactive)
  (save-some-buffers t)
  (setup-current-project (get-ios-project-root))
  (setq local-device-id (get-connected-device-id))
  
  (swift-additions:terminate-app-in-simulator current-simulator-id)

  (if (get-buffer-process xcodebuild-buffer)
      (delete-process xcodebuild-buffer))
  
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (setup-default-buffer-state)
    (let* ((default-directory current-project-root)
           (proc (progn
                   (if local-device-id
                       (async-shell-command (concat (build-app-command local-device-id) " | xcbeautify") xcodebuild-buffer)
                     (async-shell-command (concat (build-app-command (fetch-or-load-simulator-id)) " | xcbeautify") xcodebuild-buffer))
                   (progn
                     (compilation-minor-mode)
                     (reset-default-buffer-state)
                     (get-buffer-process xcodebuild-buffer)))))
      (if (process-live-p proc)
          (set-process-sentinel proc #'check-for-errors)))
   (message-with-color :tag "[Build/Install]" :text (format "%s on %s" current-xcode-scheme (current-device-type)) :attributes 'warning)))

(defun swift-additions:compile-and-run-silent ()
  "Build project using xcodebuild."
  (interactive)
  (save-some-buffers t)
  (periphery-kill-buffer)
  (swift-additions:kill-xcode-buffer)
  (setup-current-project (get-ios-project-root))
  
  (let ((default-directory current-project-root))
    (async-shell-command-to-string "periphery" (build-app-command (fetch-or-load-simulator-id)) #'run-parser))
  (message-with-color :tag "[Building]" :text (format "%s. Please wait. Patience is a virtue!" current-xcode-scheme) :attributes '(:inherit warning)))

(defun swift-additions:build-ios-app ()
  "Build project using xcodebuild."
  (interactive)
  (save-some-buffers t)
  (setup-current-project (get-ios-project-root))
  (swift-additions:terminate-app-in-simulator)
  (if (get-buffer-process xcodebuild-buffer)
      (delete-process xcodebuild-buffer))
  
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (setup-default-buffer-state)
    (message-with-color :tag "[Building]" :text (format "%s. Please wait. Patience is a virtue!" current-xcode-scheme) :attributes '(:inherit warning))
    (let* ((default-directory current-project-root)
           (proc (progn
                   (async-shell-command (build-app-command (fetch-or-load-simulator-id)) xcodebuild-buffer)
                   (compilation-minor-mode)
                   (reset-default-buffer-state)
                   (get-buffer-process xcodebuild-buffer))))
      (if (process-live-p proc)
          (set-process-sentinel proc #'check-for-errors)))))

(defun swift-additions:clean-build-folder ()
  "Clean app build folder."
  (interactive)
  (setup-current-project (get-ios-project-root))
  (message-with-color :tag "[Cleaning]" :text (format "Build folder for %s Standby..." current-xcode-scheme) :attributes '(:inherit warning))
  (let ((default-directory (concat current-project-root "build")))
    (if (file-directory-p default-directory)
        (progn
          (message-with-color :tag "[Removing]" :text (format "Folder for %s" default-directory) :attributes '(:inherit warning))
          (delete-directory default-directory t nil))
          (message-with-color :tag "[Failed]" :text (format "Build folder %s doesn't exist" default-directory) :attributes '(:inherit warning))))
    (message-with-color :tag "[Done]" :text "Ready to rumble." :attributes '(:inherit success)))

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

(defun insert-text-and-go-to-eol (text)
  "Function that that insert (as TEXT) and go to end of line."
  (save-excursion
    (indent-for-tab-command)
    (insert text)
    (move-end-of-line nil))
  (goto-char (point-at-eol))
  (evil-insert-state t))

(defun swift-additions:insert-mark ()
  "Insert a mark at line."
  (interactive)
    (insert-text-and-go-to-eol "// MARK: - "))

(defun swift-additions:insert-todo ()
  "Insert a Todo."
  (interactive)
  (insert-text-and-go-to-eol "// TODO:"))

(defun swift-additions:toggle-xcodebuild-buffer ()
  "Function to toggle xcodebuild-buffer."
  (interactive)
  (if (get-buffer xcodebuild-buffer)
      (bury-buffer xcodebuild-buffer)
      (pop-to-buffer xcodebuild-buffer)))

(defun swift-additions:get-bundle-identifier (config)
  "Get bundle identifier (as CONFIG)."
  (unless current-project-root
    (setq current-project-root (get-ios-project-root)))
  
  (let* ((default-directory current-project-root)
         (json (call-process-to-json "xcrun" "xcodebuild" "-showBuildSettings" "-configuration" config "-json")))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_BUNDLE_IDENTIFIER)))

(defun swift-additions:get-target-list ()
  "Get list of project targets."
  (unless current-project-root
    (setq current-project-root (get-ios-project-root)))

  (message-with-color :tag "[Fetching]" :text "app targets.." :attributes '(:inherit warning))
  
  (let* ((default-directory current-project-root)
         (json (call-process-to-json build-info-command))
         (project (assoc 'project json))
         (targets (cdr (assoc 'targets project))))
    targets))

(defun swift-additions:get-scheme-list ()
  "Get list of project schemes."
  (unless current-project-root
    (setq current-project-root (get-ios-project-root)))

  (message current-project-root)
  (message-with-color :tag "[Fetching]" :text "build schemes.." :attributes '(:inherit warning))
  
  (let* ((default-directory current-project-root)
         (json (call-process-to-json build-info-command))
         (project (assoc 'project json))
         (result (cdr (assoc 'schemes project))))
    result))

(defun swift-additions:get-configuration-list ()
  "Get list of project configurations."
  (message-with-color :tag "[Fetching]" :text "build configurations.." :attributes '(:inherit warning))

  (unless current-project-root
    (setq current-project-root (get-ios-project-root)))
  (let* ((default-directory current-project-root)
         (json (call-process-to-json build-info-command))
         (project (assoc 'project json))
         (result (cdr (assoc 'configurations project))))
    result))

(defun swift-additions:list-simulators ()
  "List available simulators."
  (message-with-color :tag "[Fetching]" :text "available simulators..." :attributes '(:inherit warning))
  (let* ((json (call-process-to-json list-simulators-command))
         (devices (cdr (assoc 'devices json)))
         (flattened (apply 'seq-concatenate 'list (seq-map 'cdr devices)))
         (available-devices
          (seq-filter
           (lambda (device) (cdr (assoc 'isAvailable device))) flattened))
         ) available-devices))

(defun swift-additions:list-available-simulators ()
  "List available simulators."
  (let* ((devices (swift-additions:list-simulators))
         (items (seq-map
                 (lambda (device)
                   (cons (cdr (assoc 'name device))
                         (cdr (assoc 'udid device)))) devices)))
    items))

(defun build-menu (title list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) (cons item item)) list)))
        (pcase (length list)
          (1 (car list))
          (0 nil)
          (_ (widget-choose title choices)))))))

(defun swift-additions:kill-xcode-buffer ()
  "Kill the xcode buffer."
  (when (get-buffer xcodebuild-buffer)
    (kill-buffer xcodebuild-buffer)))

(cl-defun message-with-color (&key tag &key text &key attributes)
  "Print a TAG and TEXT with ATTRIBUTES."
  (interactive)
  (setq-local inhibit-message nil)
  (message "%s %s" (propertize tag 'face attributes) text)
  (setq-local inhibit-message t))

; Taken from  https://gitlab.com/woolsweater/dotemacs.d/-/blob/main/modules/my-swift-mode.el
(defun swift-additions:split-func-list ()
  "While on either the header of a function-like declaration or a
call to a function, split each parameter/argument to its own
line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (condition-case nil
        (atomic-change-group
          (search-forward "(")
          (let ((end))
            (while (not end)
              (newline-and-indent)
              (let ((parens 0)
                    (angles 0)
                    (squares 0)
                    (curlies 0)
                    (comma))
                (while (not (or comma end))
                  (re-search-forward
                   (rx (or ?\( ?\) ?< ?> ?\[ ?\] ?{ ?} ?\" ?,))
                   (line-end-position))
                  (pcase (match-string 0)
                    ("(" (cl-incf parens))
                    (")" (if (> parens 0)
                             (cl-decf parens)
                           (backward-char)
                           (newline-and-indent)
                           (setq end t)))
                    ;; Note; these could be operators in an expression;
                    ;; there's no obvious way to fully handle that.
                    ("<" (cl-incf angles))
                    ;; At a minimum we can skip greater-than and func arrows
                    (">" (unless (zerop angles)
                           (cl-decf angles)))
                    ("[" (cl-incf squares))
                    ("]" (cl-decf squares))
                    ("{" (cl-incf curlies))
                    ("}" (cl-decf curlies))
                    ("\"" (let ((string-end))
                            (while (not string-end)
                              (re-search-forward (rx (or ?\" (seq ?\\ ?\")))
                                                 (line-end-position))
                              (setq string-end (equal (match-string 0) "\"")))))
                    ("," (when (and (zerop parens) (zerop angles)
                                    (zerop squares) (zerop curlies))
                           (setq comma t)))))))))
      (error (user-error "Cannot parse function decl or call here")))))

(provide 'swift-additions)

;;; swift-additions.el ends here


