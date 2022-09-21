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
(defconst build-warning-command "xcrun xcodebuild -list -json")
(defconst list-simulators-command "xcrun simctl list devices iPhone -j")
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
(defvar current-buildconfiguration-json-data nil)
(defvar asked-to-use-secondary-simulator nil)
(defvar local-device-id nil)
(defvar DEBUG nil)

(defun get-booted-simulator ()
  "Get booted simulator if any."
  (let ((device-id (shell-command-to-string get-booted-simulator-command)))
    (if (not (string= "" device-id))
        (clean-up-newlines device-id)
      nil)))

(defun command-string-to-list (cmd)
  "Split the CMD unless it is a list.  This function respects quotes."
  (if (listp cmd) cmd (split-string-and-unquote cmd)))

(cl-defun async-shell-command-to-string (&key process-name &key command &key callback)
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
  (build-menu :title "Choose target" :list (swift-additions:get-target-list)))

(defun fetch-or-load-xcode-scheme ()
  "Get the xcode scheme if set otherwuse prompt user."
  (unless current-xcode-scheme
    (setq current-xcode-scheme (build-menu :title "Choose a scheme" :list (swift-additions:get-scheme-list))))
  current-xcode-scheme)

(defun fetch-or-load-build-configuration ()
  "Get the build configuration or promp user."
  (unless current-build-configuration
    (setq current-build-configuration (build-menu :title "Choose a configuration" :list (swift-additions:get-configuration-list))))
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
   "env /usr/bin/arch -x86_64 xcrun xcodebuild build \\"
    "xcrun xcodebuild build\\"))

(defun get-build-folder ()
  "Fetch build folder."
  (let ((config (fetch-or-load-build-configuration)))
    (if local-device-id
        (format "build/Build/Products/%s-iphoneos/" config)
      (format "build/Build/Products/%s-iphonesimulator/" config))))

(defun get-number-of-cores ()
  "Fetch number of available cores."
  (if-let ((cores (replace-regexp-in-string "\n$" "" (shell-command-to-string "sysctl -n hw.ncpu"))))
      cores
    2))

(defun get-workspace-or-project ()
  "Check if there is workspace or project."
  (let ((workspace (workspace-name))
        (projectname (project-name)))
    (if workspace
        (format "-workspace %s.xcworkspace \\" workspace)
      (format "-project %s.xcodeproj \\" projectname))))

(cl-defun build-app-command (&simulatorId simulatorId &deviceId deviceId)
  "Xcodebuild with (as &SIMULATORID as &DEVICEID)."
  (concat
   (xcodebuild-command)
   (get-workspace-or-project)
   (format "-scheme %s \\" (fetch-or-load-xcode-scheme))
   (format "-configuration %s \\" (fetch-or-load-build-configuration))
   (format "-jobs %s \\" (get-number-of-cores))
   (format "-sdk %s \\" (get-current-sdk))
   (if simulatorId
       (format "-destination 'generic/platform=iOS Simulator,id=%s' \\" simulatorId)
     (format "-destination 'generic/platform=iOS' \\" ))
   "-skipUnavailableActions \\"
   "-destination-timeout 1 \\"
   "-scmProvider system \\"
   ;; "-parallelizeTargets \\"
   "-packageCachePath ~/Library/Cache/com.apple.swiftpm \\"
   "-quiet \\"
   "-derivedDataPath build"))

(cl-defun get-app-name-from (&key folder)
  "Get compiled app name from (FOLDER)."
  (when-let (binary-name (directory-files folder nil "\\.app$"))
    (file-name-sans-extension (car binary-name))))

(defun get-index-store-path ()
  "Get the index store path."
  (let ((index-store-path (concat current-project-root "build/Index/DataStore")))
        (if (file-directory-p index-store-path)
            index-store-path
          nil)))

(cl-defun parse-errors-from (&key text)
  "Parse errors from TEXT."
  (when DEBUG (message (concat "Errors:" text)))
  (message-with-color :tag "[Errors/Warnings]" :text "Warning not a clean build." :attributes 'error)
  (periphery-run-parser text))

(defun format-device-id (id)
  "Format device id (as ID)."
  (if id
      (if (not
           (string-match-p (regexp-quote "-") id))
          (concat (substring id 0 8) "-" (substring id 6))
        id)))

(cl-defun install-app-in-simulator-command (&key simulatorID)
  "Install and launch app (SIMULATORID)."
  (when DEBUG (message (concat "Buildpath: "  (get-build-folder))))
  (let ((folder (get-build-folder)))
    (concat
     "env /usr/bin/arch -x86_64 \\"
     (format "xcrun simctl install %s %s%s.app\n" simulatorID folder (get-app-name-from :folder folder)))))

(defun swift-additions:terminate-all-running-apps ()
    "Terminate runnings apps."
    (interactive)
    (terminate-app-in-simulator :simulatorID current-simulator-id)
    (terminate-app-in-simulator :simulatorID secondary-simulator-id))

(cl-defun terminate-app-in-simulator (&key simulatorID)
  "Terminate app that is running in simulator with SIMULATORID."
  (message-with-color :tag "[Terminating]" :text current-xcode-scheme :attributes 'warning)
  (call-process-shell-command
   (concat
    (if simulatorID
        (format "xcrun simctl terminate %s %s" simulatorID (fetch-or-load-app-identifier))
      (format "xcrun simctl terminate booted %s" (fetch-or-load-app-identifier))))))

(defun swift-additions:run-app()
  "Run app.  Either in simulator or on physical."
  (if local-device-id
      (swift-additions:install-app-on-device)
    (install-app-in-simulator)))

(defun swift-additions:check-for-errors (text)
  "Run periphery parser on TEXT."
  (if (or
       (string-match-p (regexp-quote "BUILD FAILED") text)
       (string-match-p (regexp-quote "error: ") text)
       (string-match-p (regexp-quote "warning: ") text))
      (progn
        (parse-errors-from :text text)
        (message text)
        (when (not (string-match-p (regexp-quote "error: ") text))
          (swift-additions:run-app)))
    (swift-additions:run-app)))

(cl-defun launch-app-in-simulator (&key appIdentifier applicationName simulatorName simulatorID)
  "Command to filter and log the simulator (as APPIDENTIFIER APPLICATIONNAME SIMULATORNAME SIMULATORID)."

  (message-with-color :tag "[Running]" :text (format "%s on %s" applicationName simulatorName) :attributes 'success)

  (if-let ((simulatorID simulatorID))
      (format "xcrun simctl launch --console-pty %s %s" simulatorID appIdentifier)
    (format "xcrun simctl launch --console-pty booted %s" appIdentifier)))

(cl-defun run-async-command-in-buffer (&key command)
  "Run async-command in xcodebuild buffer (as COMMAND)."
  (async-shell-command command xcodebuild-buffer))

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

(defun get-connected-device-id ()
  "Get the id of the connected device."
  (let ((device-id
         (clean-up-newlines
          (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
    (if (= (length device-id) 0)
        nil
      (format-device-id device-id))))

(cl-defun show-notification (&key title &key message)
  "Show notification (as TITLE as MESSAGE)."
  (shell-command (format "%s -title \"%s\" -message \"%s\"" notifier-command title message)))

(defun get-current-sdk ()
  "Return the current SDK."
  (if local-device-id
      "iphoneos"
    "iphonesimulator"))

(defun swift-additions:install-app-on-device ()
  "Install an app on device."
  (when DEBUG (message (concat "Buildpath:" (get-build-folder))))

  (let* ((folder (get-build-folder))
         (app-name (get-app-name-from :folder folder))
         (default-directory (concat current-project-root folder)))
    (message-with-color :tag "[Installing]" :text (format "%s onto physical device. Will launch app when done." app-name) :attributes 'warning)
    (run-async-command-in-buffer :command (format "ios-deploy -b %s.app -d" app-name))))

(defun install-app-in-simulator ()
  "Install the app in the simulator."
  (let* ((default-directory current-project-root)
         (simulator-id (fetch-or-load-simulator-id)))

    (swift-additions:terminate-all-running-apps)
    (setq applicationName (get-app-name-from :folder (get-build-folder)))
    (setq simulatorName  (fetch-simulator-name))

    (message-with-color
     :tag "[Installing]"
     :text (format "%s onto %s. Will launch app when done." applicationName simulatorName)
     :attributes '(:inherit success))

    (call-process-shell-command (install-app-in-simulator-command :simulatorID simulator-id))

    (when-let ((secondary-id secondary-simulator-id))
      (setup-simulator-dwim secondary-simulator-id)
      (call-process-shell-command (install-app-in-simulator-command :simulatorID secondary-id)))

    (run-async-command-in-buffer
        :command (launch-app-in-simulator
                  :appIdentifier current-app-identifier
                  :applicationName applicationName
                  :simulatorName simulatorName
                  :simulatorID simulator-id))))

(defun build-using-compilation-mode ()
  "Build using builtin compile and 'compilation-mode'."
  (interactive)
  (let* ((default-directory (current-project-root))
         (compile-command (build-app-command
                           :simulatorId (fetch-or-load-simulator-id)
                           :deviceId (get-connected-device-id))))
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
  (setq current-buildconfiguration-json-data nil)
  (setq local-device-id nil)
  (message-with-color :tag "[Resetting]" :text "Build configiration" :attributes 'warning))

(defun swift-additions:compile-and-run-silent ()
  "Build project using xcodebuild."
  (interactive)
  (save-some-buffers t)
  (periphery-kill-buffer)
  (swift-additions:kill-xcode-buffer)

  (setq device-or-simulator "[Building device target]")
  (setq local-device-id (get-connected-device-id))
  (when (not local-device-id)
    (fetch-or-load-simulator-id)
    (setq device-or-simulator "[Building simulator target]"))

  (if (is-a-swift-package-base-project)
    (swift-additions:build-swift-package)
    (progn
      (if (vc-root-dir)
          (setup-current-project (get-ios-project-root)))
      (let ((default-directory current-project-root))
        (async-shell-command-to-string
        :process-name "periphery"
        :command (build-app-command :simulatorId: current-simulator-id :deviceId local-device-id)
        :callback #'swift-additions:check-for-errors))
      (message-with-color :tag device-or-simulator :text (format "%s. Please wait. Patience is a virtue!" current-xcode-scheme) :attributes 'warning))))

(defun swift-additions:test-module-silent ()
    "Test module."
  (interactive)
  (save-some-buffers t)
  (periphery-kill-buffer)
  (swift-additions:kill-xcode-buffer)
  (swift-additions:test-swift-package))

(defun swift-additions:clean-build-folder ()
  "Clean app build folder."
  (interactive)
  (if (is-a-swift-package-base-project)
      (swift-additions:clean-build-folder-with (vc-root-dir) ".build" "swift package")
    (swift-additions:clean-build-folder-with (get-ios-project-root) "build" current-xcode-scheme)))

(defun swift-additions:clean-build-folder-with (projectRoot buildFolder projectName)
  "Clean build folder with PROJECTROOT BUILDFOLDER and PROJECTNAME."
  (message-with-color :tag "[Cleaning]" :text (format "Build folder for %s Standby..." projectName) :attributes '(:inherit warning))
  (let ((default-directory (concat projectRoot buildFolder)))
    (if (file-directory-p default-directory)
        (progn
          (message-with-color :tag "[Removing]" :text (format "Folder for %s" default-directory) :attributes '(:inherit warning))
          (delete-directory default-directory t nil))
          (message-with-color :tag "[Failed]" :text (format "Build folder %s doesn't exist" default-directory) :attributes '(:inherit error))))
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

(defun swift-additions:get-buildconfiguration-json ()
  "Return a cached version or load the build configuration."
  (unless current-buildconfiguration-json-data
    (setq current-buildconfiguration-json-data (call-process-to-json build-warning-command)))
  current-buildconfiguration-json-data)

(defun swift-additions:get-target-list ()
  "Get list of project targets."
  (unless current-project-root
    (setq current-project-root (get-ios-project-root)))

  (message-with-color :tag "[Fetching]" :text "app targets.." :attributes '(:inherit warning))

  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (targets (cdr (assoc 'targets project))))
    targets))

(defun swift-additions:get-scheme-list ()
  "Get list of project schemes."
  (unless current-project-root
    (setq current-project-root (get-ios-project-root)))

  (message-with-color :tag "[Fetching]" :text "build schemes.." :attributes '(:inherit warning))

  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (result (cdr (assoc 'schemes project))))
    result))

(defun swift-additions:get-configuration-list ()
  "Get list of project configurations."
  (message-with-color :tag "[Fetching]" :text "build configurations.." :attributes '(:inherit warning))

  (unless current-project-root
    (setq current-project-root (get-ios-project-root)))
  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
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

(cl-defun build-menu (&key title &key list)
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
    (if (not DEBUG)
        (setq-local inhibit-message t)))

(defun is-a-swift-package-base-project ()
  "Check if project is a swift package based."
  (let ((default-directory (vc-root-dir)))
    (file-exists-p "Package.swift")))

(defun swift-additions:check-for-spm-build-errors (text)
    "Check for Swift package build erros in TEXT."
  (when DEBUG (message text))
  (if (or
       (string-match-p (regexp-quote "BUILD FAILED") text)
       (string-match-p (regexp-quote "error:") text)
       (string-match-p (regexp-quote "warning:") text))
      (progn
        (parse-errors-from :text text)
        (when (not (string-match-p (regexp-quote "error:") text))
         (shell-command "swift run" xcodebuild-buffer)))
    (shell-command "swift run" xcodebuild-buffer)))

(defun swift-additions:build-swift-package ()
  "Build swift package module."
  (interactive)
  (let ((default-directory (vc-root-dir)))
    (async-shell-command-to-string :process-name "periphery" :command "swift build" :callback #'swift-additions:check-for-spm-build-errors)
    (message-with-color :tag "[Building Package]" :text (format "%s. Please wait. Patience is a virtue!" (vc-root-dir)) :attributes 'warning)))

(defun swift-additions:test-swift-package ()
  "Test swift package module."
  (interactive)
  (let ((default-directory (vc-root-dir)))
    (async-shell-command-to-string :process-name "periphery" :command "swift test" :callback #'swift-additions:check-for-spm-build-errors)
    (message-with-color :tag "[Testing Package]" :text (format "%s. Please wait. Patience is a virtue!" (vc-root-dir)) :attributes 'warning)))

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


(defface tree-sitter-hl-face:repeat
  '((t :inherit tree-sitter-hl-face:keyword
       :foreground "#666bb2"))
  "Face for loops (for, in etc)."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:parameter
  '((t :inherit tree-sitter-hl-face:label
       :foreground "#666bb2"))
  "Face for parameters in function calls."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:conditional
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for if/switch."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:include
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Import/include face."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:boolean
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for true/false."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.return
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for 'return'."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.operator
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for operator."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.function
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for 'func' keyword."
  :group 'tree-sitter-hl-faces)

(defun command-output-to-string (command &rest args)
  "Like `shell-command-to-string' but dropping error output.

Also trims whitespace from the ends of any output."
  (string-trim
   (with-output-to-string
    (with-current-buffer standard-output
      (apply #'call-process command nil '(t nil) nil args)))))

(defvar-local my-swift-mode:eglot-server-platform nil
  "Platform for the current project, either `:ios' or `:macos'.
`nil' by default.

This is used to calculate Swift compiler args when starting up a
SourceKit server through eglot. Note that this is really only
needed for Xcode projects to work with SourceKit. SPM projects
work without any extra configuration.")

(defvar my-swift-mode:-eglot-default-target nil
  "This machine's default Clang target triple.

Lazily initialized during Swift Eglot configuration.")

(defun my-swift-mode:xcrun (&rest args)
  "Invoke xcrun with the given ARGS.

The result is returned as a string."
  (apply #'command-output-to-string "xcrun" args))

(defun my-swift-mode:sourcekit-args (platform)
  "Determine Swift compiler args for SourceKit for PLATFORM.

See also `my-swift-mode:eglot-server-platform'."
  (unless my-swift-mode:-eglot-default-target
    (setq my-swift-mode:-eglot-default-target
          (command-output-to-string "clang" "-print-target-triple")))
  (let* ((show-sdk-path (lambda (sdk-name)
                          (my-swift-mode:xcrun
                           "--show-sdk-path" "--sdk" sdk-name)))
         (arg-vals
          (pcase platform
            (:ios
             (let* ((target-components
                     (split-string my-swift-mode:-eglot-default-target "-"))
                    (arch (nth 0 target-components))
                    (vendor (nth 1 target-components))
                    (sim-version (my-swift-mode:xcrun "--sdk" "iphonesimulator"
                                                      "--show-sdk-version")))
               (list (funcall show-sdk-path "iphonesimulator")
                     (format "%s-%s-ios%s-simulator" arch vendor sim-version))))
            ((or :macos :macosx :osx)
             (list (funcall show-sdk-path "macosx")
                   my-swift-mode:-eglot-default-target))
            ;; No args needed for SPM
            (_ nil))))
    (when arg-vals
      `(
        "build"
        "--completion-max-results" "50"
        "-Xswiftc" "-sdk"
        "-Xswiftc" ,(car arg-vals)
        "-Xswiftc" "-target"
        "-Xswiftc" ,(cadr arg-vals)))))

(defun my-swift-mode:eglot-server-contact (_ignored)
  "Construct the list that eglot needs to start sourcekit-lsp.

If `my-swift-mode:eglot-server-platform' is defined, the
appropriate flags to pass to the Swift compiler for the platform
will be included in the list."
  (let ((args (my-swift-mode:sourcekit-args my-swift-mode:eglot-server-platform))
        (sourcekit-path (my-swift-mode:xcrun "--find" "sourcekit-lsp")))
        (message args)
    `(,sourcekit-path ,@args)))

(provide 'swift-additions)

;;; swift-additions.el ends here
