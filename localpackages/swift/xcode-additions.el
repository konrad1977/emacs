;;; xcode-additions.el --- package for compiling and running swift apps in  -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(require 'project)
(require 'mode-line-hud)
(require 'xcodebuildserver)
(require 'swift-project)

(defvar current-project-root nil)
(defvar current-xcode-scheme nil)
(defvar current-build-settings-json nil)
(defvar current-build-configuration nil)
(defvar current-app-identifier nil)
(defvar current-build-folder nil)
(defvar current-is-xcode-project nil)
(defvar current-local-device-id nil)
(defvar current-errors-or-warnings nil)
(defvar xcode-additions:last-device-type nil)
(defvar xcode-additions:device-choice nil
  "Stores the user's choice of device (simulator or physical device).")

(defconst xcodebuild-list-config-command "xcrun xcodebuild -list -json")

(defgroup xcode-additions:xcodebuild nil
  "REPL."
  :tag "xcode-additions:xcodebuild"
  :group 'xcode-additions)

(defcustom xcode-additions:debug nil
  "Enable debug mode for xcode additions."
  :type 'boolean
  :group 'xcode-additions:xcodebuild)
        
(defconst xcode-additions-extensions
  '(
    :project ".*\\.xcodeproj$"
    :workspace ".*\\.xcworkspace$"
    )
  "File extensions for Xcode project files.")

;;;###autoload
(defcustom xcode-additions:clean-build-ignore-list
  '("ModuleCache.noindex" "SourcePackages")
  "List of directories to ignore when cleaning build folder."
  :type '(repeat string)
  :group 'xcode-additions)

(defun xcode-get-app-path ()
  "Get the path to Xcode.app/ (with trailing slash) using xcode-select."
  (let ((developer-path (string-trim (shell-command-to-string "xcode-select -p"))))
    (cond
     ;; Extract Xcode.app path from developer path and add trailing slash
     ((string-match "\\(.*/Xcode\\.app\\)" developer-path)
      (concat (match-string 1 developer-path) "/"))
     
     ;; Fallback to standard location if it exists
     ((file-exists-p "/Applications/Xcode.app")
      "/Applications/Xcode.app/")
     
     ;; Return nil if not found
     (t nil))))

(defun xcode-additions-accessability-inspector ()
  "Launch the Accessibility Inspector."
  (interactive)
  (let ((accessibility-inspector-path (concat (xcode-get-app-path) "Contents/Applications/Accessibility Inspector.app")))
    (if (file-exists-p accessibility-inspector-path)
        (start-process "Accessibility Inspector" nil "open" accessibility-inspector-path)
      (message "Accessibility Inspector not found at %s" accessibility-inspector-path))))

(defun xcode-additions-instruments ()
  "Launch the Instruments application."
  (interactive)
  (let ((instruments-file-path (concat (xcode-get-app-path) "Contents/Applications/Instruments.app")))
    (if (file-exists-p instruments-file-path)
        (start-process "Instruments" nil "open" instruments-file-path)
      (message "Istruments not found at %s" instruments-file-path))))

(defun xcode-additions-get-extension (type)
  "Get file extension for TYPE (:project or :workspace)."
  (plist-get xcode-additions-extensions type))

(defconst xcodeproject-extension (xcode-additions-get-extension :project)
  "Xcode project extensions.")

(defconst workspace-extension (xcode-additions-get-extension :workspace)
  "Xcode workspace extensions.")

(defun xcode-additions:filename-by-extension (extension directory)
  "Get filename (without extension) for first file matching EXTENSION in DIRECTORY."
  (when-let* ((files (directory-files directory t extension))
              (first-match (car files)))
    (file-name-sans-extension (file-name-nondirectory first-match))))

(defun xcode-additions:directory-contains-p (extension directory)
  "Check if DIRECTORY contain files matching EXTENSION."
  (consp (directory-files directory nil extension)))

(defun xcode-additions:project-directory-p (directory)
  "Check if xcodeproj file exists in DIRECTORY or immediate subdirectories."
  (or (xcode-additions:directory-contains-p xcodeproject-extension directory)
      (cl-some (lambda (dir)
                 (let ((full-dir (expand-file-name dir directory)))
                   (and (file-directory-p full-dir)
                        (not (member dir '("." "..")))
                        (xcode-additions:directory-contains-p xcodeproject-extension full-dir))))
               (directory-files directory))))

(defun xcode-additions:workspace-directory-p (directory)
  "Check if xcworkspace file exists in DIRECTORY."
  (xcode-additions:directory-contains-p workspace-extension directory))

(defun xcode-additions:find-ancestor-or-self-directory (predicate &optional directory)
  "Find first ancestor directory (including DIRECTORY itself) where PREDICATE returns non-nil.
If DIRECTORY is nil, use `default-directory'."
  (let ((dir (expand-file-name (or directory default-directory))))
    (cond ((funcall predicate dir) dir)
          ((or (string-equal dir "/")
               (string-equal dir (directory-file-name dir)))
           nil)
          (t (xcode-additions:find-ancestor-or-self-directory
              predicate
              (file-name-directory (directory-file-name dir)))))))

(defun xcode-additions:find-xcode-project-directory (&optional directory)
  "Try to find xcode project in DIRECTORY or its subdirectories."
  (when-let* ((start-dir (or directory default-directory))
              (found-dir (xcode-additions:find-ancestor-or-self-directory 'xcode-additions:project-directory-p start-dir)))
    (if (xcode-additions:directory-contains-p xcodeproject-extension found-dir)
        found-dir
      (cl-some (lambda (dir)
                 (let ((full-dir (expand-file-name dir found-dir)))
                   (and (file-directory-p full-dir)
                        (not (member dir '("." "..")))
                        (xcode-additions:directory-contains-p xcodeproject-extension full-dir)
                        full-dir)))
               (directory-files found-dir)))))

(defun xcode-additions:find-workspace-directory (&optional directory)
  "Try to find xcode workspace in DIRECTORY or its ancestors."
  (xcode-additions:find-ancestor-or-self-directory 'xcode-additions:workspace-directory-p directory))

(defun xcode-additions:workspace-name ()
  "Get the workspace name in current or ancestor directories."
  (when-let* ((dir (xcode-additions:find-workspace-directory)))
    (xcode-additions:filename-by-extension workspace-extension dir)))

(defun xcode-additions:project-name ()
  "Get the project name in current or ancestor directories."
  (when-let* ((dir (xcode-additions:find-xcode-project-directory)))
    (xcode-additions:filename-by-extension xcodeproject-extension dir)))

(defun xcode-additions:list-xcscheme-files (folder)
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of FOLDER."
  (let ((xcscheme-names '()))
    (setq folder (file-name-as-directory (expand-file-name folder)))
    ;; Fixa stavningen av "schemes" (var "xcshemes")
    (setq xcshemes-folder (concat folder "xcshareddata/xcschemes/"))

    (when xcode-additions:debug
      (message "Searching for schemes in folder: %s" xcshemes-folder))

    (when (file-directory-p xcshemes-folder)
      (dolist (item (directory-files xcshemes-folder t "\\.xcscheme$"))
        (when (file-regular-p item)
          (let ((scheme-name (file-name-sans-extension (file-name-nondirectory item))))
            (when xcode-additions:debug
              (message "Found scheme: %s" scheme-name))
            (push scheme-name xcscheme-names)))))

    (setq xcscheme-names (nreverse xcscheme-names))

    (when xcode-additions:debug
      (message "Final scheme list: %s" xcscheme-names))

    xcscheme-names))

(defun xcode-additions:list-scheme-files ()
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of the current Xcode project or workspace directory."
  (let* ((project-directory (xcode-additions:find-xcode-project-directory))
         (project-name (xcode-additions:project-name))
         (full-project-path (when project-directory
                             (concat (file-name-as-directory project-directory)
                                   project-name
                                   ".xcodeproj/"))))

    (when xcode-additions:debug
      (message "xcode-additions:list-scheme-files:
Project directory: %s
Project name: %s
Full project path: %s"
               project-directory project-name full-project-path))

    (when full-project-path
      (xcode-additions:list-xcscheme-files full-project-path))))

(cl-defun xcode-additions:get-build-settings-json (&key (config "Debug"))
  "Get build settings from xcodebuild."
  (unless current-build-settings-json
    (let ((default-directory (xcode-additions:find-xcode-project-directory)))
      (setq current-build-settings-json
            (call-process-to-json "xcrun" "xcodebuild" "-showBuildSettings" "-configuration" config "-json"))))
  current-build-settings-json)

(defun xcode-additions:product-name ()
  "Get product name."
  (let ((json (xcode-additions:get-build-settings-json)))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_NAME)))

(defun xcode-additions:get-bundle-identifier (config)
  "Get bundle identifier (as CONFIG)."
  (let ((json (xcode-additions:get-build-settings-json :config config)))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_BUNDLE_IDENTIFIER)))

(cl-defun xcode-additions:build-menu (&key title list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) (cons item item)) list))
             (choice (completing-read title choices)))
        (cdr (assoc choice choices))))))

(defun xcode-additions:parse-build-folder (directory)
  "Parse build folders from DIRECTORY.
Returns a list of folder names, excluding hidden folders."
  (if (file-directory-p directory)
      (let ((folders (directory-files directory nil "^[^.].*" t)))
        (cl-remove-if-not
         (lambda (folder)
           (and (file-directory-p (expand-file-name folder directory))
                (not (member folder '("." "..")))))
         folders))
    nil))


(defun xcode-additions:scheme ()
  "Get the xcode scheme if set otherwise prompt user."
  (unless current-xcode-scheme
    (let ((schemes (xcode-additions:get-scheme-list)))
      (if (= (length schemes) 1)
          ;; If there's only one scheme, use it automatically
          (setq current-xcode-scheme (car schemes))
        ;; Otherwise prompt the user
        (setq current-xcode-scheme (xcode-additions:build-menu :title "Choose scheme: " :list schemes)))))
  (if (not current-xcode-scheme)
      (error "No scheme selected")
    (shell-quote-argument current-xcode-scheme)))

(defun xcode-additions:fetch-or-load-build-configuration ()
  "Get the build configuration or promp user."
  (setq current-build-configuration "Debug")
  current-build-configuration)

(defun xcode-additions:fetch-or-load-app-identifier ()
  "Get the app identifier for the current configiration."
  (unless current-app-identifier
    (setq current-app-identifier (xcode-additions:get-bundle-identifier (xcode-additions:fetch-or-load-build-configuration))))
  current-app-identifier)

(defun xcode-additions:get-scheme-list ()
  "Get list of project schemes."
  (xcode-additions:list-scheme-files))

(cl-defun xcode-additions:build-folder (&key (device-type :device))
  "Get build folder. Automatically choose based on device type (iphoneos or iphonesimulator), or let the user choose if there are multiple options."
  (when (or (not current-build-folder)
            (not (eq device-type xcode-additions:last-device-type)))
    (let* ((default-directory (concat (file-name-as-directory (xcode-additions:derived-data-path)) ".build/Build/Products/"))
           (all-folders (xcode-additions:parse-build-folder default-directory))
           (target-suffix (if (eq device-type :simulator) "iphonesimulator" "iphoneos"))
           (matching-folders (seq-filter (lambda (folder) (string-match-p target-suffix folder)) all-folders)))

      (when xcode-additions:debug
        (message "xcode-additions:build-folder:\nAll folders: %s" all-folders)
        (message "Matching folders: %s" matching-folders))

      ;; Cache the build folder for this device type
      (setq current-build-folder
            (cond
             ;; Only one matching folder, use it automatically
             ((= (length matching-folders) 1)
              (car matching-folders))
             ;; Multiple matching folders, let user choose once
             ((> (length matching-folders) 1)
              (xcode-additions:build-menu
               :title "Choose build folder"
               :list matching-folders))
             ;; No matching folders, show all options
             (t
              (xcode-additions:build-menu
               :title "Choose build folder"
               :list all-folders))))
      (when current-build-folder
        (setq current-build-folder (shell-quote-argument (concat default-directory current-build-folder "/"))))
      (setq xcode-additions:last-device-type device-type)))
  current-build-folder)

(defun xcode-additions:setup-xcodebuildserver ()
  "Setup xcodebuild server."
  (xcodebuildserver:check-configuration
   :root (xcode-additions:project-root)
   :workspace (xcode-additions:get-workspace-or-project)
   :scheme (shell-quote-argument (xcode-additions:scheme))))

(defun xcode-additions:get-workspace-or-project ()
  "Check if there is workspace or project."
  (let ((workspace (xcode-additions:workspace-name))
        (projectname (xcode-additions:project-name)))
    (if workspace
        (format "-workspace %s.xcworkspace" (shell-quote-argument workspace))
      (format "-project %s.xcodeproj" (shell-quote-argument projectname)))))

(defun xcode-additions:get-configuration-list ()
  "Get list of project configurations."
  (let* ((default-directory (xcode-additions:project-root))
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (result (cdr (assoc 'configurations project))))
    result))

(defun xcode-additions:get-buildconfiguration-json ()
  "Return a cached version or load the build configuration."
  (unless current-buildconfiguration-json-data
    (mode-line-hud:update :message "Fetching build configuration")
    (setq current-buildconfiguration-json-data (call-process-to-json xcodebuild-list-config-command)))
  current-buildconfiguration-json-data)

(defun xcode-additions:get-target-list ()
  "Get list of project targets."
  (let* ((default-directory (xcode-additions:project-root))
         (json (xcode-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (targets (cdr (assoc 'targets project))))
    targets))

(defun xcode-additions:is-xcodeproject ()
  "Check if it's an Xcode project."
  (unless current-is-xcode-project
    (when-let* ((root (xcode-additions:project-root)))
      (setq current-is-xcode-project
            (directory-files root nil "\\(?:\\.xcworkspace\\|\\.xcodeproj\\)$" t 1))))
  current-is-xcode-project)

(defun xcode-additions:setup-current-project (project)
  "Check if we have a new project (as PROJECT).  If true reset settings."
  (unless current-project-root
    (setq current-project-root project)
    (let ((current-root (if (listp current-project-root)
                            (car current-project-root)
                          current-project-root)))
      (when (not (string= current-root project))
        (when xcode-additions:debug
          (message "Setting up new project: %s" project))
        (xcode-additions:reset)
        (setq default-directory project)
        (setq current-project-root project)))))

(defun xcode-additions:setup-project ()
  "Setup the current project."
  (xcode-additions:setup-current-project (xcode-additions:project-root)))

(defun xcode-additions:project-root ()
  "Get the project root as a path string."
  (setq current-project-root (swift-project-root)))

(cl-defun xcode-additions:device-or-simulator-menu (&key title)
"Build device or simulator menu (as TITLE)."
(defconst deviceList '(("Simulator" nil)
                        ("Physical device" t)))
(progn
  (let* ((choices (seq-map (lambda (item) item) deviceList))
          (choice (completing-read title choices)))
    (car (cdr (assoc choice choices))))))

(defun xcode-addition:ask-for-device-or-simulator ()
  "Show menu for running on simulator or device."
  (interactive)
  (when (and (ios-device:connected-device-id) (not xcode-additions:device-choice))
    (setq xcode-additions:device-choice
          (xcode-additions:device-or-simulator-menu :title "Run on simulator or device?"))
    (setq current-run-on-device xcode-additions:device-choice)))

(defun xcode-additions:run-in-simulator ()
  "Return t if app should run in simulator, nil for physical device."
  (if (null xcode-additions:device-choice)
      t  ; Default to simulator if not set
    (not xcode-additions:device-choice)))

;;;###autoload
(defun xcode-additions:reset ()
  "Reset the current project root and device choice."
  (interactive)
  (ios-simulator:reset)
  (periphery-kill-buffer)
  (setq current-run-on-device nil
        current-local-device-id nil
        current-is-xcode-project nil
        current-build-folder nil
        current-app-identifier nil
        current-build-configuration nil
        current-project-root nil
        current-xcode-scheme nil
        current-build-settings-json nil
        current-buildconfiguration-json-data nil
        current-errors-or-warnings nil
        xcode-additions:device-choice nil
        xcode-additions:last-device-type nil)  ; Reset device choice
  (swift-project-reset-root)
  (mode-line-hud:update :message "Resetting configuration"))

;;;###autoload
(defun xcode-additions:start-debugging ()
  "Start debugging immediately without confirmation."
  (interactive)
  (xcode-additions:setup-dape)
  (let ((config (copy-tree (cdr (assq 'ios dape-configs)))))
    (dape config)))

(defun xcode-additions:setup-dape()
  "Setup dape."
  (interactive)
  (require 'dape)
  (add-to-list 'dape-configs
               `(ios
                 modes (swift-mode)
                 command-cwd ,(or (project-root (project-current))
                                  default-directory)
                 command ,(file-name-concat dape-adapter-dir
                                            "codelldb"
                                            "extension"
                                            "adapter"
                                            "codelldb")
                 command-args ("--port" :autoport
                               "--settings" "{\"sourceLanguages\":[\"swift\"]}"
                               "--liblldb" "/Applications/Xcode.app/Contents/SharedFrameworks/LLDB.framework/Versions/A/LLDB")
                 port :autoport
                 simulator-id ,(ios-simulator:simulator-identifier)
                 app-bundle-id ,(xcode-additions:fetch-or-load-app-identifier)
                 fn (dape-config-autoport
                     ,(lambda (config)
                        (with-temp-buffer
                          (let* ((command
                                  (format "xcrun simctl launch --wait-for-debugger --terminate-running-process %S %S --console-pty"
                                          (plist-get config 'simulator-id)
                                          (plist-get config 'app-bundle-id)))
                                 (code (call-process-shell-command command nil (current-buffer))))
                            (dape--message (format "* Running: %S *" command))
                            (dape--message (buffer-string))
                            (save-match-data
                              (if (and (zerop code)
                                       (progn (goto-char (point-min))
                                              (search-forward-regexp "[[:digit:]]+" nil t)))
                                  (plist-put config :pid (string-to-number (match-string 0)))
                                (dape--message (format "* Running: %S *" command))
                                (dape--message (format "Failed to start simulator:\n%s" (buffer-string)))
                                (user-error "Failed to start simulator")))))
                        config))
                 :type "lldb"
                 :request "attach"
                 :cwd ".")))


(defun xcode-additions:clean-build-folder ()
  "Clean app build folder."
  (interactive)
  (let ((root (xcode-additions:project-root)))
    (unless root
      (error "Not in an Xcode project"))
    (xcode-additions:clean-build-folder-with
     :root root
     :build-folder (expand-file-name ".build" root)
     :project-name (xcode-additions:product-name)
     :ignore-list xcode-additions:clean-build-ignore-list)))

(cl-defun xcode-additions:clean-build-folder-with (&key root build-folder project-name ignore-list)
  "Clean build folder with ROOT, BUILD-FOLDER, PROJECT-NAME asynchronously.
IGNORE-LIST is a list of folder names to ignore during cleaning."
  (when xcode-additions:debug
    (message "Cleaning build folder: %s for %s" build-folder project-name))
  (let ((default-directory build-folder))
    (if (file-directory-p default-directory)
        (progn
          (mode-line-hud:update
           :message (format "Cleaning build folder for %s"
                            (propertize project-name 'face 'warning)))
          (async-start
           `(lambda ()
              ,(async-inject-variables "default-directory")
              (defun delete-directory-contents (directory ignore-list)
                "Delete contents of DIRECTORY, ignoring folders in IGNORE-LIST."
                (dolist (file (directory-files directory t))
                  (let ((file-name (file-name-nondirectory file)))
                    (unless (or (member file-name '("." ".."))
                                (member file-name ignore-list))
                      (if (file-directory-p file)
                          (progn
                            (delete-directory file t t))
                        (delete-file file t))))))
              (condition-case err
                  (progn
                    (delete-directory-contents ,default-directory ',ignore-list) "successfully")
                (error (format "Error during cleaning: %s" (error-message-string err)))))
           `(lambda (result)
              (mode-line-hud:notification
               :message (format "Cleaning %s %s"
                                (propertize ,project-name 'face 'warning)
                                result)
               :seconds 3
               :reset t))))
      (mode-line-hud:notification
       :message (propertize "Build folder is empty or does not exist." 'face 'warning)
       :seconds 3
       :reset t))))

(defun xcode-additions:open-in-xcode ()
  "Open project in xcode."
  (interactive)
  (if-let* ((default-directory (xcode-additions:project-root))
           (command "xed ."))
      (inhibit-sentinel-messages #'call-process-shell-command command)))

(cl-defun xcode-additions:parse-compile-lines-output (&key input)
  "Parse compile output and print unique matched lines using separate message calls.
   Also prints compiler messages for C++ errors, warnings, and notes."
  (when xcode-additions:debug
    (message "Parsing compile output: %s" input))
  (let ((seen-messages (make-hash-table :test 'equal))
        (error-regex "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(error\\|warning\\|note\\): .+$"))
    (dolist (line (split-string input "\n"))
      (cond
       ((string-match "CompileC \\(.+\\)/\\([^/]+\\)$" line)
        (let ((msg (match-string 2 line)))
          (unless (gethash msg seen-messages)
            (mode-line-hud:update :message
                                  (format "Compiling %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ((string-match "CompileSwiftModule \\([^ ]+\\)" line)
        (let ((msg (match-string 1 line)))
          (unless (gethash msg seen-messages)
            (mode-line-hud:update :message
                                  (format "Compiling module %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ((string-match error-regex line)
        (setq current-errors-or-warnings (concat line "\n" current-errors-or-warnings))
        (periphery-run-parser current-errors-or-warnings))))))

(defun xcode-additions:derived-data-path ()
  "Extract the DerivedData path from xcodebuild output."
  (xcode-additions:project-root))

(defun xcode-additions:target-build-directory (&optional configuration)
  "Get the build directory from xcodebuild -showBuildSettings output.
CONFIGURATION is the build configuration (Debug/Release)."
  (let ((json (xcode-additions:get-build-settings-json)))
    (let-alist (seq-elt json 0)
      .buildSettings.TARGET_BUILD_DIR)))

(defun xcode-additions:open-build-folder ()
  "Open build folder."
  (interactive)
    (let ((default-directory (xcode-additions:derived-data-path)))
        (if (file-directory-p default-directory)
            (dired default-directory)
          (message "No build folder found"))))

(defun xcode-additions:toggle-device-choice ()
  "Toggle between simulator and physical device."
  (interactive)
  (setq xcode-additions:device-choice (not xcode-additions:device-choice))
  (setq current-run-on-device xcode-additions:device-choice)
  (message "Now running on %s" (if xcode-additions:device-choice "physical device" "simulator")))

(defun xcode-additions:show-current-configuration ()
  "Display the current Xcode project configuration."
  (interactive)
  (let ((config-message
         (format "Current Configuration:
Project Root: %s
Scheme: %s
Build Configuration: %s
App Identifier: %s
Build Folder: %s
Running on: %s
Simulator ID: %s
Debug Mode: %s"
                 current-project-root
                 current-xcode-scheme
                 current-build-configuration
                 current-app-identifier
                 current-build-folder
                 (if xcode-additions:device-choice "Physical Device" "Simulator")
                 (ios-simulator:simulator-identifier)
                 (if xcode-additions:debug "Enabled" "Disabled"))))
    (with-current-buffer (get-buffer-create "*Xcode Configuration*")
      (erase-buffer)
      (insert config-message)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun xcode-additions:toggle-debug ()
  "Toggle debug mode for xcode-additions."
  (interactive)
  (setq xcode-additions:debug (not xcode-additions:debug))
  (message "Xcode Additions debug mode %s" 
           (if xcode-additions:debug "enabled" "disabled")))

(provide 'xcode-additions)
;;; xcode-additions.el ends here
