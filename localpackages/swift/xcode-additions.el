;;; xcode-additions.el --- package for compiling and running swift apps in  -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(require 'project)
(require 'xcodebuildserver)
(require 'swift-project)
(require 'periphery)
(require 'periphery-helper)

;; Optional dependencies
(defvar mode-line-hud-available-p (require 'mode-line-hud nil t)
  "Whether mode-line-hud is available.")

(defun xcode-additions:safe-mode-line-update (&rest args)
  "Safely call mode-line-hud:update if available."
  (when mode-line-hud-available-p
    (apply #'mode-line-hud:update args)))

(defun xcode-additions:safe-mode-line-notification (&rest args)
  "Safely call mode-line-hud:notification if available."
  (when mode-line-hud-available-p
    (apply #'mode-line-hud:notification args)))

(defvar current-project-root nil)
(defvar previous-project-root nil)
(defvar current-xcode-scheme nil)
(defvar current-build-settings-json nil)
(defvar current-buildconfiguration-json-data nil)
(defvar current-build-configuration nil)
(defvar current-app-identifier nil)
(defvar current-build-folder nil)
(defvar current-is-xcode-project nil)
(defvar current-local-device-id nil)
(defvar current-errors-or-warnings nil)
(defvar xcode-additions:last-device-type nil)
(defvar xcode-additions:device-choice nil
  "Stores the user's choice of device (simulator or physical device).")
(defvar xcode-additions:cache-warmed-projects (make-hash-table :test 'equal)
  "Hash table tracking which projects have had their caches warmed.")

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
    ;; Set the schemes folder path  
    (setq xcschemes-folder (concat folder "xcshareddata/xcschemes/"))

    (when xcode-additions:debug
      (message "Searching for schemes in folder: %s" xcschemes-folder))

    (when (file-directory-p xcschemes-folder)
      (dolist (item (directory-files xcschemes-folder t "\\.xcscheme$"))
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
  "List the names of '.xcscheme' files in the xcshareddata/xcschemes subfolder of the current Xcode project or workspace directory."
  (let* ((project-root (xcode-additions:project-root))
         (default-directory (or project-root default-directory))
         (workspace-directory (xcode-additions:find-workspace-directory))
         (workspace-name (xcode-additions:workspace-name))
         (project-directory (xcode-additions:find-xcode-project-directory))
         (project-name (xcode-additions:project-name))
         (workspace-path (when workspace-directory
                          (concat (file-name-as-directory workspace-directory)
                                workspace-name
                                ".xcworkspace/")))
         (project-path (when project-directory
                        (concat (file-name-as-directory project-directory)
                              project-name
                              ".xcodeproj/"))))

    (when xcode-additions:debug
      (message "xcode-additions:list-scheme-files:
Workspace directory: %s
Workspace name: %s
Workspace path: %s
Project directory: %s
Project name: %s
Project path: %s"
               workspace-directory workspace-name workspace-path
               project-directory project-name project-path))

    ;; Try workspace first (preferred for CocoaPods projects), then project
    (let ((schemes (or (when workspace-path
                         (xcode-additions:list-xcscheme-files workspace-path))
                       (when project-path
                         (xcode-additions:list-xcscheme-files project-path)))))
      ;; If file-based detection fails, fall back to xcodebuild -list
      (or schemes
          (xcode-additions:get-schemes-from-xcodebuild)))))

(defun xcode-additions:run-command-and-get-json (command)
  "Run a shell COMMAND and return the JSON output."
  (let* ((json-output (shell-command-to-string command)))
    (when xcode-additions:debug
      (message "Command: %s" command)
      (message "JSON output length: %d" (length json-output))
      (message "JSON output preview: %s" (substring json-output 0 (min 200 (length json-output)))))
    (condition-case err
        (json-read-from-string json-output)
      (error 
       (when xcode-additions:debug
         (message "JSON parsing error: %s" (error-message-string err))
         (message "Full JSON output: %s" json-output))
       (error "JSON parsing failed: %s" (error-message-string err))))))

(defun xcode-additions:get-schemes-from-xcodebuild ()
  "Get list of schemes using xcodebuild -list command as fallback."
  (when xcode-additions:debug
    (message "Falling back to xcodebuild -list for scheme detection"))
  (condition-case err
      (let* ((project-dir (or (xcode-additions:project-root) default-directory))
             (json-data (let ((default-directory project-dir))
                           (xcode-additions:run-command-and-get-json "xcrun xcodebuild -list -json 2>/dev/null"))))
        (when json-data
          (let-alist json-data
            (cond
             ;; Workspace case
             (.workspace.schemes 
              (when xcode-additions:debug
                (message "Found schemes via xcodebuild (workspace): %s" .workspace.schemes))
              .workspace.schemes)
             ;; Project case  
             (.project.schemes
              (when xcode-additions:debug
                (message "Found schemes via xcodebuild (project): %s" .project.schemes))
              .project.schemes)
             (t
              (when xcode-additions:debug
                (message "No schemes found in xcodebuild output"))
              nil)))))
    (error
     (when xcode-additions:debug
       (message "Error running xcodebuild -list: %s" (error-message-string err)))
     nil)))

(cl-defun xcode-additions:get-build-settings-json (&key (config "Debug"))
  "Get build settings from xcodebuild."
  (unless current-build-settings-json
    (let ((project-dir (or (xcode-additions:project-root) default-directory)))
      (setq current-build-settings-json
            (let ((default-directory project-dir))
              (xcode-additions:run-command-and-get-json (format "xcrun xcodebuild %s -scheme %s -showBuildSettings -configuration %s -json 2>/dev/null" 
                                                                (xcode-additions:get-workspace-or-project)
                                                                (xcode-additions:scheme)
                                                                config))))))
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
  (when xcode-additions:debug
    (message "parse-build-folder: Checking directory: %s" directory)
    (message "parse-build-folder: Directory exists: %s" (file-directory-p directory)))
  (if (file-directory-p directory)
      (let* ((all-files (directory-files directory nil "^[^.].*" t))
             (folders (cl-remove-if-not
                      (lambda (folder)
                        (let ((full-path (expand-file-name folder directory))
                              (is-dir (file-directory-p (expand-file-name folder directory)))
                              (not-dot (not (member folder '("." "..")))))
                          (when xcode-additions:debug
                            (message "parse-build-folder: Checking %s -> dir:%s not-dot:%s" folder is-dir not-dot))
                          (and is-dir not-dot)))
                      all-files)))
        (when xcode-additions:debug
          (message "parse-build-folder: All files: %s" all-files)
          (message "parse-build-folder: Filtered folders: %s" folders))
        folders)
    (progn
      (when xcode-additions:debug
        (message "parse-build-folder: Directory %s does not exist" directory))
      nil)))


(defun xcode-additions:scheme ()
  "Get the xcode scheme if set otherwise prompt user."
  (unless current-xcode-scheme
    (when xcode-additions:debug
      (message "xcode-additions:scheme - Starting scheme detection..."))
    (let ((schemes (xcode-additions:get-scheme-list)))
      (when xcode-additions:debug
        (message "xcode-additions:scheme - Found %d schemes: %s" (length schemes) schemes))
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
  "Get list of project schemes from xcscheme files."
  (xcode-additions:list-scheme-files))

(cl-defun xcode-additions:build-folder (&key (device-type :device))
  "Get build folder. Automatically choose based on device type (iphoneos or iphonesimulator), or let the user choose if there are multiple options."
  (when (or (not current-build-folder)
            (not (eq device-type xcode-additions:last-device-type)))
    (let* ((build-products-dir 
            ;; Check if we're using project-local .build directory (from swift-additions)
            (let* ((project-root (file-name-as-directory (xcode-additions:project-root)))
                   (local-build-dir (concat project-root ".build/Build/Products/")))
              (if (file-exists-p local-build-dir)
                  local-build-dir
                ;; Fallback to derived data path  
                (let ((derived-path (xcode-additions:derived-data-path)))
                  (cond
                   ;; If derived path ends with .build, append Build/Products
                   ((string-suffix-p ".build" derived-path)
                    (concat derived-path "/Build/Products/"))
                   ;; Otherwise use the path as-is with Build/Products
                   (t (concat (file-name-as-directory derived-path) "Build/Products/")))))))
           (default-directory build-products-dir)
           (all-folders (xcode-additions:parse-build-folder default-directory))
           (target-suffix (if (eq device-type :simulator) "iphonesimulator" "iphoneos"))
           (matching-folders (seq-filter (lambda (folder) (string-match-p target-suffix folder)) all-folders)))

      (when xcode-additions:debug
        (message "xcode-additions:build-folder:\nUsing build products dir: %s" build-products-dir)
        (message "All folders: %s" all-folders)
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
        (setq current-build-folder (concat default-directory current-build-folder "/")))
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
    (let ((project-dir (or (xcode-additions:project-root) default-directory)))
      (xcode-additions:safe-mode-line-update :message "Fetching build configuration")
      (setq current-buildconfiguration-json-data 
            (let ((default-directory project-dir))
              (xcode-additions:run-command-and-get-json (concat xcodebuild-list-config-command " 2>/dev/null"))))))
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
  ;; Store the previous project root for comparison
  (setq previous-project-root current-project-root)
  
  ;; Normalize project path for comparison
  (let* ((normalized-project (file-truename (expand-file-name project)))
         (current-root (if current-project-root
                          (file-truename (expand-file-name 
                                         (if (listp current-project-root)
                                             (car current-project-root)
                                           current-project-root)))
                        nil))
         (project-changed (not (and current-root 
                                   (string= current-root normalized-project)))))
    
    (when (or (not current-project-root) project-changed)
      (when xcode-additions:debug
        (message "Project changed from '%s' to '%s'" current-root normalized-project))
      
      ;; Reset all cached values when project changes
      (xcode-additions:reset)
      (setq default-directory project)
      (setq current-project-root normalized-project)
      
      ;; Automatically warm build caches for new project (only once per project)
      (when (and (fboundp 'swift-additions:warm-build-cache)
                 (require 'swift-additions nil t)
                 (not (gethash normalized-project xcode-additions:cache-warmed-projects)))
        (message "Warming build caches for %s..." (file-name-nondirectory normalized-project))
        (swift-additions:warm-build-cache)
        ;; Mark this project as having warmed caches
        (puthash normalized-project t xcode-additions:cache-warmed-projects))
      
      (when xcode-additions:debug
        (message "Set up new project: %s" normalized-project)))))

(defun xcode-additions:setup-project ()
  "Setup the current project."
  (xcode-additions:setup-current-project (xcode-additions:project-root)))

;;;###autoload
(defun xcode-additions:show-project-info ()
  "Display current project information for debugging."
  (interactive)
  (message "Current project root: %s\nPrevious project root: %s\nApp identifier: %s\nBuild folder: %s\nScheme: %s"
           (or current-project-root "nil")
           (or previous-project-root "nil") 
           (or current-app-identifier "nil")
           (or current-build-folder "nil")
           (or current-xcode-scheme "nil")))

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
  (when (fboundp 'ios-simulator:reset)
    (ios-simulator:reset))
  (when (fboundp 'periphery-kill-buffer)
    (periphery-kill-buffer))
  (setq current-run-on-device nil
        current-local-device-id nil
        current-is-xcode-project nil
        current-build-folder nil
        current-app-identifier nil
        current-build-configuration nil
        previous-project-root current-project-root ; Store before resetting
        current-project-root nil
        current-xcode-scheme nil
        current-build-settings-json nil
        current-buildconfiguration-json-data nil
        current-errors-or-warnings nil
        xcode-additions:device-choice nil
        xcode-additions:last-device-type nil)  ; Reset device choice
  (swift-project-reset-root)
  (xcode-additions:safe-mode-line-update :message "Resetting configuration")
  (message "Xcode configuration reset - scheme cache cleared"))

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
          (xcode-additions:safe-mode-line-update
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
              (xcode-additions:safe-mode-line-notification
               :message (format "Cleaning %s %s"
                                (propertize ,project-name 'face 'warning)
                                result)
               :seconds 3
               :reset t))))
      (xcode-additions:safe-mode-line-notification
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
            (xcode-additions:safe-mode-line-update :message
                                  (format "Compiling %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ((string-match "CompileSwiftModule \\([^ ]+\\)" line)
        (let ((msg (match-string 1 line)))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                  (format "Compiling module %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ((string-match error-regex line)
        (setq current-errors-or-warnings (concat line "\n" current-errors-or-warnings))
        (periphery-run-parser current-errors-or-warnings))))))

(defun xcode-additions:derived-data-path ()
  "Get the actual DerivedData path by running xcodebuild -showBuildSettings."
  (let* ((default-directory (or (xcode-additions:project-root) default-directory))
         (json (xcode-additions:get-build-settings-json)))
    (when json
      (let-alist (seq-elt json 0)
        (or .buildSettings.BUILD_DIR
            .buildSettings.SYMROOT
            (concat (xcode-additions:project-root) "/.build"))))))

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
