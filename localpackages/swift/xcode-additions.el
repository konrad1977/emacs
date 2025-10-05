;;; xcode-additions.el --- package for compiling and running swift apps in  -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(require 'project)
(require 'xcodebuildserver)
(require 'swift-project)
(require 'periphery)
(require 'periphery-helper)
(require 'swift-cache nil t)  ; Optional - graceful fallback if not available

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
;; Removed: current-build-settings-json - now using swift-cache
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

(defun xcode-additions:get-app-path ()
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

(defun xcode-additions:accessibility-inspector ()
  "Launch the Accessibility Inspector."
  (interactive)
  (let ((accessibility-inspector-path (concat (xcode-additions:get-app-path) "Contents/Applications/Accessibility Inspector.app")))
    (if (file-exists-p accessibility-inspector-path)
        (start-process "Accessibility Inspector" nil "open" accessibility-inspector-path)
      (message "Accessibility Inspector not found at %s" accessibility-inspector-path))))

(defun xcode-additions:instruments ()
  "Launch the Instruments application."
  (interactive)
  (let ((instruments-file-path (concat (xcode-additions:get-app-path) "Contents/Applications/Instruments.app")))
    (if (file-exists-p instruments-file-path)
        (start-process "Instruments" nil "open" instruments-file-path)
      (message "Instruments not found at %s" instruments-file-path))))

(defun xcode-additions:get-extension (type)
  "Get file extension for TYPE (:project or :workspace)."
  (plist-get xcode-additions-extensions type))

(defconst xcodeproject-extension (xcode-additions:get-extension :project)
  "Xcode project extensions.")

(defconst workspace-extension (xcode-additions:get-extension :workspace)
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
         (cache-key (when (fboundp 'swift-cache-project-key)
                      (swift-cache-project-key project-root "scheme-files"))))
    ;; Use cache for scheme files if available
    (if (and cache-key (fboundp 'swift-cache-with))
        (swift-cache-with cache-key 600  ; Cache for 10 minutes
      (let* ((default-directory (or project-root default-directory))
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
              (xcode-additions:get-schemes-from-xcodebuild))))
      ;; Fallback when swift-cache not available - use original logic
      (let* ((default-directory (or project-root default-directory))
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
        (let ((schemes (or (when workspace-path
                             (xcode-additions:list-xcscheme-files workspace-path))
                           (when project-path
                             (xcode-additions:list-xcscheme-files project-path)))))
          (or schemes
              (xcode-additions:get-schemes-from-xcodebuild))))))))

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
  "Get build settings from xcodebuild CONFIG."
  (let* ((project-root (xcode-additions:project-root))
         (cache-key (when (fboundp 'swift-cache-project-key)
                      (swift-cache-project-key project-root 
                                              (format "build-settings-%s-%s" 
                                                      (or current-xcode-scheme "default")
                                                      config)))))
    (if (and cache-key (fboundp 'swift-cache-with))
        (swift-cache-with cache-key 1800  ; Cache for 30 minutes
          (let ((project-dir (or project-root default-directory)))
            (let ((default-directory project-dir)
                  (scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or current-xcode-scheme ""))))
              (xcode-additions:run-command-and-get-json 
               (format "xcrun xcodebuild %s -scheme %s -showBuildSettings -configuration %s -json 2>/dev/null" 
                       (xcode-additions:get-workspace-or-project)
                       (shell-quote-argument scheme-name)
                       (shell-quote-argument config))))))
      ;; Fallback when swift-cache not available
      (let ((project-dir (or project-root default-directory)))
        (let ((default-directory project-dir)
              (scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or current-xcode-scheme ""))))
          (xcode-additions:run-command-and-get-json 
           (format "xcrun xcodebuild %s -scheme %s -showBuildSettings -configuration %s -json 2>/dev/null" 
                   (xcode-additions:get-workspace-or-project)
                   (shell-quote-argument scheme-name)
                   (shell-quote-argument config))))))))

(defun xcode-additions:product-name ()
  "Get product name."
  (let ((json (xcode-additions:get-build-settings-json)))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_NAME)))

(defun xcode-additions:get-bundle-identifier (config)
  "Get bundle identifier (as CONFIG)."
  (condition-case err
      (let ((json (xcode-additions:get-build-settings-json :config config)))
        (let-alist (seq-elt json 0)
          .buildSettings.PRODUCT_BUNDLE_IDENTIFIER))
    (error
     ;; Fallback: use xcodebuild directly without JSON
     (let* ((scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or current-xcode-scheme "")))
            (workspace-or-project (xcode-additions:get-workspace-or-project))
            (cmd (format "xcodebuild -showBuildSettings %s -scheme %s -configuration %s 2>/dev/null | grep '^    PRODUCT_BUNDLE_IDENTIFIER' | head -1 | sed 's/.*= //' | tr -d ' '"
                        workspace-or-project
                        (shell-quote-argument scheme-name)
                        (shell-quote-argument config)))
            (output (string-trim (shell-command-to-string cmd))))
       (when xcode-additions:debug
         (message "Fallback command: %s" cmd)
         (message "Fallback output: %s" output))
       (if (string-empty-p output)
           (progn
             (when xcode-additions:debug
               (message "No bundle identifier found, using generic fallback"))
             "com.example.app")  ; Generic fallback
         output)))))

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
  "Get the build configuration from the scheme file."
  (unless current-build-configuration
    ;; Ensure scheme is loaded first
    (xcode-additions:scheme)
    (let* ((scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or current-xcode-scheme "")))
           (project-root (xcode-additions:project-root))
           (xcodeproj-dirs (directory-files project-root t "\\.xcodeproj$"))
           (xcodeproj-dir (car xcodeproj-dirs))
           (scheme-file (when xcodeproj-dir
                         (format "%s/xcshareddata/xcschemes/%s.xcscheme" 
                                xcodeproj-dir scheme-name))))
      (if (and scheme-file (file-exists-p scheme-file))
          (with-temp-buffer
            (insert-file-contents scheme-file)
            (goto-char (point-min))
            ;; Look for LaunchAction buildConfiguration first, then fallback to TestAction
            (if (re-search-forward "<LaunchAction[^>]*buildConfiguration\\s-*=\\s-*\"\\([^\"]+\\)\"" nil t)
                (setq current-build-configuration (match-string 1))
              (goto-char (point-min))
              (if (re-search-forward "<TestAction[^>]*buildConfiguration\\s-*=\\s-*\"\\([^\"]+\\)\"" nil t)
                  (setq current-build-configuration (match-string 1))
                (setq current-build-configuration "Debug"))))
        (setq current-build-configuration "Debug"))))
  current-build-configuration)

(defun xcode-additions:fetch-or-load-app-identifier ()
  "Get the app identifier for the current configiration."
  (unless current-app-identifier
    ;; Ensure scheme is loaded first
    (xcode-additions:scheme)
    (let ((config (xcode-additions:fetch-or-load-build-configuration)))
      (when xcode-additions:debug
        (message "xcode-additions:fetch-or-load-app-identifier - scheme: %s, config: %s" 
                 current-xcode-scheme config))
      (setq current-app-identifier (xcode-additions:get-bundle-identifier config))
      (when xcode-additions:debug
        (message "xcode-additions:fetch-or-load-app-identifier - bundle ID: %s" 
                 current-app-identifier))))
  current-app-identifier)

(defun xcode-additions:get-scheme-list ()
  "Get list of project schemes from xcscheme files."
  (xcode-additions:list-scheme-files))

(cl-defun xcode-additions:build-folder (&key (device-type :device))
  "Get build folder. Auto-detect based on scheme, configuration, and device type."
  (when (or (not current-build-folder)
            (not (eq device-type xcode-additions:last-device-type)))
    (let* ((scheme (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or current-xcode-scheme "")))
           (config (xcode-additions:fetch-or-load-build-configuration))
           (build-products-dir (xcode-additions:get-build-products-directory))
           (target-suffix (if (eq device-type :simulator) "iphonesimulator" "iphoneos"))
           (cache-key (when (fboundp 'swift-cache-project-key)
                       (swift-cache-project-key 
                        (xcode-additions:project-root)
                        (format "build-folder-%s-%s-%s" scheme config target-suffix)))))

      ;; Try cached result first
      (let ((cached-folder (when (and cache-key (fboundp 'swift-cache-get))
                            (swift-cache-get cache-key))))
        (if (and cached-folder (file-exists-p cached-folder))
            (setq current-build-folder cached-folder)
          ;; Auto-detect build folder
          (setq current-build-folder 
                (xcode-additions:auto-detect-build-folder 
                 :build-products-dir build-products-dir
                 :scheme scheme
                 :config config
                 :device-type device-type
                 :target-suffix target-suffix))
          
          ;; Format the full path
          (when current-build-folder
            (setq current-build-folder 
                  (if (file-name-absolute-p current-build-folder)
                      current-build-folder
                    (concat build-products-dir current-build-folder "/"))))
          
          ;; Cache the result
          (when (and cache-key (fboundp 'swift-cache-set) current-build-folder)
            (swift-cache-set cache-key current-build-folder 1800))))  ; Cache for 30 minutes

      (setq xcode-additions:last-device-type device-type)
      
      (when xcode-additions:debug
        (message "xcode-additions:build-folder: scheme=%s config=%s device=%s folder=%s" 
                 scheme config device-type current-build-folder))))
  current-build-folder)

(defun xcode-additions:get-build-products-directory ()
  "Get the base build products directory path."
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

(cl-defun xcode-additions:auto-detect-build-folder (&key build-products-dir scheme config device-type target-suffix)
  "Intelligently auto-detect the correct build folder."
  (let* ((default-directory build-products-dir)
         (all-folders (xcode-additions:parse-build-folder default-directory)))
    
    (when xcode-additions:debug
      (message "Auto-detecting build folder: scheme=%s config=%s target=%s" scheme config target-suffix)
      (message "Available folders: %s" all-folders))
    
    (or 
     ;; Strategy 1: Look for exact match with scheme-config-platform
     (xcode-additions:find-exact-build-folder all-folders scheme config target-suffix)
     
     ;; Strategy 2: Look for config-platform match
     (xcode-additions:find-config-platform-folder all-folders config target-suffix)
     
     ;; Strategy 3: Look for platform-only match
     (xcode-additions:find-platform-folder all-folders target-suffix)
     
     ;; Strategy 4: Look for any folder containing the scheme name
     (xcode-additions:find-scheme-folder all-folders scheme)
     
     ;; Strategy 5: Interactive fallback
     (xcode-additions:interactive-build-folder-selection all-folders target-suffix))))

(defun xcode-additions:find-exact-build-folder (folders scheme config target-suffix)
  "Find build folder matching scheme-config-platform pattern."
  (let ((patterns (list
                   ;; Pattern: Debug-iphonesimulator, Release-iphoneos, etc.
                   (format "%s-%s" config target-suffix)
                   ;; Pattern: MyApp_Debug-iphonesimulator
                   (format "%s_%s-%s" scheme config target-suffix)
                   ;; Pattern: MyApp-Debug-iphonesimulator  
                   (format "%s-%s-%s" scheme config target-suffix))))
    (cl-some (lambda (pattern)
              (cl-find-if (lambda (folder) 
                           (string-match-p (regexp-quote pattern) folder))
                         folders))
            patterns)))

(defun xcode-additions:find-config-platform-folder (folders config target-suffix)
  "Find build folder matching config-platform pattern."
  (let ((pattern (format "%s.*%s" config target-suffix)))
    (cl-find-if (lambda (folder) 
                 (string-match-p pattern folder))
               folders)))

(defun xcode-additions:find-platform-folder (folders target-suffix)
  "Find any build folder matching the platform."
  (cl-find-if (lambda (folder) 
               (string-match-p target-suffix folder))
             folders))

(defun xcode-additions:find-scheme-folder (folders scheme)
  "Find build folder containing the scheme name."
  (when (and scheme (not (string-empty-p scheme)))
    (cl-find-if (lambda (folder) 
                 (string-match-p (regexp-quote scheme) folder))
               folders)))

(defun xcode-additions:interactive-build-folder-selection (folders target-suffix)
  "Let user interactively select build folder with smart defaults."
  (let ((platform-folders (seq-filter (lambda (folder) 
                                        (string-match-p target-suffix folder)) 
                                      folders)))
    (cond
     ;; Only one platform match - use it
     ((= (length platform-folders) 1)
      (car platform-folders))
     ;; Multiple platform matches - let user choose
     ((> (length platform-folders) 1)
      (xcode-additions:build-menu
       :title (format "Choose %s build folder" 
                     (if (string= target-suffix "iphonesimulator") "Simulator" "Device"))
       :list platform-folders))
     ;; No platform matches - show all folders
     (t
      (xcode-additions:build-menu
       :title "Choose build folder"
       :list folders)))))

;;;###autoload
(defun xcode-additions:debug-build-folder-detection ()
  "Debug build folder detection process."
  (interactive)
  (let ((xcode-additions:debug t))
    (message "=== Build Folder Detection Debug ===")
    (message "Project root: %s" (xcode-additions:project-root))
    (message "Current scheme: %s" current-xcode-scheme)
    (message "Build configuration: %s" (xcode-additions:fetch-or-load-build-configuration))
    (message "Build products dir: %s" (xcode-additions:get-build-products-directory))
    (let ((build-products-dir (xcode-additions:get-build-products-directory)))
      (when (file-exists-p build-products-dir)
        (message "Available build folders:")
        (dolist (folder (xcode-additions:parse-build-folder build-products-dir))
          (message "  - %s" folder))))
    (message "Simulator folder: %s" (xcode-additions:build-folder :device-type :simulator))
    (message "Device folder: %s" (xcode-additions:build-folder :device-type :device))))

;;;###autoload  
(defun xcode-additions:clear-build-folder-cache ()
  "Clear cached build folder selections."
  (interactive)
  (setq current-build-folder nil
        xcode-additions:last-device-type nil)
  (when (fboundp 'swift-cache-invalidate-pattern)
    (swift-cache-invalidate-pattern "build-folder-"))
  (message "Build folder cache cleared"))

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
  (condition-case err
      (progn
        (xcode-additions:setup-dape)
        (let ((config (copy-tree (cdr (assq 'ios dape-configs)))))
          (if config
              (dape config)
            (error "Failed to setup dape configuration"))))
    (error
     (message "Error starting debugger: %s" (error-message-string err))
     (message "Please ensure a scheme is selected and the project is properly configured")
     (signal (car err) (cdr err)))))

(defun xcode-additions:setup-dape()
  "Setup dape."
  (interactive)
  (require 'dape)
  ;; Ensure scheme and project root are loaded first
  (xcode-additions:scheme)
  (xcode-additions:project-root)
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
                 simulator-id ,(or (ignore-errors (ios-simulator:simulator-identifier))
                                   (error "Failed to get simulator ID"))
                 app-bundle-id ,(or (xcode-additions:fetch-or-load-app-identifier)
                                    (error "Failed to get app bundle identifier"))
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
                 :cwd "."))
  (call-interactively #'dape))


(defun xcode-additions:clean-build-folder ()
  "Clean app build folder, Swift package caches, and optionally Xcode derived data."
  (interactive)
  (let ((root (xcode-additions:project-root)))
    (unless root
      (error "Not in an Xcode project"))
    
    ;; Clean .build folder
    (xcode-additions:clean-build-folder-with
     :root root
     :build-folder (expand-file-name ".build" root)
     :project-name (xcode-additions:product-name)
     :ignore-list xcode-additions:clean-build-ignore-list)
    
    ;; Clean Swift package caches
    (xcode-additions:clean-swift-package-caches)
    
    ;; Optionally clean derived data for this project
    (when (yes-or-no-p "Also clean Xcode derived data for this project?")
      (xcode-additions:clean-project-derived-data))))

(defun xcode-additions:clean-swift-package-caches ()
  "Clean Swift package manager caches."
  (let ((package-cache-dir (expand-file-name "~/Library/Caches/org.swift.packages"))
        (cloned-sources-dir (expand-file-name "~/Library/Caches/org.swift.cloned-sources")))
    
    (when (file-exists-p package-cache-dir)
      (message "Cleaning Swift package cache...")
      (async-start
       `(lambda ()
          (delete-directory ,package-cache-dir t)
          "Swift package cache cleaned")
       (lambda (result)
         (message "%s" result))))
    
    (when (file-exists-p cloned-sources-dir)
      (message "Cleaning Swift cloned sources...")
      (async-start
       `(lambda ()
          (delete-directory ,cloned-sources-dir t)
          "Swift cloned sources cleaned")
       (lambda (result)
         (message "%s" result))))))

(defun xcode-additions:clean-project-derived-data ()
  "Clean Xcode derived data for the current project."
  (let* ((project-name (xcode-additions:product-name))
         (derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
         (project-pattern (concat "^" (regexp-quote project-name) "-")))
    
    (when (file-exists-p derived-data-dir)
      (message "Cleaning derived data for %s..." project-name)
      (async-start
       `(lambda ()
          (let ((cleaned-count 0))
            (dolist (dir (directory-files ,derived-data-dir t ,project-pattern))
              (when (file-directory-p dir)
                (delete-directory dir t)
                (setq cleaned-count (1+ cleaned-count))))
            (format "Cleaned %d derived data folder(s) for %s" cleaned-count ,project-name)))
       (lambda (result)
         (message "%s" result))))))

(defun xcode-additions:deep-clean ()
  "Perform a deep clean: build folder, Swift package caches, and all derived data."
  (interactive)
  (let ((root (xcode-additions:project-root)))
    (unless root
      (error "Not in an Xcode project"))
    
    (when (yes-or-no-p "Perform deep clean? This will delete .build, Swift caches, and ALL derived data.")
      ;; Clean .build folder
      (xcode-additions:clean-build-folder-with
       :root root
       :build-folder (expand-file-name ".build" root)
       :project-name (xcode-additions:product-name)
       :ignore-list xcode-additions:clean-build-ignore-list)
      
      ;; Clean Swift package caches
      (xcode-additions:clean-swift-package-caches)
      
      ;; Clean ALL derived data (not just project-specific)
      (let ((derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData")))
        (when (file-exists-p derived-data-dir)
          (message "Cleaning all derived data...")
          (async-start
           `(lambda ()
              (dolist (dir (directory-files ,derived-data-dir t "^[^.]"))
                (when (and (file-directory-p dir)
                           (not (string-match-p "ModuleCache" dir)))
                  (delete-directory dir t)))
              "All derived data cleaned")
           (lambda (result)
             (message "%s" result))))))))

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
       ;; Check for .compile.lock error and clear it
       ((string-match "\\.compile\\.lock.*locked so long" line)
        (let ((msg "Clearing compile lock..."))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages)
            ;; Clear the lock file
            (let ((lock-file (concat (xcode-additions:project-root) "/.compile.lock")))
              (when (file-exists-p lock-file)
                (delete-file lock-file)
                (message "Cleared .compile.lock file"))))))
       ;; Resolving packages
       ((string-match "Resolve Package Graph" line)
        (let ((msg "Resolving packages..."))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'success)))
            (puthash msg t seen-messages))))
       ;; Fetching packages
       ((string-match "Fetching from \\(https://github.com/[^/]+/\\([^/ ]+\\)\\)" line)
        (let* ((url (match-string 1 line))
               (package-name (match-string 2 line))
               ;; Clean up package name (remove .git suffix if present)
               (clean-name (if (string-suffix-p ".git" package-name)
                              (substring package-name 0 -4)
                            package-name))
               (msg (format "Fetching package: %s" clean-name)))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-string-face)))
            (puthash msg t seen-messages))))
       ;; Package resolution details
       ((string-match "Resolved source packages:" line)
        (let ((msg "Package resolution complete"))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'success)))
            (puthash msg t seen-messages))))
       ;; Building specific targets
       ((string-match "Build target \\([^ ]+\\)" line)
        (let ((msg (format "Building target: %s" (match-string 1 line))))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-builtin-face)))
            (puthash msg t seen-messages))))
       ;; Linking
       ((string-match "Ld \\(.+\\)/\\([^/]+\\)$" line)
        (let ((msg (format "Linking: %s" (match-string 2 line))))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-keyword-face)))
            (puthash msg t seen-messages))))
       ;; Code signing
       ((string-match "CodeSign \\(.+\\)/\\([^/]+\\)$" line)
        (let ((msg (format "Signing: %s" (match-string 2 line))))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-constant-face)))
            (puthash msg t seen-messages))))
       ;; C compilation
       ((string-match "CompileC \\(.+\\)/\\([^/]+\\)$" line)
        (let ((msg (format "Compiling C: %s" (match-string 2 line))))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ((string-match "CompileSwiftModule \\([^ ]+\\)" line)
        (let ((msg (format "Compiling Swift: %s" (match-string 1 line))))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ;; Swift files
       ((string-match "CompileSwift.*\\([^/]+\\.swift\\)" line)
        (let ((msg (format "Swift: %s" (match-string 1 line))))
          (unless (gethash msg seen-messages)
            (xcode-additions:safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-type-face)))
            (puthash msg t seen-messages))))
       ;; Errors, warnings, notes
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

;; Build Process Interrupt Functions

;;;###autoload
(defun xcode-additions:interrupt-build ()
  "Interrupt the currently active xcodebuild process.
Also checks for and handles compile.lock errors that can halt builds."
  (interactive)
  (require 'swift-additions)
  
  ;; First check for compile.lock errors
  (let ((has-lock-error nil))
    (dolist (buffer-name '("*Swift Build*" "*compilation*" "*xcodebuild*"))
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (save-excursion
            (goto-char (point-min))
            (when (search-forward ".compile.lock exists! parse already run" nil t)
              (setq has-lock-error t)
              (message "Detected compile.lock error in %s - this can cause hanging builds" buffer-name))))))
    
    (when has-lock-error
      (message "Compile.lock error detected - will clean derived data after interrupting build")))
  
  (cond
   ;; Check for active swift-additions build process
   ((and (boundp 'swift-additions:active-build-process)
         swift-additions:active-build-process
         (process-live-p swift-additions:active-build-process))
    (message "Interrupting active build process...")
    (interrupt-process swift-additions:active-build-process)
    ;; Give it a moment, then kill if still alive
    (run-with-timer 2 nil
                    (lambda ()
                      (when (and swift-additions:active-build-process
                                 (process-live-p swift-additions:active-build-process))
                        (kill-process swift-additions:active-build-process)
                        (message "Build process killed"))))
    (setq swift-additions:active-build-process nil)
    (when (boundp 'swift-additions:active-build-buffer)
      (setq swift-additions:active-build-buffer nil))
    (when (and (boundp 'swift-additions:build-progress-spinner)
               swift-additions:build-progress-spinner)
      (spinner-stop swift-additions:build-progress-spinner)))
   
   ;; Check for compilation buffer with active process
   ((get-buffer "*Swift Build*")
    (let ((proc (get-buffer-process (get-buffer "*Swift Build*"))))
      (if (and proc (process-live-p proc))
          (progn
            (message "Interrupting build in *Swift Build* buffer...")
            (interrupt-process proc)
            (run-with-timer 2 nil
                            (lambda ()
                              (when (and proc (process-live-p proc))
                                (kill-process proc)
                                (message "Build process killed")))))
        (message "No active build process found in *Swift Build* buffer"))))
   
   ;; Fall back to killing any xcodebuild processes
   (t
    (let ((killed-count (xcode-additions:kill-all-xcodebuild-processes)))
      (if (> killed-count 0)
          (message "Killed %d xcodebuild process(es)" killed-count)
        (message "No active build processes found"))))))

;;;###autoload
(defun xcode-additions:kill-all-xcodebuild-processes ()
  "Kill all xcodebuild processes system-wide.
Returns the number of processes killed."
  (interactive)
  (let ((killed-count 0))
    ;; Kill all xcodebuild processes
    (dolist (pid (split-string 
                  (shell-command-to-string "pgrep -f xcodebuild") "\n" t))
      (when (string-match-p "^[0-9]+$" pid)
        (condition-case nil
            (progn
              (call-process "kill" nil nil nil "-TERM" pid)
              (cl-incf killed-count)
              (message "Sent TERM signal to xcodebuild process %s" pid))
          (error nil))))
    
    ;; If we killed any, wait a bit then force kill any survivors
    (when (> killed-count 0)
      (run-with-timer 3 nil
                      (lambda ()
                        (dolist (pid (split-string 
                                      (shell-command-to-string "pgrep -f xcodebuild") "\n" t))
                          (when (string-match-p "^[0-9]+$" pid)
                            (condition-case nil
                                (progn
                                  (call-process "kill" nil nil nil "-KILL" pid)
                                  (message "Force killed stubborn xcodebuild process %s" pid))
                              (error nil)))))))
    
    (when (called-interactively-p 'interactive)
      (if (> killed-count 0)
          (message "Killed %d xcodebuild process(es)" killed-count)
        (message "No xcodebuild processes found")))
    killed-count))

;;;###autoload
(defun xcode-additions:check-compile-lock-error ()
  "Check if build output contains the compile.lock error that can halt builds."
  (interactive)
  (let ((found-error nil)
        (buffers-to-check '("*Swift Build*" "*compilation*" "*xcodebuild*")))
    (dolist (buffer-name buffers-to-check)
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (save-excursion
            (goto-char (point-min))
            (when (search-forward ".compile.lock exists! parse already run" nil t)
              (setq found-error t)
              (message "Found compile.lock error in %s buffer - this can halt builds!" buffer-name))))))
    (if found-error
        (when (yes-or-no-p "Compile.lock error detected.  Kill build processes and clean derived data?")
          (xcode-additions:interrupt-build)
          (message "Interrupted build. Consider running clean derived data."))
      (message "No compile.lock errors found in build buffers"))))

;;;###autoload
(defun xcode-additions:build-status ()
  "Show status of current build process."
  (interactive)
  (require 'swift-additions)
  ;; First check for compile.lock errors
  (let ((has-lock-error nil))
    (dolist (buffer-name '("*Swift Build*" "*compilation*" "*xcodebuild*"))
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (save-excursion
            (goto-char (point-min))
            (when (search-forward ".compile.lock exists! parse already run" nil t)
              (setq has-lock-error t))))))
    
    (cond
     ;; Show lock error warning if found
     (has-lock-error
      (message "⚠️  Build may be stuck due to compile.lock error - use `xcode-additions:interrupt-build` to fix"))
     
     ((and (boundp 'swift-additions:active-build-process)
           swift-additions:active-build-process
           (process-live-p swift-additions:active-build-process))
      (message "Build active: %s (PID: %s) Buffer: %s"
               (process-name swift-additions:active-build-process)
               (process-id swift-additions:active-build-process)
               (or (and (boundp 'swift-additions:active-build-buffer)
                        swift-additions:active-build-buffer) 
                   "Unknown")))
     
     ((get-buffer "*Swift Build*")
      (let ((proc (get-buffer-process (get-buffer "*Swift Build*"))))
        (if (and proc (process-live-p proc))
            (message "Build active in *Swift Build* buffer (PID: %s)" (process-id proc))
          (message "No active build process (buffer exists but no process)"))))
     
     (t (message "No active build process")))))

(provide 'xcode-additions)
;;; xcode-additions.el ends here
