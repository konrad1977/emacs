;;; xcode-additions.el --- package for compiling and running swift apps in  -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(require 'project)
(require 'mode-line-hud)
(require 'xcodebuildserver)

(defvar current-project-root nil)
(defvar current-xcode-scheme nil)
(defvar current-build-configuration nil)
(defvar current-app-identifier nil)
(defvar current-build-folder nil)
(defvar current-is-xcode-project nil)
(defvar current-local-device-id nil)
(defvar current-run-on-device nil)

(defconst xcodebuild-list-config-command "xcrun xcodebuild -list -json")

(defgroup xcode-additions:xcodebuild nil
  "REPL."
  :tag "xcode-additions:xcodebuild"
  :group 'xcode-additions)

(defvar xcode-additions:debug t
  "Debug flag.")

(defconst xcodeproject-extension ".*\\.xcodeproj$"
  "Xcode project extensions.")

(defconst workspace-extension ".*\\.xcworkspace$"
  "Xcode workspace extensions.")

(defun xcode-additions:filename-by-extension (extension directory)
  "Get filename based on (as EXTENSION)."
  (if-let* ((name (directory-files directory t extension)))
      (file-name-sans-extension (file-name-nondirectory (car name)))))

(defun xcode-additions:project-directory-p (directory)
  "Check if xcodeproj file exists in (DIRECTORY)."
  (consp (directory-files directory nil xcodeproject-extension)))

(defun xcode-additions:workspace-directory-p (directory)
  "Check if xcodeproj file exists in (DIRECTORY)."
  (consp (directory-files directory nil workspace-extension)))

(defun xcode-additions:find-xcode-project-directory (&optional directory)
  "Try to find xcode project in (DIRECTORY)."
  (xcode-additions:find-ancestor-or-self-directory 'xcode-additions:project-directory-p directory))

(defun xcode-additions:find-workspace-directory (&optional directory)
  "Try to find xcode workspace in (DIRECTORY)."
  (xcode-additions:find-ancestor-or-self-directory 'xcode-additions:workspace-directory-p directory))

(defun xcode-additions:find-ancestor-or-self-directory (predicate &optional directory)
  "Find"
  (unless directory (setq directory default-directory))
  (if (funcall predicate directory)
      directory
    (let ((parent (file-name-directory (directory-file-name directory))))
      (if (or (null parent) (string-equal parent directory))
          nil
        (xcode-additions:find-ancestor-or-self-directory predicate parent)))))

(defun xcode-additions:workspace-name ()
  "Get the workspace name."
  (if-let* ((default-directory (xcode-additions:find-workspace-directory)))
      (xcode-additions:filename-by-extension workspace-extension default-directory)))

(defun xcode-additions:project-name ()
  "Get the workspace name."
  (if-let* ((default-directory (xcode-additions:find-xcode-project-directory)))
      (xcode-additions:filename-by-extension xcodeproject-extension default-directory)))

(defun xcode-additions:list-xcscheme-files (folder)
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of FOLDER."
  (let ((project-name (xcode-additions:project-name))
        (xcscheme-names '()))
    (setq folder (expand-file-name folder))
    (setq xcshemes-folder (concat folder "xcshareddata/xcschemes/"))
    (when (file-directory-p xcshemes-folder)
      (dolist (item (directory-files xcshemes-folder t))
        (when (and (file-regular-p item)
                   (string-match-p ".*\\.xcscheme$" item))
          (setq xcscheme-names (cons (file-name-sans-extension (file-name-nondirectory item)) xcscheme-names)))))
    (if xcscheme-names
        (setq xcscheme-names (nreverse xcscheme-names))
      (setq xcscheme-names (list project-name)))
    xcscheme-names))

(defun xcode-additions:list-scheme-files ()
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of the current Xcode project or workspace directory."
  (let* ((project-name (concat (xcode-additions:project-name) ".xcodeproj/"))
         (project-directory (concat (xcode-additions:find-xcode-project-directory) project-name)))
    (cond
     (project-directory
      (let ((xcscheme-files (xcode-additions:list-xcscheme-files project-directory)))
        (if xcscheme-files
            xcscheme-files
          ))))))

(defun xcode-additions:get-bundle-identifier (config)
  "Get bundle identifier (as CONFIG)."
  (unless current-project-root
    (setq current-project-root (xcode-additions:project-root)))

  (setq-local default-directory current-project-root)
  (let ((json (call-process-to-json "xcrun" "xcodebuild" "-showBuildSettings" "-configuration" config "-json")))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_BUNDLE_IDENTIFIER)))

(cl-defun xcode-additions:build-menu (&key title &key list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) (cons item item)) list))
             (choice (completing-read title choices)))
        (cdr (assoc choice choices))))))

(defun xcode-additions:parse-build-folder (directory)
  "Parse build folders from (as DIRECTORY)."
  ;; check if directory exists first
    (if (file-directory-p directory)
        (let* ((folders (directory-files directory nil "^[^.].*" t)))
            (mapc (lambda (folder)
                    (when (file-directory-p folder)
                    (file-name-nondirectory folder)))
                  folders))
      nil))

(defun xcode-additions:project-root ()
  "Get the project root."
  (unless current-project-root
    (setq current-project-root (cdr (project-current)))
    (setq-local defualt-directory current-project-root))
  current-project-root)

(defun xcode-additions:scheme ()
  "Get the xcode scheme if set otherwuse prompt user."
  (unless current-xcode-scheme
    (setq current-xcode-scheme (xcode-additions:build-menu :title "Choose scheme: " :list (xcode-additions:get-scheme-list))))
  current-xcode-scheme)

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
  (unless current-project-root
    (mode-line-hud:update :message "Fetching build schemes")
    (setq current-project-root (xcode-additions:project-root)))
  (xcode-additions:list-scheme-files))

(cl-defun xcode-additions:build-folder (&key (device-type :device))
  "Get build folder. Automatically choose based on device type (iphoneos or iphonesimulator), or let the user choose if there are multiple options."
  (unless current-build-folder
    (let* ((default-directory (concat (xcode-additions:derived-data-path) "build/Build/Products/"))
          (all-folders (xcode-additions:parse-build-folder default-directory))
          (target-suffix (if (eq device-type :simulator) "iphonesimulator" "iphoneos"))
          (matching-folders (seq-filter (lambda (folder) (string-match-p target-suffix folder)) all-folders)))

      (when xcode-additions:debug
        (message "xcode-additions:build-folder:\nAll folders: %s" all-folders)
        (message "Matching folders: %s" matching-folders))

      (setq current-build-folder
            (cond
             ;; Only one matching folder, use it
             ((= (length matching-folders) 1)
              (car matching-folders))
             ;; Multiple matching folders, let user choose
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
        (setq current-build-folder (shell-quote-argument (concat default-directory current-build-folder "/"))))))
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
  (unless current-project-root
    (mode-line-hud:update :message "Fetching build configurations")
    (setq current-project-root (xcode-additions:project-root)))

  (let* ((default-directory current-project-root)
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
  (unless current-project-root
    (mode-line-hud:update :message "Fetching app targets")
    (setq current-project-root (xcode-additions:project-root)))

  (let* ((default-directory current-project-root)
         (json (xcode-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (targets (cdr (assoc 'targets project))))
    targets))

(defun xcode-additions:is-xcodeproject ()
  "Check if it's an Xcode project."
  (unless current-is-xcode-project
    (when-let ((root (xcode-additions:project-root)))
      (setq current-is-xcode-project
            (directory-files root nil "\\(?:\\.xcworkspace\\|\\.xcodeproj\\)$" t 1))))
  current-is-xcode-project)

(defun xcode-additions:setup-current-project (project)
  "Check if we have a new project (as PROJECT).  If true reset settings."
  (xcode-additions:check-root)

  (unless current-project-root
    (setq current-project-root project))
  (when (not (string= current-project-root project))
    (progn
      (xcode-additions:reset)
      (setq current-project-root project))))

(defun xcode-additions:setup-project ()
  "Setup the current project."
  (xcode-additions:setup-current-project (xcode-additions:project-root))
  (setq default-directory current-project-root))

(defun xcode-additions:check-root ()
"Check root of the project.  If its different reset the settings."
(when (not (string-equal current-project-root (cdr (project-current))))
  (xcode-additions:reset)
  (setq current-project-root (cdr (project-current)))))

(cl-defun xcode-additions:device-or-simulator-menu (&key title)
"Build device or simulator menu (as TITLE)."
(defconst deviceList '(("Simulator" nil)
                        ("Physical device" t)))
(progn
  (let* ((choices (seq-map (lambda (item) item) deviceList))
          (choice (completing-read title choices)))
    (car (cdr (assoc choice choices))))))

(defun xcode-addition:ask-for-device-or-simulator ()
"Show menu for runnings on simulator or device."
(interactive)
(when (ios-device:id)
  (setq current-run-on-device (xcode-additions:device-or-simulator-menu :title "Run on simulator or device?"))))

(defun xcode-additions:run-in-simulator ()
  "Run the app in simulator."
  (not current-run-on-device))

;;;###autoload
(defun xcode-additions:reset ()
  "Reset the current project root."
  (interactive)
  (ios-simulator:reset)
  (periphery-kill-buffer)
  (spinner-stop build-progress-spinner)
  (setq current-run-on-device nil)
  (setq current-local-device-id nil)
  (setq current-is-xcode-project nil)
  (setq current-build-folder nil)
  (setq current-app-identifier nil)
  (setq current-build-configuration nil)
  (setq current-project-root nil)
  (setq current-xcode-scheme nil)
  (mode-line-hud:update :message "Resetting configuration"))

;;;###autoload
(defun xcode-additions:setup-dape()
  "Setup dape."
  (interactive)
  (require 'dape)
  (add-to-list 'dape-configs
             `(ios
               modes (swift-mode)
               command-cwd dape-command-cwd
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
                                (format "xcrun simctl launch --wait-for-debugger --terminate-running-process %S %S"
                                        (plist-get config 'simulator-id)
                                        (plist-get config 'app-bundle-id)))
                               (code (call-process-shell-command command nil (current-buffer))))
                          (dape--repl-message (format "* Running: %S *" command))
                          (dape--repl-message (buffer-string))
                          (save-match-data
                            (if (and (zerop code)
                                     (progn (goto-char (point-min))
                                            (search-forward-regexp "[[:digit:]]+" nil t)))
                                (plist-put config :pid (string-to-number (match-string 0)))
                              (dape--repl-message (format "* Running: %S *" command))
                              (dape--repl-message (format "Failed to start simulator:\n%s" (buffer-string)))
                              (user-error "Failed to start simulator")))))
                      config))
               :type "lldb"
               :request "attach"
               :cwd ".")))

;;;###autoload
(defun xcode-additions:clean-build-folder ()
  "Clean app build folder."
  (interactive)
  (xcode-additions:clean-build-folder-with
   :root (xcode-additions:project-root)
   :build-folder "build"
 :project-name (xcode-additions:scheme)))

(cl-defun xcode-additions:clean-build-folder-with (&key root build-folder project-name)
  "Clean build folder with (as ROOT) (as BUILD-FOLDER) (as PROJECT-NAME) asynchronously."
  (when xcode-additions:debug
    (message "Cleaning build %s folder for %s" build-folder project-name))

  (let ((default-directory (concat root build-folder)))
    (if (file-directory-p default-directory)
        (progn
          (mode-line-hud:update
           :message (format "Cleaning build folder for %s"
                            (propertize project-name 'face 'warning)))
          (async-start
           `(lambda ()
              ,(async-inject-variables "default-directory")
              (defun delete-directory-recursive (dir)
                "Delete DIR and all files and directories under it."
                (cond
                 ((file-symlink-p dir) (delete-file dir))
                 ((file-directory-p dir)
                  (mapc #'delete-directory-recursive
                        (directory-files dir t "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
                  (delete-directory dir))
                 (t (delete-file dir))))
              (condition-case err
                  (progn
                    (delete-directory-recursive ,default-directory)
                    "Cleaning completed successfully")
                (error (format "Error during cleaning: %s" (error-message-string err)))))
           `(lambda (result)
              (mode-line-hud:notification
               :message (format "Cleaning result for %s: %s"
                                (propertize ,project-name 'face 'warning)
                                result)
               :seconds 5))))
      (mode-line-hud:notification
       :message (propertize "Build folder is empty or does not exist." 'face 'warning)
       :seconds 2))))

(defun xcode-additions:open-in-xcode ()
  "Open project in xcode."
  (interactive)
  (if-let ((default-directory (xcode-additions:project-root))
           (command "xed ."))
      (inhibit-sentinel-messages #'call-process-shell-command command)))

(defun xcode-additions:derived-data-path ()
  "Extract the DerivedData path from xcodebuild output."
  (xcode-additions:project-root))

  ;; (let* ((default-directory (xcode-additions:project-root))
  ;;        (xcodebuild-output
  ;;         (shell-command-to-string "xcodebuild -showBuildSettings | grep BUILT_PRODUCTS_DIR"))
  ;;        (built-products-dir
  ;;         (when (string-match "BUILT_PRODUCTS_DIR = \\(.*\\)" xcodebuild-output)
  ;;           (match-string 1 xcodebuild-output))))
  ;;   (when built-products-dir
  ;;     (replace-regexp-in-string
  ;;      "/Build/Products/.*$"
  ;;      ""
  ;;      built-products-dir))))

(defun xcode-additions:open-build-folder ()
  "Open build folder."
  (interactive)
    (let ((default-directory (xcode-additions:derived-data-path)))
        (if (file-directory-p default-directory)
            (dired default-directory)
        (message "No build folder found"))))

(provide 'xcode-additions)
;;; xcode-additions.el ends here
