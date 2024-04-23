;;; xcode-additions.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(require 'project)
(require 'mode-line-hud)

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
  (let ((xcscheme-names '()))
    (setq folder (expand-file-name folder))
    (setq xcshemes-folder (concat folder "xcshareddata/xcschemes/"))
    (when (file-directory-p xcshemes-folder)
      (dolist (item (directory-files xcshemes-folder t))
        (when (and (file-regular-p item)
                   (string-match-p ".*\\.xcscheme$" item))
          (setq xcscheme-names (cons (file-name-sans-extension (file-name-nondirectory item)) xcscheme-names)))))
    (if xcscheme-names
        (setq xcscheme-names (nreverse xcscheme-names))
      (message "No '.xcscheme' files found in %s" xcshemes-folder))
    xcscheme-names))

(defun xcode-additions:list-scheme-files ()
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of the current Xcode project or workspace directory."
  (let* ((xcshemes '())
         (project-name (concat (xcode-additions:project-name) ".xcodeproj/"))
         (project-directory (concat (xcode-additions:find-xcode-project-directory) project-name)))
    (cond
     (project-directory
      (let ((xcscheme-files (xcode-additions:list-xcscheme-files project-directory)))
        (if xcscheme-files
            xcscheme-files
          nil))))))

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

(defun xcode-additions:build-folder ()
  "Get build folder.  If there are more than one let the user choose wich one to use."
  (unless current-build-folder
    (setq current-build-folder
    (if-let* ((default-directory (concat (xcode-additions:project-root) "build/Build/Products/"))
              (choosen-folder (xcode-additions:build-menu :title "Choose build folder" :list (xcode-additions:parse-build-folder default-directory))))
        (shell-quote-argument (concat default-directory choosen-folder "/")))))
  current-build-folder)

(defun xcode-additions:get-workspace-or-project ()
  "Check if there is workspace or project."
  (let ((workspace (xcode-additions:workspace-name))
        (projectname (xcode-additions:project-name)))
    (if workspace
        (format "-workspace %s.xcworkspace" workspace)
      (format "-project %s.xcodeproj" projectname))))

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
  "Check if its an xcode-project."
  (unless current-is-xcode-project
    (if-let ((default-directory (xcode-additions:project-root)))
        (setq current-is-xcode-project
        (or
         (directory-files-recursively default-directory "\\xcworkspace$" t)
         (directory-files-recursively default-directory "\\xcodeproj$" t)))))
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
(when current-local-device-id
  (setq current-run-on-device (xcode-additions:device-or-simulator-menu :title "Run on simulator or device?"))))

(defun xcode-additions:run-in-simulator ()
  "Run the app in simulator."
  (not current-run-on-device))

;;;###autoload
(defun xcode-additions:reset ()
  "Reset the current project root."
  (interactive)
  (ios-simulator:kill-buffer)
  (periphery-kill-buffer)
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
(defun xcode-additions:clean-build-folder ()
"Clean app build folder."
(interactive)
(xcode-additions:clean-build-folder-with (periphery-helper:project-root-dir) ".build" "swift package")
(xcode-additions:clean-build-folder-with (xcode-additions:project-root) "/build" (xcode-additions:scheme)))

(defun xcode-additions:clean-build-folder-with (projectRoot buildFolder projectName)
"Clean build folder with PROJECTROOT BUILDFOLDER and PROJECTNAME."
(mode-line-hud:update
  :message (format "Cleaning build folder for %s"
                  (propertize projectName 'face 'warning)))

(let ((default-directory (concat projectRoot buildFolder)))
  (when (file-directory-p default-directory)
    (delete-directory default-directory t nil)))

(mode-line-hud:update
  :message (format "Cleaning done for %s"
                  (propertize projectName 'face 'warning))))

(provide 'xcode-additions)
;;; xcode-additions.el ends here
