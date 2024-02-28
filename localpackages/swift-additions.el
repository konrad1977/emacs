;;; swift-additions.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; commentary:

;;; package for building and runnning ios/macos apps from Emacs

;;; code:
(require 'eglot)
(require 'projectile)
(require 'periphery-helper)
(require 'xcodebuildserver)
(require 'periphery)
(require 'ios-simulator)
(require 'ios-device)
(require 'spinner)
(require 'xcode-additions)
(require 'mode-line-hud)

(defgroup swift-additions:xcodebuild nil
  "REPL."
  :tag "swift-additions:xcodebuild"
  :group 'swift-additions)

(defconst xcodebuild-list-config-command "xcrun xcodebuild -list -json")

(defvar current-root nil)
(defvar current-xcode-scheme nil)
(defvar current-app-identifier nil)
(defvar current-project-root nil)
(defvar current-build-configuration nil)
(defvar current-build-folder nil)
(defvar current-environment-x86 nil)
(defvar current-simulator-id nil)
(defvar current-simulator-name nil)
(defvar current-buildconfiguration-json-data nil)
(defvar current-local-device-id nil)
(defvar current-build-command nil)
(defvar build-progress-spinner nil)
(defvar current-is-xcode-project nil)
(defvar run-on-device nil)
(defvar run-app-on-build t)
(defvar compilation-time nil)
(defvar DEBUG nil)

(defun swift-additions:fetch-or-load-xcode-scheme ()
  "Get the xcode scheme if set otherwuse prompt user."
  (unless current-xcode-scheme
    (setq current-xcode-scheme (swift-additions:build-menu :title "Choose scheme: " :list (swift-additions:get-scheme-list))))
  current-xcode-scheme)

(defun swift-additions:fetch-or-load-build-configuration ()
  "Get the build configuration or promp user."
  (setq current-build-configuration "Debug")
  ;; (unless current-build-configuration
  ;;   (setq current-build-configuration (swift-additions:build-menu :title "Choose configuration: " :list (swift-additions:get-configuration-list))))
  current-build-configuration)


(defun swift-additions:fetch-or-load-app-identifier ()
  "Get the app identifier for the current configiration."
  (unless current-app-identifier
    (setq current-app-identifier (swift-additions:get-bundle-identifier (swift-additions:fetch-or-load-build-configuration))))
  current-app-identifier)

(defun swift-additions:setup-current-project (project)
  "Check if we have a new project (as PROJECT).  If true reset settings."
  (unless current-project-root
    (setq current-project-root project))
  (when (not (string= current-project-root project))
      (progn
        (swift-additions:reset-settings)
        (setq current-project-root project))))

(defun swift-additions:xcodebuild-command ()
  "Use x86 environement."
  (if current-environment-x86
      "env /usr/bin/arch -x86_64 xcrun xcodebuild build \\"
    "xcrun xcodebuild build \\"))

(defun swift-additions:get-build-folder ()
  "Get build folder. If there are more than one let the user choose wich one to use."
  (unless current-build-folder
    (setq current-build-folder
    (if-let* ((default-directory (concat (swift-additions:get-ios-project-root) "/build/Build/Products/"))
              (choosen-folder (swift-additions:build-menu :title "Choose build folder" :list (swift-additions:parse-build-folder default-directory))))
        (shell-quote-argument (concat default-directory choosen-folder "/")))))
  current-build-folder)

(defun swift-additions:parse-build-folder (directory)
  "Parse build folders from (as DIRECTORY)."
  (let ((folders (directory-files directory nil "^[^.].*" t)))
    (mapc (lambda (folder)
            (when (file-directory-p folder)
                 (expand-file-name folder directory)))
          folders)))

(defun swift-additions:get-number-of-cores ()
  "Fetch number of available cores."
  (if-let ((cores (replace-regexp-in-string "\n$" "" (shell-command-to-string "sysctl -n hw.ncpu"))))
      cores
    2))

(defun swift-additions:get-workspace-or-project ()
  "Check if there is workspace or project."
  (let ((workspace (xcode-additions:workspace-name))
        (projectname (xcode-additions:project-name)))
    (if workspace
        (format "-workspace %s.xcworkspace" workspace)
      (format "-project %s.xcodeproj" projectname))))

(cl-defun build-app-command (&key sim-id)
  "Xcodebuild with (as SIM-ID)."
  (if current-build-command
      current-build-command
    (concat
     (swift-additions:xcodebuild-command)
     (format "%s \\" (swift-additions:get-workspace-or-project))
     (format "-scheme '%s' \\" (swift-additions:fetch-or-load-xcode-scheme))
     (format "-sdk %s \\" (swift-additions:get-current-sdk))
     (format "-configuration DEBUG \\")
     ;; (format "-jobs %s" (swift-additions:get-number-of-cores))
     (when sim-id
       (format "-destination 'generic/platform=iOS Simulator,id=%s' \\" sim-id))
     (when (and current-local-device-id run-on-device)
       (format "-destination 'generic/platform=iOS' \\" ))
     "-hideShellScriptEnvironment \\"
     "-derivedDataPath build | xcode-build-server parse -avv")))
;; (format "BUILD_DIR=%s "  (swift-additions:get-build-folder))

(cl-defun swift-additions:build-device-or-simulator-menu (&key title)
  "Build device or simulator menu (as TITLE)."
  (defconst deviceList '(("Simulator" nil)
                         ("Physical device" t)))
  (progn
    (let* ((choices (seq-map (lambda (item) item) deviceList))
           (choice (completing-read title choices)))
      (car (cdr (assoc choice choices))))))

(defun swift-addition:ask-for-device-or-simulator ()
  "Show menu for runnings on simulator or device."
  (interactive)
  (when current-local-device-id
    (setq run-on-device (swift-additions:build-device-or-simulator-menu :title "Run on simulator or device?"))))

(defun swift-additions:compilation-time ()
  "Get the time of the compilation."
  (if-let ((end-time (current-time)))
      (format "%.1f" (float-time (time-subtract end-time compilation-time)))
    nil))

(defun swift-additions:re-run-app ()
  "Rerun already compiled and installed app."
  (interactive)
  (ios-simulator:install-and-run-app
   :rootfolder (swift-additions:get-ios-project-root)
   :build-folder (swift-additions:get-build-folder)
   :simulatorId (ios-simulator:load-simulator-id)
   :appIdentifier (swift-additions:fetch-or-load-app-identifier)))

(defun swift-additions:run-app()
  "Either in simulator or on physical."
  (mode-line-hud:update :message (format "Built %s in %s seconds"
                                         (propertize current-xcode-scheme 'face 'font-lock-builtin-face)
                                         (propertize (swift-additions:compilation-time) 'face 'warning)))

  (ios-simulator:install-and-run-app
   :rootfolder current-project-root
   :build-folder (swift-additions:get-build-folder)
   :simulatorId (ios-simulator:load-simulator-id)
   :appIdentifier (swift-additions:fetch-or-load-app-identifier)))

(defun swift-additions:check-if-build-was-successful (input-text)
  "Check if INPUT-TEXT does not contain 'BUILD FAILED' or 'BUILD INTERRUPTED' from the end."
  (when DEBUG (message input-text))
  (and (not (string-match-p "BUILD FAILED" input-text))
       (not (string-match-p "BUILD INTERRUPTED" input-text))
       (not (string-match-p "xcodebuild: error" input-text))))

(defun swift-additions:check-for-errors (output callback)
  "Run periphery parser on TEXT (optional as OUTPUT CALLBACK)."
  (when (swift-additions:check-if-build-was-successful output)
    (funcall callback))
  (periphery-run-parser output))

(defun swift-additions:get-project-files ()
  "Get project files."
  (let* ((root-dir (periphery-helper:project-root-dir))
         (files (directory-files-recursively root-dir "\.xcworkspace$\\|\\.xcodeproj$" t))
         (file-list (cdr-safe files))
         (filtered-list (cl-remove-if (lambda (file)
                                        (string-match-p "/\.build/" file))
                                      file-list)))
    (periphery-helper:filter-xcworkspace filtered-list)))

(defun swift-additions:get-ios-project-root ()
  "Get the ios-project root."
  (unless current-project-root
    (setq current-project-root (cdr (project-current))))
  current-project-root)

(defun swift-additions:get-current-sdk ()
  "Return the current SDK."
  (if current-local-device-id
      "iphoneos"
    "iphonesimulator"))

;;;###autoload
(defun swift-additions:reset-settings ()
  "Reset current settings.  Change current configuration."
  (interactive)
  (ios-simulator:kill-buffer)
  (periphery-kill-buffer)
  (setq current-xcode-scheme nil)
  (setq current-app-identifier nil)
  (setq current-app-name nil)
  (setq current-project-root nil)
  (setq current-build-configuration nil)
  (setq current-simulator-id nil)
  (setq current-simulator-name nil)
  (setq current-buildconfiguration-json-data nil)
  (setq current-local-device-id nil)
  (setq current-build-command nil)
  (setq current-build-folder nil)
  (mode-line-hud:update :message "Resetting configuration"))

(defun swift-additions:successful-build ()
  "Show that the build was successful."
  (message-with-color :tag "[Build]" :text "Successful" :attributes 'success))

;;;###autoload
(defun swift-additions:run-without-compiling ()
  "Run app in simulator/device without compiling."
  (interactive)
  (periphery-kill-buffer)
  (ios-simulator:kill-buffer)
  (swift-additions:run-app))

;;;###autoload
(defun swift-additions:compile-and-run-app ()
  "Compile and run app."
  (interactive)
  (swift-additions:compile-and-run-silent t))

;;;###autoload
(defun swift-additions:compile-app ()
  "Compile app."
  (interactive)
  (swift-additions:compile-and-run-silent nil))

(defun swift-additions:compile-and-run-silent (runApp)
  "Build project using xcodebuild (as RUNAPP)."
   (let ((savings (save-some-buffers t))
         (buffer (periphery-kill-buffer)))
     (swift-additions:check-root)
     (ios-simulator:kill-buffer)
     (setq current-local-device-id (ios-device:id))
     (swift-addition:ask-for-device-or-simulator)

  (if (swift-additions:is-xcodeproject)
      (progn
        (setq run-app-on-build runApp)
        (if run-on-device
            (progn
              (setq device-or-simulator "physical device")
              (swift-additions:compile-and-run-on-device))
          ;; Simulator
          (swift-additions:compile-app-for-simulator :run runApp)))

    (if (swift-additions:is-a-swift-package-base-project)
        (swift-additions:build-swift-package)
      (message "Not xcodeproject nor swift package")))))

(cl-defun swift-additions:compile-app-for-simulator (&key run)
  "Compile app (RUN)."

  (swift-additions:setup-current-project (swift-additions:get-ios-project-root))
  (ios-simulator:load-simulator-id)
  (setq current-simulator-name (ios-simulator:simulator-name))

  (let ((default-directory current-project-root)
        (build-command (build-app-command :sim-id current-simulator-id))
        (run-app-on-build run))

    (setq current-build-command build-command)
    (setq compilation-time (current-time))

    (xcodebuildserver:check-configuration :root current-project-root
                                          :workspace (swift-additions:get-workspace-or-project)
                                          :scheme current-xcode-scheme)
    (when DEBUG
      (message build-command))

    (spinner-start 'progress-bar-filled)
    (setq build-progress-spinner spinner-current)

    (mode-line-hud:update :message (format "Compiling %s/%s"
                                           (propertize current-xcode-scheme 'face 'font-lock-builtin-face)
                                           (propertize current-simulator-name 'face 'font-lock-negation-char-face)))

    (async-start-command-to-string
     :command build-command
     :callback '(lambda (text)
                  (spinner-stop build-progress-spinner)
                  (if run-app-on-build
                      (swift-additions:check-for-errors text #'swift-additions:run-app)
                    (swift-additions:check-for-errors text #'swift-additions:successful-build))))))

(defun swift-additions:compile-and-run-on-device ()
  "Compile and run on device."
  (swift-additions:setup-current-project (swift-additions:get-ios-project-root))

  (message-with-color
   :tag "[Preparing]"
   :text "Fetching build information..."
   :attributes '(:inherit warning))

  (let ((default-directory current-project-root)
        (build-command (build-app-command :sim-id nil))
        (build-folder (swift-additions:get-build-folder)))
    (setq current-build-command build-command)
    (setq current-build-folder build-folder)

    (when DEBUG
      (message current-build-command)
      (message "Build-folder: %s" current-build-folder))

    (async-start-command-to-string
     :command build-command
     :callback '(lambda (text)
                  (if run-app-on-build
                      (ios-device:install-app
                       :project-root current-project-root
                       :buildfolder current-build-folder
                       :appname (ios-simulator:app-name-from :folder current-build-folder)))
                    (swift-additions:check-for-errors text #'swift-additions:successful-build)))))

;;;###autoload
(defun swift-additions:test-module-silent ()
  "Test module."
  (interactive)
  (save-some-buffers t)
  (periphery-kill-buffer)
  (ios-simulator:kill-buffer)
  (swift-additions:test-swift-package))

;;;###autoload
(defun swift-additions:clean-build-folder ()
  "Clean app build folder."
  (interactive)
  (swift-additions:clean-build-folder-with (periphery-helper:project-root-dir) ".build" "swift package")
  (swift-additions:clean-build-folder-with (swift-additions:get-ios-project-root) "/build" (swift-additions:fetch-or-load-xcode-scheme)))

(defun swift-additions:clean-build-folder-with (projectRoot buildFolder projectName)
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

(defun swift-additions:check-root ()
  "Check root of the project.  If its different reset the settings."
  (when (not (string-equal current-root (cdr (project-current))))
    (swift-additions:reset-settings)
    (setq current-root (cdr (project-current)))))

(defun swift-additions:insert-text-and-go-to-eol (text)
  "Function that that insert (as TEXT) and go to end of line."
  (save-excursion
    (indent-for-tab-command)
    (insert text)
    (move-end-of-line nil))
  (goto-char (point-at-eol))
  (evil-insert-state t))

;;;###autoload
(defun swift-additions:functions-and-pragmas ()
  "Show swift file compressed functions and pragmas."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "\\(#pragma mark\\)\\|\\(MARK:\\)")))

;;;###autoload
(defun swift-additions:print-thing-at-point ()
  "Print thing at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (end-of-line)
    (newline-and-indent)
    (insert (format "debugPrint(\"%s: \ \\(%s\)\")" word word))))

;;;###autoload
(defun swift-additions:insert-mark ()
  "Insert a mark at line."
  (interactive)
  (swift-additions:insert-text-and-go-to-eol "// MARK: - "))

;;;###autoload
(defun swift-additions:insert-todo ()
  "Insert a Todo."
  (interactive)
  (swift-additions:insert-text-and-go-to-eol "// TODO: "))

(defun swift-additions:get-bundle-identifier (config)
  "Get bundle identifier (as CONFIG)."
  (unless current-project-root
    (setq current-project-root (swift-additions:get-ios-project-root)))
  
  (let ((default-directory current-project-root)
        (json (call-process-to-json "xcrun" "xcodebuild" "-showBuildSettings" "-configuration" config "-json")))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_BUNDLE_IDENTIFIER)))

(defun swift-additions:get-buildconfiguration-json ()
  "Return a cached version or load the build configuration."
  (unless current-buildconfiguration-json-data
    (mode-line-hud:update :message "Fetching build configuration")
    (setq current-buildconfiguration-json-data (call-process-to-json xcodebuild-list-config-command)))
  current-buildconfiguration-json-data)

(defun swift-additions:get-target-list ()
  "Get list of project targets."
  (unless current-project-root
    (mode-line-hud:update :message "Fetching app targets")
    (setq current-project-root (swift-additions:get-ios-project-root)))

  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (targets (cdr (assoc 'targets project))))
    targets))

(defun swift-additions:get-scheme-list ()
  "Get list of project schemes."
  (unless current-project-root
    (mode-line-hud:update :message "Fetching build schemes")
    (setq current-project-root (swift-additions:get-ios-project-root)))
  (xcode-additions:list-scheme-files))

(defun swift-additions:get-configuration-list ()
  "Get list of project configurations."
  (unless current-project-root
    (mode-line-hud:update :message "Fetching build configurations")
    (setq current-project-root (swift-additions:get-ios-project-root)))
  
  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (result (cdr (assoc 'configurations project))))
    result))

(cl-defun swift-additions:build-menu (&key title &key list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) (cons item item)) list))
             (choice (completing-read title choices)))
        (cdr (assoc choice choices))))))

(defun swift-additions:is-xcodeproject ()
  "Check if its an xcode-project."
  (unless current-is-xcode-project
    (if-let ((default-directory (swift-additions:get-ios-project-root)))
        (setq current-is-xcode-project
        (or
         (directory-files-recursively default-directory "\\xcworkspace$" t)
         (directory-files-recursively default-directory "\\xcodeproj$" t)))))
    current-is-xcode-project)

(defun swift-additions:is-a-swift-package-base-project ()
  "Check if project is a swift package based."
  (let ((default-directory (periphery-helper:project-root-dir)))
    (file-exists-p "Package.swift")))

(defun swift-additions:check-for-spm-build-errors (text)
  "Check for Swift package build erros in TEXT."
  (when DEBUG (message text))
  (if (or
       (string-match-p (regexp-quote "error:") text)
       (string-match-p (regexp-quote "warning:") text))
      (progn
        (when (not (string-match-p (regexp-quote "error:") text))
          (swift-additions:run-async-swift-package)))
    (swift-additions:run-async-swift-package)))

(defun swift-additions:run-async-swift-package ()
  "Run async swift package and hide the normal output."
  (inhibit-sentinel-messages #'async-shell-command
                             "swift run"
                             "*Swift Package*"))

;;;###autoload
(defun swift-additions:build-swift-package ()
  "Build swift package module."
  (interactive)
  (let ((default-directory (periphery-helper:project-root-dir)))
    (swift-additions:reset-settings)
    (async-shell-command-to-string :process-name "periphery" :command "swift build" :callback #'swift-additions:check-for-spm-build-errors)
    (message-with-color :tag "[ Package]" :text (format "%s. Please wait. Patience is a virtue!" (periphery-helper:project-root-dir)) :attributes 'warning)))

;;;###autoload
(defun swift-additions:test-swift-package ()
  "Test swift package module."
  (interactive)
  (swift-additions:test-swift-package :root (periphery-helper:project-root-dir)))

;;;###autoload
(defun swift-additions:test-swift-package-from-file ()
  "Test swift package module."
  (interactive)
  (swift-additions:test-swift-package :root (swift-additions:detect-package-root)))

(cl-defun swift-additions:test-swift-package (&key root)
  "Test package in ROOT."
  (let ((default-directory root)
        (package-name (file-name-nondirectory (directory-file-name root))))
    (spinner-start 'progress-bar-filled)
    (setq build-progress-spinner spinner-current)
    (async-start-command-to-string
     :command "swift test"
     :callback '(lambda (text)
                  (spinner-stop build-progress-spinner)
                  (let ((filtered (periphery-helper:filter-keep-beginning-paths text)))
                    (periphery-run-test-parser filtered (lambda ()
                                                          (message-with-color
                                                           :tag "[All tests passed]"
                                                           :text ""
                                                           :attributes 'success))))))
    (message-with-color
     :tag (format "[Testing '%s'-package]" package-name)
     :text "Please wait. Patience is a virtue!"
     :attributes 'warning)))

(defun swift-additions:detect-package-root ()
  "Detects the root directory of the Swift package based on the current buffer."
  (let ((buffer-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (locate-dominating-file buffer-dir "Package.swift")))

(defun swift-additions:lsp-arguments ()
  "Get the lsp arguments to support UIKit."
  (let* ((sdk (ios-simulator:sdk-path))
         (target (ios-simulator:target)))
    (list
     ;; "--completion-max-results" "12"
     "-Xswiftc" "-sdk"
     "-Xswiftc" sdk
     "-Xswiftc" "-target"
     "-Xswiftc" target)))

(defun my-swift-mode:eglot-server-contact (_ignored)
  "Construct the list that eglot needs to start sourcekit-lsp."
  (setq arglist (swift-additions:lsp-arguments))
  (add-to-list 'arglist (clean-up-newlines (shell-command-to-string "xcrun --find sourcekit-lsp"))))

(require 'tree-sitter-hl)

(defface tree-sitter-hl-face:case-pattern
  '((t :inherit tree-sitter-hl-face:property))
  "Face for enum case names in a pattern match"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment.special
  '((t :inherit tree-sitter-hl-face:comment
       :weight semi-bold))
  "Face for comments with some markup-like meaning, like MARK"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:operator.special
  '((t :inherit font-lock-negation-char-face
       :weight semi-bold))
  "Face for operators that need to stand out, like unary negation"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:punctuation.type
  '((t :inherit tree-sitter-hl-face:type
       :weight normal))
  "Face for punctuation in type names or annotations"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:annotation
  '((t :inherit font-lock-keyword-face))
  "Face for annotations or attributes attached to declarations."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:annotation.builtin
  '((t :inherit tree-sitter-hl-face:annotation))
  "Face for declaration annotations which are built in to the language."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:annotation.type
  '((t :inherit tree-sitter-hl-face:annotation))
  "Face for annotations attached to type descriptors."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.annotation
  '((t :inherit tree-sitter-hl-face:annotation.builtin))
  "Face for subelements of annotations which are built in to the language."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.compiler
  '((t :inherit tree-sitter-hl-face:keyword
       :weight semi-bold))
  "Face for compile-time keywords"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.type
  '((t :inherit tree-sitter-hl-face:keyword))
  "Face for keywords that appear in type annotations"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable.synthesized
  '((t :inherit tree-sitter-hl-face:variable))
  "Face for compiler-synthesized identifiers"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:default
  '((t :inherit default))
  "Face to override other faces, forcing the base display
attributes."
  :group 'tree-sitter-hl-faces)

(provide 'swift-additions)

;;; swift-additions.el ends here
