;;; swift-additions.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; commentary:

;;; package for building and runnning ios/macos apps from Emacs

;;; code:
(require 'eglot)
(require 'projectile)
(require 'xcodebuildserver)
(require 'periphery)
(require 'periphery-helper)
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
(defvar DEBUG t)

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

(cl-defun build-app-command (&key sim-id)
"Xcodebuild with (as SIM-ID)."
(if current-build-command
    current-build-command
  (concat
    (swift-additions:xcodebuild-command)
    (format "%s \\" (xcode-additions:get-workspace-or-project))
    (format "-scheme '%s' \\" (xcode-additions:scheme))
    (format "-sdk %s \\" (swift-additions:get-current-sdk))
    (format "-configuration DEBUG \\")
    ;; (format "-jobs %s" (swift-additions:get-number-of-cores))
    (when sim-id
      (format "-destination 'generic/platform=iOS Simulator,id=%s' \\" sim-id))
    (when (and current-local-device-id (not (xcode-additions:run-in-simulator)))
      (format "-destination 'generic/platform=iOS' \\" ))
    "-hideShellScriptEnvironment \\"
    "-derivedDataPath build | xcode-build-server parse -avv")))
;; (format "BUILD_DIR=%s "  (xcode-additions:get-build-folder))

(defun swift-additions:compilation-time ()
"Get the time of the compilation."
(if-let ((end-time (current-time)))
    (format "%.1f" (float-time (time-subtract end-time compilation-time)))
  nil))

(defun swift-additions:run()
"Rerun already compiled and installed app."
(interactive)
(periphery-kill-buffer)

(let ((appname (ios-simulator:app-name-from :folder (xcode-additions:get-build-folder))))
  (if appname
      (ios-simulator:install-and-run-app
        :rootfolder (xcode-additions:project-root)
        :build-folder (xcode-additions:get-build-folder)
        :simulatorId (ios-simulator:load-simulator-id)
        :appIdentifier (xcode-additions:fetch-or-load-app-identifier))
    (swift-additions:compile-and-run))))

(defun swift-additions:run-app-after-build()
"Either in simulator or on physical."
(mode-line-hud:update :message (format "Built %s in %s seconds"
                                        (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                                        (propertize (swift-additions:compilation-time) 'face 'warning)))

(ios-simulator:install-and-run-app
  :rootfolder (xcode-additions:project-root)
  :build-folder (xcode-additions:get-build-folder)
  :simulatorId (ios-simulator:load-simulator-id)
  :appIdentifier (xcode-additions:fetch-or-load-app-identifier)))

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
(defun swift-additions:compile-and-run ()
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
    (ios-simulator:kill-buffer)
    (setq current-local-device-id (ios-device:id))
    (xcode-addition:ask-for-device-or-simulator)

(if (xcode-additions:is-xcodeproject)
    (progn
      (setq run-app-on-build runApp)
      (if (not (xcode-additions:run-in-simulator))
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

(xcode-additions:setup-project)
(ios-simulator:load-simulator-id)

(let ((default-directory (xcode-additions:project-root))
      (build-command (build-app-command :sim-id current-simulator-id))
      (run-app-on-build run))

  (spinner-start 'progress-bar-filled)
  (setq current-build-command build-command)
  (setq compilation-time (current-time))
  (setq build-progress-spinner spinner-current)

  (when DEBUG
    (message build-command))

  (xcodebuildserver:check-configuration :root current-project-root
                                        :workspace (xcode-additions:get-workspace-or-project)
                                        :scheme current-xcode-scheme)

  (mode-line-hud:update :message (format "Compiling %s|%s"
                                          (propertize current-xcode-scheme 'face 'font-lock-builtin-face)
                                          (propertize (ios-simulator:simulator-name) 'face 'font-lock-negation-char-face)))
  (async-start-command-to-string
    :command build-command
    :callback '(lambda (text)
                (spinner-stop build-progress-spinner)
                (if run-app-on-build
                    (swift-additions:check-for-errors text #'swift-additions:run-app-after-build)
                  (swift-additions:check-for-errors text #'swift-additions:successful-build))))))

(defun swift-additions:compile-and-run-on-device ()
"Compile and run on device."
(setq current-build-command nil)
(xcode-additions:setup-project)

(let ((default-directory (xcode-additions:project-root))
      (build-command (build-app-command :sim-id nil))
      (build-folder (xcode-additions:get-build-folder)))

  (spinner-start 'progress-bar-filled)
  (setq current-build-command build-command)
  (setq compilation-time (current-time))
  (setq build-progress-spinner spinner-current)

  (mode-line-hud:update :message (format "Compiling %s|%s"
                                          (propertize current-xcode-scheme 'face 'font-lock-builtin-face)
                                          (propertize "Physical Device" 'face 'font-lock-negation-char-face)))

  (when DEBUG
    (message current-build-command)
    (message "Build-folder: %s" build-folder))

  (xcodebuildserver:check-configuration :root current-project-root
                                        :workspace (xcode-additions:get-workspace-or-project)
                                        :scheme current-xcode-scheme)

  (spinner-start 'progress-bar-filled)

  (async-start-command-to-string
    :command build-command
    :callback '(lambda (text)
                (spinner-stop build-progress-spinner)
                (if run-app-on-build
                    (ios-device:install-app
                      :buildfolder current-build-folder
                      :appIdentifier (xcode-additions:fetch-or-load-app-identifier)))
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
(swift-additions:clean-build-folder-with (xcode-additions:project-root) "/build" (xcode-additions:scheme)))

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

(defun swift-additions:insert-text-and-go-to-eol (text)
"Function that that insert (as TEXT) and go to end of line."
(save-excursion
  (indent-for-tab-command)
  (insert text)
  (move-end-of-line nil))
(goto-char (point-at-eol))
(evil-insert-state t))

(cl-defun swift-additions:build-menu (&key title &key list)
"Builds a widget menu from (as TITLE as LIST)."
(if (<= (length list) 1)
    (elt list 0)
  (progn
    (let* ((choices (seq-map (lambda (item) (cons item item)) list))
            (choice (completing-read title choices)))
      (cdr (assoc choice choices))))))

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
  "Face for enum case names in a pattern match."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment.special
  '((t :inherit tree-sitter-hl-face:comment
       :weight semi-bold))
  "Face for comments with some markup-like meaning, like MARK."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:operator.special
  '((t :inherit font-lock-negation-char-face
       :weight semi-bold))
  "Face for operators that need to stand out, like unary negation."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:punctuation.type
  '((t :inherit tree-sitter-hl-face:type
       :weight normal))
  "Face for punctuation in type names or annotations."
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
  "Face for compile-time keywords."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.type
  '((t :inherit tree-sitter-hl-face:keyword))
  "Face for keywords that appear in type annotations."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable.synthesized
  '((t :inherit tree-sitter-hl-face:variable))
  "Face for compiler-synthesized identifiers."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:default
  '((t :inherit default))
  "Face to override other faces, forcing the base display attributes."
  :group 'tree-sitter-hl-faces)

(provide 'swift-additions)

;;; swift-additions.el ends here
