;;; swift-additions.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Package for building and running iOS/macOS apps from Emacs

;;; Code:

(require 'ios-device)
(require 'ios-simulator)

(defgroup swift-additions nil
  "Swift development tools and utilities for Emacs."
  :group 'programming
  :prefix "swift-additions:")

(defgroup swift-additions:xcodebuild nil
  "XcodeBuild configuration settings."
  :tag "swift-additions:xcodebuild"
  :group 'swift-additions)

(defcustom swift-additions:default-configuration "Debug"
  "Default build configuration to use."
  :type 'string
  :group 'swift-additions:xcodebuild)

(defcustom swift-additions:modern-build-system "YES"
  "Whether to use the modern build system."
  :type '(choice (const "YES")
                 (const "NO"))
  :group 'swift-additions:xcodebuild)

(defcustom swift-additions:additional-build-flags '()
  "Additional flags to pass to xcodebuild."
  :type '(repeat string)
  :group 'swift-additions:xcodebuild)

(defvar swift-additions:optimization-level 'fast
  "Build optimization level.")

;; Internal variables
(defvar-local swift-additions:current-build-configuration nil)
(defvar-local swift-additions:current-build-folder nil)
(defvar-local swift-additions:current-environment-x86 nil)
(defvar-local swift-additions:current-local-device-id nil)
(defvar-local swift-additions:current-build-command nil)
(defvar-local swift-additions:build-progress-spinner nil)
(defvar-local swift-additions:compilation-time nil)

(defvar swift-additions:debug nil
  "Enable debug output when non-nil.")

(defun swift-additions:log-debug (format-string &rest args)
  "Log debug message using FORMAT-STRING and ARGS when debug is enabled."
  (when swift-additions:debug
    (apply #'message (concat "[Swift Debug] " format-string) args)))

(defun swift-additions:handle-build-error (error-message)
  "Handle build ERROR-MESSAGE and display appropriate feedback."
  (swift-additions:cleanup)
  (mode-line-hud:update
   :message (format "Build failed: %s"
                   (propertize (truncate-string-to-width error-message 50) 'face 'error)))
  (periphery-run-parser error-message))

(defun swift-additions:get-optimization-flags ()
  "Get optimization flags based on current optimization level."
      `("SWIFT_COMPILATION_MODE=wholemodule"
        "COMPILER_INDEX_STORE_ENABLE=NO"
        "DEBUG_INFORMATION_FORMAT=dwarf"
        "SWIFT_PARALLEL_MODULE_JOBS=8"
        "SWIFT_USE_PARALLEL_WHOLE_MODULE_OPTIMIZATION=YES"
        "SWIFT_ENABLE_BATCH_MODE=YES"
        "SWIFT_USE_PARALLEL_SOURCEJOB_TASKS=YES"
        "BUILD_LIBRARY_FOR_DISTRIBUTION=NO"))

(defun swift-additions:xcodebuild-command ()
"Use x86 environement."
(if swift-additions:current-environment-x86
    "env /usr/bin/arch -x86_64 xcrun xcodebuild build \\"
  "xcrun xcodebuild build \\"))

(defun swift-additions:setup-build-environment ()
  "Setup optimal environment variables for build."
  (setenv "SWIFT_DETERMINISTIC_HASHING" "1")
  (setenv "SWIFT_ENABLE_INCREMENTAL_COMPILATION" "1")
  (when (eq swift-additions:optimization-level 'fast)
    (setenv "SWIFT_DISABLE_SAFETY_CHECKS" "1"))
  (setenv "SWIFT_MEMORY_ALLOCATOR" "malloc")  ; Snabbare än standard
  (setenv "SWIFT_MAX_PARALLEL_LTO_JOBS"
          (number-to-string (max 1 (/ (num-processors) 2)))))

(cl-defun swift-additions:build-app-command (&key sim-id derived-path)
  "Xcodebuild with (as SIM-ID DERIVED-PATH)."
  (if swift-additions:current-build-command
      swift-additions:current-build-command
    (concat
     (swift-additions:xcodebuild-command)
     (format "%s \\" (xcode-additions:get-workspace-or-project))
     (format "-scheme %s \\" (xcode-additions:scheme))
     (format "-jobs %s \\" (swift-additions:get-optimal-jobs))
     (if sim-id
         (format "-destination 'generic/platform=iOS Simulator,id=%s' -sdk %s \\" sim-id "iphonesimulator")
       (format "-destination 'generic/platform=%s' -sdk %s \\" "iOS" "iphoneos"))
     (format "-configuration %s \\" swift-additions:default-configuration)
     "-parallelizeTargets \\"
     ;; (format "-UseModernBuildSystem=%s \\" swift-additions:modern-build-system)
     "-enableAddressSanitizer NO \\"
     "-enableThreadSanitizer NO \\"
     "-enableUndefinedBehaviorSanitizer NO \\"
     (mapconcat (lambda (flag) (concat flag " \\"))
                (append
                 (swift-additions:get-optimization-flags)
                 swift-additions:additional-build-flags)
                "")
     "-derivedDataPath .build | xcode-build-server parse -avv")))

(defun swift-additions:get-optimal-jobs ()
  "Get optimal number of parallel jobs based on CPU cores."
  (number-to-string (max 1 (- (num-processors) 1))))

(defun swift-additions:compilation-time ()
  "Get the time of the compilation."
  (if-let* ((end-time (current-time))
           (start-time swift-additions:compilation-time))
      (format "%.1f" (float-time (time-subtract end-time start-time)))
    "N/A"))


(defun swift-additions:run-app-on-device-after-build ()
  "Run app on device after build."
  (mode-line-hud:update :message (format "Built %s in %s seconds"
                                         (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                                         (propertize (swift-additions:compilation-time) 'face 'warning)))

  (ios-device:install-app
   :buildfolder (xcode-additions:build-folder :device-type :device)
   :appIdentifier (xcode-additions:fetch-or-load-app-identifier)))

(defun swift-additions:run-app-after-build()
  "Either in simulator or on physical."
  (mode-line-hud:update :message (format "Built %s in %s seconds"
                                         (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                                         (propertize (swift-additions:compilation-time) 'face 'warning)))

  (ios-simulator:install-and-run-app
   :rootfolder (xcode-additions:project-root)
   :build-folder (xcode-additions:build-folder :device-type :simulator)
   :simulatorId (ios-simulator:simulator-identifier)
   :appIdentifier (xcode-additions:fetch-or-load-app-identifier)))

(defun swift-additions:check-if-build-was-successful (input-text)
  "Check if INPUT-TEXT does not contain build failure indicators."
  (when swift-additions:debug (message input-text))
  (not (string-match-p "\\(BUILD FAILED\\|BUILD INTERRUPTED\\|xcodebuild: error\\)" input-text)))

(defun swift-additions:check-for-errors (output callback)
  "Run periphery parser on TEXT (optional as OUTPUT CALLBACK)."
  (when swift-additions:debug
    (message "checking for error: %s" output))
  (condition-case err
      (when (swift-additions:check-if-build-was-successful output)
        (funcall callback))
    (error
     (swift-additions:handle-build-error (error-message-string err))))
  (periphery-run-parser output))

(defun swift-additions:cleanup ()
  "Cleanup resources and state."
  (when swift-additions:build-progress-spinner
    (spinner-stop swift-additions:build-progress-spinner))
  (setq swift-additions:current-build-command nil
        swift-additions:compilation-time nil))

(defun swift-additions:successful-build ()
  "Show that the build was successful."
  (mode-line-hud:update :message (format "Successful build %s"
                                         (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face))))

(cl-defun swift-additions:compile-with-progress (&key command callback update-callback)
  "Run compilation COMMAND with progress indicator and CALLBACK/UPDATE-CALLBACK."
  (spinner-start 'progress-bar-filled)
  (setq swift-additions:build-progress-spinner spinner-current
        swift-additions:compilation-time (current-time))

  (condition-case err
      (async-start-command-to-string
       :command command
       :callback (lambda (text)
                  (spinner-stop swift-additions:build-progress-spinner)
                  (if (swift-additions:check-if-build-was-successful text)
                      (progn
                        (funcall callback text)
                        (swift-additions:cleanup))
                    (swift-additions:handle-build-error text)))
       :update-callback update-callback
       :debug swift-additions:debug)
    (error
     (swift-additions:handle-build-error (error-message-string err)))))


(cl-defun swift-additions:compile (&key run)
  "Build project using xcodebuild (as RUN)."
  (save-some-buffers t)

  (if (xcode-additions:is-xcodeproject)
      (progn
        (periphery-kill-buffer)
        (ios-simulator:kill-buffer)
        (xcode-addition:ask-for-device-or-simulator)
        (if (not (xcode-additions:run-in-simulator))
            (swift-additions:compile-for-device :run run)
          (swift-additions:compile-for-simulator :run run)))
    (if (swift-additions:is-a-swift-package-base-project)
        (swift-additions:build-swift-package)
      (message "Not xcodeproject nor swift package"))))

(cl-defun swift-additions:compile-for-simulator (&key run)
  "Compile app for simulator with optional RUN after completion."
  (swift-additions:cleanup)
  (swift-additions:setup-build-environment)
  (setq swift-additions:current-build-command nil)
  (xcode-additions:setup-project)
  (setq run-once-compiled run)

  (let ((build-command (swift-additions:build-app-command
                       :sim-id (ios-simulator:simulator-identifier)))
        (default-directory (xcode-additions:project-root)))

    (mode-line-hud:update
     :message (format "Building %s|%s"
                     (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                     (propertize (ios-simulator:simulator-name) 'face 'font-lock-negation-char-face)))

    (xcode-additions:setup-xcodebuildserver)

    (swift-additions:compile-with-progress
     :command build-command
     :callback (lambda (text)
                (if run-once-compiled
                    (swift-additions:check-for-errors text #'swift-additions:run-app-after-build)
                  (swift-additions:check-for-errors text #'swift-additions:successful-build)))
     :update-callback (lambda (text)
                       (xcode-additions:parse-compile-lines-output :input text)))))

(defun swift-additions:compile-for-device (&key run)
  "Compile and optionally RUN on device."
  (swift-additions:cleanup)
  (setq swift-additions:current-build-command nil)
  (xcode-additions:setup-project)
  (setq run-once-compiled run)

  (let ((build-command (swift-additions:build-app-command))
        (default-directory (xcode-additions:project-root)))

    (swift-additions:log-debug "Build-folder: %s" (xcode-additions:derived-data-path))

    (xcode-additions:setup-xcodebuildserver)
    (mode-line-hud:update
     :message (format "Compiling %s|%s"
                     (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                     (propertize "Physical Device" 'face 'font-lock-negation-char-face)))

    (swift-additions:compile-with-progress
     :command build-command
     :callback (lambda (text)
                (if run-once-compiled
                    (swift-additions:check-for-errors
                     text #'swift-additions:run-app-on-device-after-build)
                  (swift-additions:check-for-errors
                   text #'swift-additions:successful-build)))
     :update-callback (lambda (text)
                       (xcode-additions:parse-compile-lines-output :input text)))))


(defun swift-additions:is-a-swift-package-base-project ()
  "Check if project is a swift package based."
  (let ((default-directory (periphery-helper:project-root-dir)))
    (file-exists-p "Package.swift")))

(defun swift-additions:check-for-spm-build-errors (text)
  "Check for Swift package build erros in TEXT."
  (when swift-additions:debug (message text))
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

(defun swift-additions:build-swift-package ()
  "Build swift package module."
  (interactive)
  (let ((default-directory (periphery-helper:project-root-dir)))
    (xcode-additions:reset)
    (async-shell-command-to-string :process-name "periphery" :command "swift build" :callback #'swift-additions:check-for-spm-build-errors)
    (message-with-color :tag "[ Package]" :text (format "%s. Please wait. Patience is a virtue!" (periphery-helper:project-root-dir)) :attributes 'warning)))

(defun swift-additions:test-swift-package ()
  "Test swift package module."
  (interactive)
  (swift-additions:test-swift-package :root (periphery-helper:project-root-dir)))

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
     :callback (lambda (text)
                  (spinner-stop build-progress-spinner)
                  (let ((filtered (periphery-helper:filter-keep-beginning-paths text)))
                    (periphery-run-test-parser filtered (lambda ()
                                                          (message-with-color
                                                           :tag "[All tests passed]"
                                                           :text ""
                                                           :attributes 'success)))))
     :debug swift-additions:debug)

    (message-with-color
     :tag (format "[Testing '%s'-package]" package-name)
     :text "Please wait. Patience is a virtue!"
     :attributes 'warning)))

(defun swift-additions:detect-package-root ()
  "Detects the root directory of the Swift package based on the current buffer."
  (let ((buffer-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (locate-dominating-file buffer-dir "Package.swift")))

;;;###autoload
(defun swift-additions:compile-and-run ()
  "Compile and run app."
  (interactive)
  (swift-additions:compile :run t))

;;;###autoload
(defun swift-additions:compile-app ()
  "Compile app."
  (interactive)
  (swift-additions:compile :run nil))

;;;###autoload
(defun swift-additions:run()
    "Rerun already compiled and installed app."
    (interactive)
    (periphery-kill-buffer)
    (ios-simulator:kill-buffer)

    (if (xcode-additions:run-in-simulator)
        (ios-simulator:install-and-run-app
         :rootfolder (xcode-additions:project-root)
         :build-folder (xcode-additions:build-folder :device-type :simulator)
         :simulatorId (ios-simulator:simulator-identifier)
         :appIdentifier (xcode-additions:fetch-or-load-app-identifier))
      (ios-device:install-app
       :buildfolder (xcode-additions:build-folder :device-type :device)
       :appIdentifier (xcode-additions:fetch-or-load-app-identifier))))

;;;###autoload
(defun swift-additions:test-module-silent ()
  "Test module."
  (interactive)
  (save-some-buffers t)
  (periphery-kill-buffer)
  (ios-simulator:kill-buffer)
  (swift-additions:test-swift-package))

(defun swift-additions:diagnose ()
  "Display diagnostic information about the current Swift development environment."
  (interactive)
  (with-help-window "*Swift Diagnostics*"
    (let ((standard-output (get-buffer-create "*Swift Diagnostics*")))
      (princ "Swift Development Environment Diagnostics\n")
      (princ "=====================================\n\n")

      ;; Project information
      (princ "Project Information:\n")
      (princ "-------------------\n")
      (princ (format "Project Root: %s\n" (xcode-additions:project-root)))
      (princ (format "Project Type: %s\n"
                    (cond ((xcode-additions:is-xcodeproject) "Xcode Project")
                          ((swift-additions:is-a-swift-package-base-project) "Swift Package")
                          (t "Unknown"))))
      (when (xcode-additions:is-xcodeproject)
        (princ (format "Scheme: %s\n" (xcode-additions:scheme)))
        (princ (format "Derived Data Path: %s\n" (xcode-additions:derived-data-path))))

      ;; Build configuration
      (princ "\nBuild Configuration:\n")
      (princ "------------------\n")
      (princ (format "Current Build Configuration: %s\n"
                    (or swift-additions:current-build-configuration "Not set")))
      (princ (format "Default Configuration: %s\n" swift-additions:default-configuration))
      (princ (format "Build System: %s\n" swift-additions:modern-build-system))
      (princ (format "Additional Build Flags: %s\n"
                    (if swift-additions:additional-build-flags
                        (mapconcat #'identity swift-additions:additional-build-flags " ")
                      "None")))

      ;; Environment
      (princ "\nEnvironment:\n")
      (princ "------------\n")
      (princ (format "x86 Environment: %s\n"
                    (if swift-additions:current-environment-x86 "Yes" "No")))
      (princ (format "Number of CPU Cores: %s\n" (swift-additions:get-optimal-jobs)))

      ;; Device/Simulator
      (princ "\nDevice Configuration:\n")
      (princ "-------------------\n")
      (if (xcode-additions:run-in-simulator)
          (progn
            (princ "Running in Simulator:\n")
            (princ (format "Simulator ID: %s\n" (ios-simulator:simulator-identifier)))
            (princ (format "Simulator Name: %s\n" (ios-simulator:simulator-name))))
        (princ "Running on Physical Device\n"))

      ;; Current build state
      (princ "\nCurrent Build State:\n")
      (princ "------------------\n")
      (when swift-additions:current-build-command
        (princ "Current Build Command:\n")
        (princ "------------------\n")
        (princ swift-additions:current-build-command)
        (princ "\n"))
      (when swift-additions:compilation-time
        (princ (format "\nLast Compilation Time: %s seconds\n"
                      (swift-additions:compilation-time))))

      ;; Debug settings
      (princ "\nDebug Settings:\n")
      (princ "--------------\n")
      (princ (format "Debug Mode: %s\n" (if swift-additions:debug "Enabled" "Disabled")))

      ;; File system checks
      (princ "\nFile System Checks:\n")
      (princ "-----------------\n")
      (let ((build-dir (xcode-additions:build-folder :device-type :simulator)))
        (princ (format "Build Directory Exists: %s\n"
                      (if (and build-dir (file-exists-p build-dir)) "Yes" "No")))
        (when build-dir
          (princ (format "Build Directory Path: %s\n" build-dir))))

      ;; Swift toolchain
      (princ "\nSwift Toolchain:\n")
      (princ "---------------\n")
      (let ((swift-version (shell-command-to-string "swift --version")))
        (princ swift-version))

      ;; System info
      (princ "\nSystem Information:\n")
      (princ "------------------\n")
      (princ (format "Emacs Version: %s\n" emacs-version))
      (princ (format "System Type: %s\n" system-type))

      ;(princ "\nSystem Information:\n")
      (princ "------------------\n")
      (princ (format "Build command: %s\n" (swift-additions:build-app-command
                       :sim-id (ios-simulator:simulator-identifier)
                       :derived-path (xcode-additions:derived-data-path))))

      (princ "\nRecommendations:\n")
      (princ "---------------\n")
      (unless (xcode-additions:is-xcodeproject)
        (unless (swift-additions:is-a-swift-package-base-project)
          (princ "WARNING: Neither Xcode project nor Swift package detected\n")))
      (unless swift-additions:current-build-command
        (princ "NOTE: No build command has been generated yet\n"))
      (when swift-additions:debug
        (princ "NOTE: Debug mode is enabled - this may affect performance\n")))))

(provide 'swift-additions)
;;; swift-additions.el ends here
