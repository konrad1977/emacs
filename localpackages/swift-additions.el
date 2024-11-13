;;; swift-additions.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Package for building and running iOS/macOS apps from Emacs

;;; Code:
(require 'ios-device)
(require 'ios-simulator)

(defgroup swift-additions:xcodebuild nil
  "REPL."
  :tag "swift-additions:xcodebuild"
  :group 'swift-additions)

(defvar current-build-configuration nil)
(defvar current-build-folder nil)
(defvar current-environment-x86 nil)
(defvar current-local-device-id nil)
(defvar current-build-command nil)
(defvar build-progress-spinner nil)
(defvar compilation-time nil)
(defvar swift-additions:debug nil
  "Debug.")

(defun swift-additions:xcodebuild-command ()
"Use x86 environement."
(if current-environment-x86
    "env /usr/bin/arch -x86_64 xcrun xcodebuild build \\"
  "xcrun xcodebuild build \\"))

(cl-defun build-app-command (&key sim-id (nil) derived-path)
"Xcodebuild with (as SIM-ID DERIVED-PATH)."
(if current-build-command
    current-build-command
  (concat
    (swift-additions:xcodebuild-command)
    (format "%s \\" (xcode-additions:get-workspace-or-project))
    (format "-scheme %s \\" (xcode-additions:scheme))
    (format "-jobs %s \\" (swift-additions:get-number-of-cores))
    (if sim-id
        (format "-destination 'generic/platform=iOS Simulator,id=%s' -sdk %s \\" sim-id "iphonesimulator")
      (format "-destination 'generic/platform=%s' -sdk %s \\" "iOS" "iphoneos"))
    "-configuration Debug \\"
    "-parallelizeTargets \\"
    "-UseModernBuildSystem=YES \\"
    "-derivedDataPath build | xcode-build-server parse -avv"
    ;; "-resultBundlePath .bundle \\"
    ;; (format "-derivedDataPath %s | xcode-build-server parse -av" derived-path)
    ;; (format "-derivedDataPath %s" derived-path)
    )))

(defun swift-additions:get-number-of-cores ()
  "Get the number of available CPU cores."
  (number-to-string (string-to-number (shell-command-to-string "sysctl -n hw.ncpu"))))

(defun swift-additions:compilation-time ()
    "Get the time of the compilation."
    (if-let* ((end-time (current-time)))
        (format "%.1f" (float-time (time-subtract end-time compilation-time)))
      nil))

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
  (when (swift-additions:check-if-build-was-successful output)
     (funcall callback))
   (periphery-run-parser output))

(cl-defun swift-additions:get-current-sdk (&key sim-id)
"Return the current SDK with SIM-ID."
(if sim-id
    "iphoneos"
  "iphonesimulator"))

(defun swift-additions:successful-build ()
  "Show that the build was successful."
  (mode-line-hud:update :message (format "Successful build %s"
                                         (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face))))

;;;###autoload
(defun swift-additions:compile-and-run ()
  "Compile and run app."
  (interactive)
  (swift-additions:compile :run t))

(defun swift-additions:compile-app ()
  "Compile app."
  (interactive)
  (swift-additions:compile :run nil))

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
  "Compile app (RUN)."
  (setq current-build-command nil)
  (xcode-additions:setup-project)
  (setq run-once-compiled run)

  (let ((build-command (build-app-command
                        :sim-id (ios-simulator:simulator-identifier)
                        :derived-path (xcode-additions:derived-data-path))))
    (spinner-start 'progress-bar-filled)
    (setq current-build-command build-command)
    (setq compilation-time (current-time))
    (setq build-progress-spinner spinner-current)
    (when swift-additions:debug
      (message build-command))

    (mode-line-hud:update :message
                          (format "Building %s|%s"
                                  (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                                  (propertize (ios-simulator:simulator-name) 'face 'font-lock-negation-char-face)))

    (xcode-additions:setup-xcodebuildserver)

    (async-start-command-to-string
     :command build-command
     :callback (lambda (text)
                 (when swift-additions:debug (message text))
                 (spinner-stop build-progress-spinner)
                 (if run-once-compiled
                     (swift-additions:check-for-errors text #'swift-additions:run-app-after-build)
                   (swift-additions:check-for-errors text #'swift-additions:successful-build)))
     :update-callback (lambda (text)
                        (xcode-additions:parse-compile-lines-output :input text))
     :debug swift-additions:debug)))

(defun swift-additions:compile-for-device (&key run)
  "Compile and RUN on device ."
  (setq current-build-command nil)
  (xcode-additions:setup-project)
  (setq run-once-compiled run)

  (let ((build-command (build-app-command :derived-path (xcode-additions:derived-data-path))))
    (spinner-start 'progress-bar-filled)
    (setq current-build-command build-command)
    (setq compilation-time (current-time))
    (setq build-progress-spinner spinner-current)


    (when swift-additions:debug
      (message current-build-command)
      (message "Build-folder: %s" (xcode-additions:derived-data-path)))

    (xcode-additions:setup-xcodebuildserver)
    (mode-line-hud:update :message (format "Compiling %s|%s"
                                           (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                                           (propertize "Physical Device" 'face 'font-lock-negation-char-face)))
    (spinner-start 'progress-bar-filled)

    (async-start-command-to-string
     :command build-command
     :callback (lambda (text)
                 (spinner-stop build-progress-spinner)
                 (if run-once-compiled
                     (swift-additions:check-for-errors text #'swift-additions:run-app-on-device-after-build)
                   (swift-additions:check-for-errors text #'swift-additions:successful-build)))
     :debug ios-device:debug)))

;;;###autoload
(defun swift-additions:test-module-silent ()
"Test module."
(interactive)
(save-some-buffers t)
(periphery-kill-buffer)
(ios-simulator:kill-buffer)
(swift-additions:test-swift-package))

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


(provide 'swift-additions)

;;; swift-additions.el ends here
