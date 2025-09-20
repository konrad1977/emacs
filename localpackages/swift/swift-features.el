;;; swift-features.el --- Additional features for Swift development -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides SwiftUI preview, test coverage, and other advanced features

;;; Code:

(require 'cl-lib)
(require 'xcode-additions nil t)
(require 'swift-cache nil t)

(defgroup swift-features nil
  "Advanced features for Swift development."
  :group 'programming
  :prefix "swift-features-")

(defcustom swift-features-coverage-threshold 80
  "Minimum code coverage percentage threshold."
  :type 'integer
  :group 'swift-features)

;; SwiftUI Preview Support

(defun swift-features:swiftui-preview-start ()
  "Start SwiftUI Preview for the current file."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (project-root (xcode-additions:project-root))
         (scheme (xcode-additions:scheme)))
    (if (not (string-match-p "\\.swift$" current-file))
        (message "SwiftUI Preview only works with Swift files")
      (message "Starting SwiftUI Preview for %s..." (file-name-nondirectory current-file))
      (start-process 
       "swiftui-preview"
       "*SwiftUI Preview Log*"
       "xcrun"
       "xcodebuild"
       "-workspace" (format "%s.xcworkspace" (xcode-additions:workspace-name))
       "-scheme" scheme
       "-derivedDataPath" ".build"
       "-destination" "platform=iOS Simulator,name=iPhone 15 Pro"
       "preview"
       current-file))))

(defun swift-features:swiftui-preview-stop ()
  "Stop SwiftUI Preview."
  (interactive)
  (when-let ((process (get-process "swiftui-preview")))
    (kill-process process)
    (message "SwiftUI Preview stopped")))

;; Test Coverage Support

(defun swift-features:run-tests-with-coverage ()
  "Run tests with code coverage reporting."
  (interactive)
  (let* ((project-root (xcode-additions:project-root))
         (scheme (xcode-additions:scheme))
         (coverage-file (expand-file-name ".build/coverage.json" project-root)))
    (message "Running tests with coverage...")
    (make-process
     :name "swift-test-coverage"
     :buffer "*Swift Test Coverage*"
     :command (list "xcrun" "xcodebuild"
                   "test"
                   "-scheme" scheme
                   "-enableCodeCoverage" "YES"
                   "-derivedDataPath" ".build"
                   "-resultBundlePath" ".build/TestResults.xcresult"
                   "-destination" "platform=iOS Simulator,name=iPhone 15 Pro")
     :sentinel (lambda (proc event)
                 (when (string= event "finished\n")
                   (swift-features:parse-coverage-results))))))

(defun swift-features:parse-coverage-results ()
  "Parse and display code coverage results."
  (let* ((project-root (xcode-additions:project-root))
         (result-bundle (expand-file-name ".build/TestResults.xcresult" project-root)))
    (if (file-exists-p result-bundle)
        (progn
          (message "Parsing coverage results...")
          (let ((coverage-output 
                 (shell-command-to-string 
                  (format "xcrun xcresulttool get --format json --path %s" result-bundle))))
            (swift-features:display-coverage-summary coverage-output)))
      (message "No test results found"))))

(defun swift-features:display-coverage-summary (json-output)
  "Display coverage summary from JSON-OUTPUT."
  (condition-case err
      (let* ((json-data (json-read-from-string json-output))
             (metrics (cdr (assoc 'metrics json-data)))
             (line-coverage (cdr (assoc 'lineCoverage metrics))))
        (with-current-buffer (get-buffer-create "*Swift Coverage Report*")
          (erase-buffer)
          (insert "Swift Test Coverage Report\n")
          (insert "==========================\n\n")
          (insert (format "Line Coverage: %.1f%%\n" (* 100 line-coverage)))
          (when (< (* 100 line-coverage) swift-features-coverage-threshold)
            (insert (format "\n⚠ Warning: Coverage below threshold (%d%%)\n" 
                           swift-features-coverage-threshold)))
          (display-buffer (current-buffer))))
    (error
     (message "Failed to parse coverage results: %s" (error-message-string err)))))

;; Performance Profiling

(defun swift-features:profile-build ()
  "Profile build performance and identify bottlenecks."
  (interactive)
  (let* ((project-root (xcode-additions:project-root))
         (scheme (xcode-additions:scheme)))
    (message "Starting build profiling...")
    (make-process
     :name "swift-build-profile"
     :buffer "*Swift Build Profile*"
     :command (list "xcrun" "xcodebuild"
                   "build"
                   "-scheme" scheme
                   "-showBuildTimingSummary"
                   "-derivedDataPath" ".build")
     :sentinel (lambda (proc event)
                 (when (string= event "finished\n")
                   (message "Build profiling complete. Check *Swift Build Profile* buffer"))))))

;; Multiple Simulator Support

(defvar swift-features--active-simulators '()
  "List of currently active simulator IDs.")

(defun swift-features:launch-multiple-simulators ()
  "Launch app on multiple simulators simultaneously."
  (interactive)
  (let* ((available-simulators (ios-simulator:available-simulators))
         (selected (completing-read-multiple 
                   "Select simulators (comma-separated): "
                   available-simulators)))
    (dolist (sim-choice selected)
      (let ((sim-id (cdr (assoc sim-choice available-simulators))))
        (swift-features:launch-on-simulator sim-id)
        (push sim-id swift-features--active-simulators)))
    (message "Launched app on %d simulators" (length selected))))

(defun swift-features:launch-on-simulator (simulator-id)
  "Launch app on specific SIMULATOR-ID."
  (ios-simulator:boot-simulator-with-id simulator-id)
  (sit-for 2)
  (ios-simulator:install-and-run-app
   :rootfolder (xcode-additions:project-root)
   :build-folder (xcode-additions:build-folder :device-type :simulator)
   :simulatorId simulator-id
   :appIdentifier (xcode-additions:fetch-or-load-app-identifier)))

(defun swift-features:terminate-all-simulators ()
  "Terminate app on all active simulators."
  (interactive)
  (dolist (sim-id swift-features--active-simulators)
    (ios-simulator:terminate-app :simulatorID sim-id
                                 :appIdentifier (xcode-additions:fetch-or-load-app-identifier)))
  (setq swift-features--active-simulators '())
  (message "Terminated app on all simulators"))

;; Local Swift Package Support

(defun swift-features:add-local-package ()
  "Add a local Swift package to the current project."
  (interactive)
  (let* ((package-path (read-directory-name "Select local package directory: "))
         (project-root (xcode-additions:project-root)))
    (if (file-exists-p (expand-file-name "Package.swift" package-path))
        (progn
          (message "Adding local package: %s" package-path)
          (shell-command 
           (format "cd %s && swift package add-dependency %s" 
                   project-root package-path)))
      (message "No Package.swift found in selected directory"))))

;; Memory Leak Detection

(defun swift-features:check-memory-leaks ()
  "Run memory leak detection on the current app."
  (interactive)
  (let ((app-name (xcode-additions:product-name)))
    (message "Starting memory leak detection for %s..." app-name)
    (start-process 
     "leak-check"
     "*Memory Leak Report*"
     "xcrun"
     "leaks"
     "--atExit"
     "--"
     (format "xcrun simctl launch booted %s" 
             (xcode-additions:fetch-or-load-app-identifier)))))

;; Documentation Generation

(defun swift-features:generate-documentation ()
  "Generate documentation for the current Swift project."
  (interactive)
  (let ((project-root (xcode-additions:project-root)))
    (message "Generating documentation...")
    (start-process
     "swift-doc"
     "*Swift Documentation*"
     "sh" "-c"
     (format "cd %s && swift-doc generate --output .docs" project-root))))

;; Dependency Analysis

(defun swift-features:analyze-dependencies ()
  "Analyze and visualize project dependencies."
  (interactive)
  (let* ((project-root (xcode-additions:project-root))
         (output-file (expand-file-name "dependencies.dot" project-root)))
    (message "Analyzing dependencies...")
    (shell-command 
     (format "cd %s && swift package show-dependencies --format dot > %s" 
             project-root output-file))
    (if (file-exists-p output-file)
        (message "Dependency graph saved to %s" output-file)
      (message "Failed to generate dependency graph"))))

;; Quick Actions Menu

(defun swift-features:quick-actions ()
  "Show quick actions menu for Swift development."
  (interactive)
  (let ((action (completing-read 
                "Swift Action: "
                '("SwiftUI Preview"
                  "Run Tests with Coverage"
                  "Profile Build"
                  "Check Memory Leaks"
                  "Generate Documentation"
                  "Analyze Dependencies"
                  "Add Local Package"
                  "Launch Multiple Simulators"))))
    (pcase action
      ("SwiftUI Preview" (swift-features:swiftui-preview-start))
      ("Run Tests with Coverage" (swift-features:run-tests-with-coverage))
      ("Profile Build" (swift-features:profile-build))
      ("Check Memory Leaks" (swift-features:check-memory-leaks))
      ("Generate Documentation" (swift-features:generate-documentation))
      ("Analyze Dependencies" (swift-features:analyze-dependencies))
      ("Add Local Package" (swift-features:add-local-package))
      ("Launch Multiple Simulators" (swift-features:launch-multiple-simulators)))))

(provide 'swift-features)
;;; swift-features.el ends here