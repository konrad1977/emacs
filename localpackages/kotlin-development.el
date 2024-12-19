;; kotlin-development.el --- Enhanced Kotlin development environment setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a robust setup for Kotlin development in Emacs with LSP integration,
;; tree-sitter support, and proper error handling.

;;; Code:
(require 'eglot)
(require 'treesit)
(require 'compile)
(require 'kotlin-mode)
(require 'kotlin-ts-mode)
(require 'mode-line-hud)
(require 'android-emulator)

(defgroup kotlin-development nil
  "Customization group for Kotlin development setup."
  :group 'programming
  :prefix "kotlin-development-")

(defcustom kotlin-development-debug nil
  "Enable debug output for Kotlin development."
  :type 'boolean
  :group 'kotlin-development)

(defcustom kotlin-development-lsp-server-path
  (expand-file-name "var/lsp/server/kotlin/server/bin/kotlin-language-server"
                    user-emacs-directory)
  "Path to the Kotlin Language Server executable."
  :type 'string
  :group 'kotlin-development)

(defcustom kotlin-development-indent-offset 4
  "Number of spaces for indentation in Kotlin modes."
  :type 'integer
  :group 'kotlin-development)

;; Add these to your defcustom section
(defcustom kotlin-development-gradle-executable
  (or (executable-find "gradlew")
      (executable-find "./gradlew")
      "gradle")
  "Path to Gradle executable."
  :type 'string
  :group 'kotlin-development)

(defcustom kotlin-development-android-sdk-path
  (or (getenv "ANDROID_HOME")
      (getenv "ANDROID_SDK_ROOT")
      "~/library/android/sdk")
  "Path to Android SDK."
  :type 'string
  :group 'kotlin-development)

(defcustom kotlin-development-test-runner 'junit
  "Test runner to use for Kotlin tests.
Options are 'junit or 'kotest."
  :type '(choice (const :tag "JUnit" junit)
                 (const :tag "Kotest" kotest))
  :group 'kotlin-development)

(defcustom kotlin-development-emulator-name "Pixel_3a_API_34_extension_level_7_arm64-v8a"
  "Name of the Android emulator to use."
  :type 'string
  :group 'kotlin-development)

;; New variables for performance monitoring
(defvar kotlin-development--build-start-time nil
  "Start time of the current build process.")

(defvar kotlin-development--build-history '()
  "History of build times and results.")

(defvar kotlin-development--build-timer nil
  "Timer for checking build status.")

(defvar kotlin-development-current-root nil
  "Current project root directory.")

;; New functions for test running
(defun kotlin-development-run-tests ()
  "Run Kotlin tests based on selected test runner."
  (interactive)
  (let* ((project-root (kotlin-development-find-project-root))
         (default-directory project-root)
         (test-command (pcase kotlin-development-test-runner
                         ('junit "./gradlew test")
                         ('kotest "./gradlew testDebug"))))
    (compile test-command)))

;; New functions for code analysis
(defun kotlin-development-analyze-code ()
  "Run static code analysis using ktlint."
  (interactive)
  (let* ((project-root (kotlin-development-find-project-root))
         (default-directory project-root))
    (compile "./gradlew ktlintCheck")))

(defun kotlin-development-fix-code-style ()
  "Fix code style issues using ktlint."
  (interactive)
  (let* ((project-root (kotlin-development-find-project-root))
         (default-directory project-root))
    (compile "./gradlew ktlintFormat")))

;; Performance monitoring
(defun kotlin-development--start-build-timer ()
  "Start timing the build process."
  (setq kotlin-development--build-start-time (current-time)))

(defun kotlin-development--record-build-time (status)
  "Record build time and status."
  (when kotlin-development--build-start-time
    (let* ((end-time (current-time))
           (duration (float-time (time-subtract end-time kotlin-development--build-start-time))))
      (push `(:duration ,duration :status ,status :timestamp ,end-time)
            kotlin-development--build-history)
      (setq kotlin-development--build-start-time nil))))

(defun kotlin-development-clean-build ()
  "Clean and build the Kotlin project."
  (interactive)
  (let ((default-directory (kotlin-development-find-project-root)))
    (compile (if (file-exists-p "gradlew")
                 "./gradlew clean build"
               "gradle clean build"))))

(defun kotlin-development-rebuild ()
  "Rebuild the Kotlin project without cleaning."
  (interactive)
  (let ((default-directory (kotlin-development-find-project-root)))
    (compile (if (file-exists-p "gradlew")
                 "./gradlew build"
               "gradle build"))))

(defun kotlin-development-check-dependencies ()
  "Check and validate project dependencies."
  (interactive)
  (let* ((default-directory (kotlin-development-find-project-root)))
    (compile "./gradlew dependencies")))

(defun kotlin-development--ensure-valid-avd ()
  "Ensure we have a valid AVD selected, prompting if necessary."
  (unless (member android-emulator-name (android-emulator-get-available-avds))
    (let ((avds (android-emulator-get-available-avds)))
      (unless avds
        (user-error "No Android Virtual Devices found. Please create one in Android Studio"))
      ;; Select the first AVD if there's only one
      (if (= (length avds) 1)
          (progn
            (setq android-emulator-name (car avds))
            (setq kotlin-development-emulator-name android-emulator-name)
            (message "Auto-selected AVD: %s" android-emulator-name))
        ;; Otherwise, prompt for selection
        (let ((selected (completing-read "Select Android Virtual Device: " avds nil t)))
          (setq android-emulator-name selected)
          (setq kotlin-development-emulator-name selected)
          (message "Selected AVD: %s" selected))))))

(defun kotlin-development-build-and-run ()
  "Build and run the Android app in the emulator."
  (interactive)
  (let ((root-dir (kotlin-development-find-project-root)))
    (unless root-dir
      (user-error "Could not find project root directory"))

    (setq default-directory root-dir)
    (setq kotlin-development-current-root root-dir)

    ;; Ensure we have a valid AVD before proceeding
    (kotlin-development--ensure-valid-avd)

    (if (android-emulator-running-p)
        (kotlin-development-build-and-launch)
      (progn
        (when kotlin-development-debug
          (message "Starting emulator '%s' before build..." android-emulator-name))
        ;; Start the emulator and set up build timer
        (android-emulator-start)
        (kotlin-development-setup-build-timer)))))

;; New diagnostic functions
(defun kotlin-development-show-build-history ()
  "Display build history statistics."
  (interactive)
  (with-current-buffer (get-buffer-create "*Kotlin Build History*")
    (erase-buffer)
    (insert "Kotlin Build History:\n\n")
    (dolist (build (reverse kotlin-development--build-history))
      (insert (format "Time: %s\nDuration: %.2fs\nStatus: %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S"
					  (plist-get build :timestamp))
                      (plist-get build :duration)
                      (plist-get build :status))))
    (display-buffer (current-buffer))))

;; APK management
(defun kotlin-development-install-apk (apk-path)
  "Install APK file at APK-PATH."
  (interactive "fSelect APK file: ")
  (let ((adb-path (expand-file-name "platform-tools/adb"
                                    kotlin-development-android-sdk-path)))
    (shell-command (format "%s install -r %s" adb-path apk-path))))

(defun kotlin-development-uninstall-app ()
  "Uninstall the current app from the emulator."
  (interactive)
  (let* ((package-name (kotlin-development--get-package-name))
         (adb-path (expand-file-name "platform-tools/adb"
                                     kotlin-development-android-sdk-path)))
    (shell-command (format "%s uninstall %s" adb-path package-name))))

(defun kotlin-development-find-java-home ()
  "Find Java home directory with specific handling for Oracle JDK."
  (or (getenv "JAVA_HOME")
      (when-let* ((java-path (executable-find "java"))
                  ;; Follow symlinks to get the actual Java path
                  (real-java-path (file-truename java-path)))
        (if (string-match "/bin/java$" real-java-path)
            ;; Strip off /bin/java to get JAVA_HOME
            (substring real-java-path 0 (- (length real-java-path) 9))
          ;; Fallback if path structure is different
          (file-name-directory real-java-path)))
      ;; Common Oracle JDK locations
      (cl-loop for path in '("/usr/lib/jvm/java-23-oracle"
                             "/usr/java/jdk-23"
                             "/Library/Java/JavaVirtualMachines/jdk-23.jdk/Contents/Home")
               when (file-directory-p path)
               return path)))

(defun kotlin-development-ensure-java-home ()
  "Ensure JAVA_HOME is set, prompting user if necessary."
  (unless (getenv "JAVA_HOME")
    (let ((detected-java-home (kotlin-development-find-java-home)))
      (if detected-java-home
          (progn
            (setenv "JAVA_HOME" detected-java-home)
            (message "Set JAVA_HOME to %s" detected-java-home))
        (setenv "JAVA_HOME"
                (expand-file-name
                 (read-directory-name "Set JAVA_HOME: " "/usr/lib/jvm/")))))))

(defun kotlin-development-get-build-dir (project-root)
  "Get the build directory for compiled Kotlin classes."
  (expand-file-name "build/classes/kotlin/main" project-root))

(defun kotlin-development-get-source-roots (project-root)
  "Get list of source roots for the project."
  (let ((source-roots '()))
    (dolist (path '("src/main/kotlin" "src/main/java"))
      (let ((full-path (expand-file-name path project-root)))
        (when (file-directory-p full-path)
          (push full-path source-roots))))
    source-roots))

(defun kotlin-development-setup-dape ()
  "Setup dape for Android debugging."
  (interactive)
  (require 'dape)

  ;; Increase the timeout for initialization
  (setq dape-request-timeout 30)

  ;; Ensure JAVA_HOME is set first
  (kotlin-development-ensure-java-home)

  (let* ((project-root (or (kotlin-development-find-project-root)
                           (error "Could not find project root")))
         (source-roots (kotlin-development-get-source-roots project-root))
         (debug-adapter-path (expand-file-name
                              "~/.emacs.d/var/dape-adapters/kotlin-debug-adapter/bin/kotlin-debug-adapter")))

    (add-to-list 'dape-configs
                 `(android-debug
                   modes (kotlin-mode kotlin-ts-mode)
                   command ,debug-adapter-path
                   :type "kotlin"
                   :request "attach"  ; Changed from "launch" to "attach" for Android
                   :projectRoot ,project-root
                   :hostName "localhost"
                   :port 5005  ; Default Android debug port
		   :classPaths [,(expand-file-name "app/build/intermediates/javac/debug/classes"
						   project-root)]
                   :modulePaths [,(expand-file-name "." project-root)]
                   :args []
		   :env (:JAVA_HOME ,(getenv "JAVA_HOME")
				    :ANDROID_HOME ,(getenv "ANDROID_HOME"))
                   :sourceRoots ,source-roots))))

(defun kotlin-development-start-android-debug ()
  "Start Android debugging process."
  (interactive)
  (let* ((package-name (kotlin-development--get-package-name))
         (main-activity (read-string "Main activity: " ".MainActivity")))

    ;; Kill any existing debug app
    (call-process "adb" nil nil nil "shell" "am" "force-stop" package-name)

    ;; Start app in debug mode
    (call-process "adb" nil nil nil "shell" "am" "start"
                  "-D" ; Debug flag
                  "-n" (concat package-name "/" package-name main-activity))

    ;; Forward debug port
    (call-process "adb" nil nil nil "forward" "tcp:5005" "jdwp:*))")))

;; ;; Helper function to install debug adapter if needed
;; (defun kotlin-development-ensure-debug-adapter ()
;;   "Ensure the Java debug adapter is installed."
;;   (interactive)
;;   (let ((adapter-dir (file-name-concat dape-adapter-dir "vscode-java-debug")))
;;     (unless (file-exists-p adapter-dir)
;;       (make-directory adapter-dir t)
;;       (let ((default-directory adapter-dir))
;;         (call-process "git" nil t t "clone" "Https://github.com/microsoft/vscode-java-debug.git" ".")
;;         (call-process "npm" nil t t "install")
;;         (call-process "npm" nil t t "run" "build")))))

(defun kotlin-development-start-debugger ()
  "Start debugging Android application."
  (interactive)
  ;; Ensure debug adapter is installed
  ;; Check if emulator is running
  (unless (string-match-p "[A-Za-z0-9]+" (shell-command-to-string "adb devices"))
    (user-error "No Android device/emulator found. Please connect a device or start an emulator"))
  (let ((default-directory (kotlin-development-find-project-root)))
    (dape 'android)))

(defun kotlin-development-select-emulator ()
  "Select an Android emulator interactively."
  (interactive)
  (let* ((emulator-path (expand-file-name "emulator/emulator" kotlin-development-android-sdk-path))
         (output (shell-command-to-string (format "%s -list-avds" emulator-path)))
         (avds (split-string output "\n" t))
         (selected (completing-read "Select emulator: " avds nil t)))
    (setq kotlin-development-emulator-name selected)
    (when kotlin-development-debug
      (message "Selected emulator: %s" selected))))

(defun kotlin-development-check-emulator-status ()
  "Check if emulator is fully booted."
  (let* ((adb-path (expand-file-name "platform-tools/adb" kotlin-development-android-sdk-path))
         (output (shell-command-to-string
                  (format "%s shell getprop sys.boot_completed" adb-path))))
    (string-match-p "1" output)))

;; Add these functions
;;;###autoload
(defun kotlin-development-find-project-root ()
  "Find the root directory of the Android/Kotlin project and cache it in 'kotlin-development-current-root'."
  (unless (bound-and-true-p kotlin-development-current-root)
    (setq kotlin-development-current-root
	  (let ((git-root (locate-dominating-file default-directory ".git")))
	    (if git-root
		(or (locate-dominating-file git-root "settings.gradle.kts")
		    git-root)
	      (locate-dominating-file default-directory "settings.gradle.kts")))))
  kotlin-development-current-root)

(defun kotlin-development-build-and-run ()
  "Build and run the Android app in the emulator."
  (interactive)
  (let ((root-dir (kotlin-development-find-project-root)))
    (unless root-dir
      (user-error "Could not find project root directory"))

    (setq default-directory root-dir)
    (if (android-emulator-running-p)
        (kotlin-development-build-and-launch)
      (kotlin-development-start-emulator-and-build))))

(defun kotlin-development-build-and-launch ()
  "Build and launch the app when emulator is already running."
  ;; (mode-line-hud:update :message "Building...")
  (kotlin-development-execute-build))

(defun kotlin-development-start-emulator-and-build ()
  "Start the emulator and build when ready."
  (when kotlin-development-debug
    (message "Emulator not running, starting emulator first..."))
  (android-emulator-start)
  (kotlin-development-setup-build-timer))

(defun kotlin-development-setup-build-timer ()
  "Set up timer to check emulator status and initiate build."
  (when kotlin-development--build-timer
    (cancel-timer kotlin-development--build-timer))
  (message "Waiting for emulator to complete startup...")
  (setq kotlin-development--build-timer
        (run-with-timer
         0 2
         (lambda ()
           (when (android-emulator-check-status)
             (kotlin-development-handle-emulator-ready))))))

(defun kotlin-development-handle-emulator-ready ()
  "Handle when emulator is ready to build."
  (let ((default-directory kotlin-development-current-root))  ; Set correct directory
    (when kotlin-development-debug
      (message "Emulator is ready, starting build in: %s" default-directory))
    (kotlin-development-execute-build)
    (when kotlin-development--build-timer
      (cancel-timer kotlin-development--build-timer)
      (setq kotlin-development--build-timer nil))))

(defun kotlin-development-execute-build ()
  "Execute the gradle build and set up completion hook."
  (let* ((project-root (kotlin-development-find-project-root))
         (default-directory project-root)  ; Explicitly set build directory
         (gradle-command (format "%s installDebug" kotlin-development-gradle-executable)))
    (when kotlin-development-debug
      (message "Executing build in directory: %s" default-directory)
      (message "Build command: %s" gradle-command))
    (compile gradle-command)
    (add-hook 'compilation-finish-functions
              (lambda (buf status)
                (when (string-match-p "finished" status)
                  (kotlin-development-handle-build-success))))))

(defun kotlin-development-handle-build-success ()
  "Handle successful build completion."
  (mode-line-hud:update :message "Build completed successfully! Launching app...")
  (run-with-timer 2 nil #'kotlin-development-launch-app))

(cl-defun kotlin-development-parse-build-output (&key input)
  "Parse Gradle build output and display relevant status updates.
   Shows compilation progress and handles errors/warnings."
  (when kotlin-development-debug
    (message "Parsing Gradle output: %s" input))
  (let ((seen-messages (make-hash-table :test 'equal))
        (error-regex "^\\(.+\\):\\([0-9]+\\): \\(error\\|warning\\|note\\): \\(.+\\)$")
        (task-regex "^Task \\(:?[a-zA-Z0-9:]+\\)")
        (build-status-regex "BUILD \\(SUCCESSFUL\\|FAILED\\)"))

    (dolist (line (split-string input "\n"))
      (cond
       ;; Match Gradle tasks
       ((string-match task-regex line)
        (let ((task-name (match-string 1 line)))
          (unless (gethash task-name seen-messages)
            (mode-line-hud:update
             :message (format "Running %s" (propertize task-name 'face 'warning)))
            (puthash task-name t seen-messages))))

       ;; Match build status
       ((string-match build-status-regex line)
        (let ((status (match-string 1 line)))
          (mode-line-hud:update
           :message (format "Build %s"
                            (propertize status 'face
					(if (string= status "SUCCESSFUL")
                                            'success 'error))))))

       ;; Match compilation errors/warnings
       ((string-match error-regex line)
        (let ((file (match-string 1 line))
              (line-num (match-string 2 line))
              (type (match-string 3 line))
              (message (match-string 4 line)))
          (message "%s:%s: %s: %s"
                   (propertize file 'face 'warning)
                   line-num
                   (propertize type 'face
                               (cond ((string= type "error") 'error)
                                     ((string= type "warning") 'warning)
                                     (t 'success)))
                   message)))))))


(defun kotlin-development-cleanup ()
  "Clean up timers and processes."
  (interactive)
  (when kotlin-development--build-timer
    (cancel-timer kotlin-development--build-timer)
    (setq kotlin-development--build-timer nil))
  (when kotlin-development--emulator-check-timer
    (cancel-timer kotlin-development--emulator-check-timer)
    (setq kotlin-development--emulator-check-timer nil))
  (when kotlin-development--emulator-process
    (when (process-live-p kotlin-development--emulator-process)
      (kill-process kotlin-development--emulator-process))
    (setq kotlin-development--emulator-process nil)))

(defun kotlin-development-emulator-terminate-app ()
  "Terminate the Android app using adb."
  (interactive)
  (let* ((default-directory (kotlin-development-find-project-root))
         (package-name (or (kotlin-development--get-package-name)
                           (error "Could not determine package name"))))
    (shell-command (format "adb shell am force-stop %s" package-name))))

(defun kotlin-development-emulator-wipe ()
  "Wipe the Android emulator data using adb."
  (interactive)
  (let* ((default-directory (kotlin-development-find-project-root))
         (package-name (or (kotlin-development--get-package-name)
                           (error "Could not determine package name"))))
    (when kotlin-development-debug
      (message "Wiping data for package: %s" package-name))
    (shell-command (format "adb shell pm clear %s" package-name))
    (message "Emulator data wiped for package %s" package-name)))

(defun kotlin-development--get-package-name ()
  "Extract package name from build.gradle.kts, with enhanced debugging."
  (let* ((project-root (kotlin-development-find-project-root))
         (gradle-path (expand-file-name "app/build.gradle.kts" project-root)))

    (when kotlin-development-debug
      (message "Project root: %s" project-root)
      (message "Looking for gradle file at: %s" gradle-path))

    (if (not (file-exists-p gradle-path))
        (message "Error: build.gradle.kts not found at %s" gradle-path)
      (with-temp-buffer
        (insert-file-contents gradle-path)
	(when kotlin-development-debug
	  (message "File contents length: %d" (buffer-size)))

        (goto-char (point-min))
        (let ((case-fold-search t)
              (found nil))
          ;; Try multiple regex patterns
          (catch 'found
            (dolist (pattern '("applicationId *= *\"\\([^\"]+\\)\""
                               "applicationId[[:space:]]*=[[:space:]]*\"\\([^\"]+\\)\""
                               "applicationId\\s*=\\s*\"\\([^\"]+\\)\""))
	      (when kotlin-development-debug
	  	(message "Trying pattern: %s" pattern))
              (goto-char (point-min))
              (when (re-search-forward pattern nil t)
                (let ((match (match-string 1)))
		  (when kotlin-development-debug
		    (message "Found match with pattern %s: %s" pattern match))
                  (setq found match)
                  (throw 'found match)))))
          found)))))

(defun kotlin-development--get-launch-activity ()
  "Extract launch activity from AndroidManifest.xml with better pattern matching."
  (let* ((project-root (kotlin-development-find-project-root))
         (manifest-path (expand-file-name "app/src/main/AndroidManifest.xml" project-root)))
    (when (file-exists-p manifest-path)
      (with-temp-buffer
        (insert-file-contents manifest-path)
        (goto-char (point-min))
        ;; Try different activity patterns
        (or
         ;; Look for activity with MAIN action and LAUNCHER category
         (progn
           (goto-char (point-min))
           (when (re-search-forward
                  (concat "<activity[^>]*android:name=\"\\([^\"]+\\)\"[^>]*>.*?"
                          "<intent-filter>.*?"
                          "<action android:name=\"android.intent.action.MAIN\".*?"
                          "<category android:name=\"android.intent.category.LAUNCHER\"")
                  nil t)
             (match-string 1)))
         ;; Try a more lenient pattern
         (progn
           (goto-char (point-min))
           (when (re-search-forward
                  "<activity[^>]*android:name=\"\\([^\"]+\\)\".*?MAIN.*?LAUNCHER"
                  nil t)
             (match-string 1)))
         ;; Try looking for MainActivity specifically
         (progn
           (goto-char (point-min))
           (when (re-search-forward
                  "<activity[^>]*android:name=\"\\([^\"]*MainActivity[^\"]*\\)\""
                  nil t)
             (match-string 1))))))))

(defun kotlin-development-launch-app ()
  "Launch the installed Android app."
  (interactive)
  (let* ((default-directory (kotlin-development-find-project-root))
	 (package-name (kotlin-development--get-package-name))
         (activity (kotlin-development--get-launch-activity))
         (adb-path (expand-file-name "platform-tools/adb" android-emulator-sdk-path)))

    (android-emulator-set-app-identifier package-name)
    (when kotlin-development-debug
      (message "Launching app with package: %s, activity: %s" package-name activity))

    (unless package-name
      (user-error "Could not determine package name"))

    (unless activity
      (user-error "Could not determine launch activity"))

    (mode-line-hud:update :message
                          (format "Launching app: %s/%s" package-name activity))

    (let* ((full-activity (if (string-prefix-p "." activity)
                              (concat package-name activity)
                            activity))
           (launch-command (format "%s shell am start -n %s/%s"
                                   adb-path package-name full-activity))
           (result (shell-command-to-string launch-command)))

      (if (string-match-p "Error\\|Exception" result)
          (progn
            (message "Failed to launch app: %s" result)
            (when kotlin-development-debug
              (message "Launch command was: %s" launch-command)))
        (mode-line-hud:update
         :message (propertize package-name 'face 'font-lock-constant-face))))))

;;;###autoload
(defun kotlin-development-verify-lsp-server ()
  "Verify that the Kotlin LSP server exists and is executable."
  (unless (and (file-exists-p kotlin-development-lsp-server-path)
               (file-executable-p kotlin-development-lsp-server-path))
    (user-error "Kotlin Language Server not found or not executable at %s"
                kotlin-development-lsp-server-path)))

;;;###autoload
(defun kotlin-development-install-tree-sitter-grammar ()
  "Install the Kotlin tree-sitter grammar if not present."
  (unless (treesit-language-available-p 'kotlin)
    (message "Installing Kotlin tree-sitter grammar...")
    (condition-case err
        (treesit-install-language-grammar 'kotlin)
      (error
       (message "Failed to install Kotlin grammar: %s" (error-message-string err))))))

;;;###autoload
(defun kotlin-development-common-hook ()
  "Common setup for both Kotlin modes."
  (setq-local comment-start "// "
              comment-end ""))

;;;###autoload
(defun kotlin-development-setup ()
  "Main setup function for Kotlin development environment."
  (interactive)
  ;; Configure tree-sitter
  (add-to-list 'treesit-language-source-alist
               '(kotlin "https://github.com/fwcd/tree-sitter-kotlin"))

  ;; Install grammar if needed
  (kotlin-development-install-tree-sitter-grammar)

  ;; Setup both modes
  (kotlin-development-common-hook)
  (add-hook 'kotlin-ts-mode (lambda () (setq-local indent-tabs-mode nil))))

;;;###autoload
(defun kotlin-development-validate-android-setup ()
  "Validate Android SDK setup and paths."
  (interactive)
  (let ((sdk-path kotlin-development-android-sdk-path)
	(emulator-path (expand-file-name "emulator/emulator" kotlin-development-android-sdk-path))
	(adb-path (expand-file-name "platform-tools/adb" kotlin-development-android-sdk-path)))

    (unless (file-exists-p sdk-path)
      (user-error "Android SDK not found at %s" sdk-path))

    (unless (file-exists-p emulator-path)
      (user-error "Android emulator not found at %s" emulator-path))

    (unless (file-exists-p adb-path)
      (user-error "ADB not found at %s" adb-path))

    (message "Android setup validated successfully:
SDK: %s
Emulator: %s
ADB: %s" sdk-path emulator-path adb-path)))

(defun kotlin-development-diagnose ()
  "Diagnose Android emulator setup."
  (interactive)
  (let* ((emulator-path (expand-file-name "emulator/emulator" kotlin-development-android-sdk-path))
         (avd-path (expand-file-name ".android/avd" (getenv "HOME")))
         (diagnostic-buffer "*Android Diagnostic*"))

    (with-current-buffer (get-buffer-create diagnostic-buffer)
      (erase-buffer)
      (insert "Android Development Diagnostic Results:\n\n")

      ;; Check SDK path
      (insert (format "SDK Path: %s\n" kotlin-development-android-sdk-path))
      (insert (format "SDK Exists: %s\n\n" (file-exists-p kotlin-development-android-sdk-path)))

      ;; Check emulator path
      (insert (format "Emulator Path: %s\n" emulator-path))
      (insert (format "Emulator Exists: %s\n" (file-exists-p emulator-path)))
      (insert (format "Emulator Executable: %s\n\n" (file-executable-p emulator-path)))

      ;; List available AVDs
      (insert "Available AVDs:\n")
      (insert (shell-command-to-string
               (format "%s -list-avds" emulator-path)))
      (insert "\n")

      ;; Check current AVD
      (insert (format "Current AVD Name: %s\n" kotlin-development-emulator-name))
      (insert (format "AVD Path: %s\n" avd-path))
      (when (file-exists-p avd-path)
        (insert "AVD Directory Contents:\n")
        (insert (shell-command-to-string (format "ls -l %s" avd-path))))

      (display-buffer (current-buffer)))))

;;;###autoload
(define-minor-mode kotlin-development-minor-mode
  "Minor mode for Kotlin development environment."
  :lighter " KotDev"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'kotlin-development-build-and-run)
            (define-key map (kbd "C-c C-r") #'kotlin-development-rebuild)
            (define-key map (kbd "C-c C-d") (lambda () (interactive)
                                              (kotlin-development-setup-dape)
                                              (kotlin-development-start-debugger)))
            (define-key map (kbd "C-c C-e e") #'kotlin-development-select-emulator)
            (define-key map (kbd "C-c C-e k") #'kotlin-development-kill-emulator)
            (define-key map (kbd "C-c C-e l") #'kotlin-development-list-emulators)
            (define-key map (kbd "C-c C-e s") #'kotlin-development-start-emulator)
            (define-key map (kbd "C-c t t") #'kotlin-development-run-tests)
            (define-key map (kbd "M-r") #'kotlin-development-launch-app)
            (define-key map (kbd "M-s") #'kotlin-development-emulator-terminate-app)
            (define-key map (kbd "M-K") #'kotlin-development-clean-build)
            (define-key map (kbd "C-c a") #'kotlin-development-analyze-code)
            (define-key map (kbd "C-c d") #'kotlin-development-check-dependencies)
            (define-key map (kbd "C-c f") #'kotlin-development-fix-code-style)
            (define-key map (kbd "C-c h") #'kotlin-development-show-build-history)
            map))

(defun kotlin-development--debug-manifest ()
  "Debug function to show manifest contents."
  (let* ((project-root (kotlin-development-find-project-root))
         (manifest-path (expand-file-name "app/src/main/AndroidManifest.xml" project-root)))
    (when (file-exists-p manifest-path)
      (with-current-buffer (get-buffer-create "*Android Manifest Debug*")
        (erase-buffer)
        (insert-file-contents manifest-path)
        (xml-mode)
        (display-buffer (current-buffer))))))

;;;###autoload
(defun kotlin-development-mode-setup ()
  "Setup for both kotlin-mode and kotlin-ts-mode."
  (kotlin-development-minor-mode 1)
  (kotlin-development-setup))

(provide 'kotlin-development)
;;; kotlin-development.el ends here
