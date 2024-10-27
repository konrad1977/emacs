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

(defcustom kotlin-development-emulator-name "Pixel_3a_API_34_extension_level_7_arm64-v8a"
  "Name of the Android emulator to use."
  :type 'string
  :group 'kotlin-development)

(defvar kotlin-development--emulator-process nil
  "Process object for the running emulator.")

(defvar kotlin-development--emulator-check-timer nil
  "Timer for checking emulator status.")

(defvar kotlin-development--build-timer nil
  "Timer for checking build status.")

(defvar kotlin-development-current-root nil
  "Cached project root directory.")

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
		    git-root)))))
  kotlin-development-current-root)

(defun kotlin-development-get-adb-devices ()
  "Get list of connected Android devices."
  (let* ((adb-path (expand-file-name "platform-tools/adb" kotlin-development-android-sdk-path))
         (output (shell-command-to-string (format "%s devices" adb-path))))
    (split-string output "\n" t)))

(defun kotlin-development-kill-emulator ()
  "Kill the running emulator and clean up processes."
  (interactive)
  (when kotlin-development--emulator-check-timer
    (cancel-timer kotlin-development--emulator-check-timer)
    (setq kotlin-development--emulator-check-timer nil))

  (when kotlin-development--emulator-process
    (delete-process kotlin-development--emulator-process)
    (setq kotlin-development--emulator-process nil))

  (let ((adb-path (expand-file-name "platform-tools/adb" kotlin-development-android-sdk-path)))
    (shell-command (format "%s emu kill" adb-path))
    (when kotlin-development-debug
      (message "Emulator shutdown initiated."))))

(defun kotlin-development-list-emulators ()
  "List available Android emulators."
  (interactive)
  (let* ((emulator-path (expand-file-name "emulator/emulator" kotlin-development-android-sdk-path))
         (output (shell-command-to-string (format "%s -list-avds" emulator-path))))
    (with-current-buffer (get-buffer-create "*Android Emulators*")
      (erase-buffer)
      (insert "Available Android Emulators:\n\n")
      (insert output)
      (display-buffer (current-buffer)))))

(defun kotlin-development-emulator-running-p ()
  "Check if the emulator is running."
  (let ((devices (kotlin-development-get-adb-devices)))
    (seq-some (lambda (device) (string-match-p "emulator-" device)) devices)))

(defun kotlin-development-get-emulator-error ()
  "Get the error message from the emulator buffer."
  (when (get-buffer "*Android Emulator*")
    (with-current-buffer "*Android Emulator*"
      (buffer-string))))

(defun kotlin-development-start-emulator ()
  "Start the Android emulator asynchronously with status updates."
  (interactive)
  (let* ((emulator-path (expand-file-name "emulator/emulator" kotlin-development-android-sdk-path))
         (default-directory (file-name-directory emulator-path)))
    (if (not (file-executable-p emulator-path))
        (user-error "Emulator not found at %s" emulator-path)

      ;; Create or clear the emulator buffer
      (with-current-buffer (get-buffer-create "*Android Emulator*")
        (erase-buffer))

      (mode-line-hud:update :message (format "Starting emulator %s..." kotlin-development-emulator-name))

      ;; Kill any existing process and timer
      (when kotlin-development--emulator-process
        (delete-process kotlin-development--emulator-process))
      (when kotlin-development--emulator-check-timer
        (cancel-timer kotlin-development--emulator-check-timer))

      ;; Start emulator process with verbose output
      (setq kotlin-development--emulator-process
            (start-process "android-emulator" "*Android Emulator*"
                          emulator-path
                          "-avd" kotlin-development-emulator-name
                          "-verbose"))

      ;; Set up process sentinel with better error handling
      (set-process-sentinel
       kotlin-development--emulator-process
       (lambda (proc event)
         (message "Emulator process status: %s" event)
         (when (and (string-match-p "exited abnormally" event)
                   (get-buffer "*Android Emulator*"))
           (let ((error-output (kotlin-development-get-emulator-error)))
             (with-current-buffer (get-buffer-create "*Android Emulator Error*")
               (erase-buffer)
               (insert "Emulator failed to start. Error output:\n\n")
               (insert error-output)
               (display-buffer (current-buffer)))))))

      ;; Display the emulator buffer
      (when kotlin-development-debug
      (display-buffer "*Android Emulator*")))))

(defun kotlin-development-ensure-emulator ()
  "Ensure emulator is running, start it if not."
  (unless (kotlin-development-emulator-running-p)
    (kotlin-development-start-emulator)))

(defun kotlin-development-build-and-run ()
  "Build and run the Android app in the emulator."
  (interactive)
  ;; Find and set the project root directory globally
  (let ((root-dir (kotlin-development-find-project-root)))
    (unless root-dir
      (user-error "Could not find project root directory"))

    ;; Set the default-directory globally
    (setq default-directory root-dir)
    (message "Building and running Kotlin project in: %s" default-directory)

    (if (kotlin-development-emulator-running-p)
        (progn
          (mode-line-hud:update :message "Building...")
          ;; Use compile-in-project-root to ensure correct directory
          (let ((default-directory root-dir)) ; Ensure directory is set for compilation
            (compile (format "%s installDebug" kotlin-development-gradle-executable))
            (add-hook 'compilation-finish-functions
                     (lambda (buf status)
                       (when (string-match-p "finished" status)
                         (mode-line-hud:update :message "Build completed successfully! Launching app...")
                         ;; Increased delay to ensure installation is complete
                         (run-with-timer 2 nil #'kotlin-development-launch-app))))))

      ;; Emulator not running case
      (when kotlin-development-debug
        (message "Emulator not running, starting emulator first..."))
      (kotlin-development-start-emulator)

      ;; Cancel existing timer if any
      (when kotlin-development--build-timer
        (cancel-timer kotlin-development--build-timer))

      ;; Set up new timer
      (setq kotlin-development--build-timer
            (run-with-timer
             0 2
             (lambda ()
               (when (kotlin-development-check-emulator-status)
                 (when kotlin-development-debug
                   (mode-line-hud:update :message "Emulator is ready, starting build..."))
                 (let ((default-directory root-dir)) ; Ensure directory is set for compilation
                   (compile (format "%s installDebug" kotlin-development-gradle-executable))
                   (add-hook 'compilation-finish-functions
                            (lambda (buf status)
                              (when (string-match-p "finished" status)
                                (mode-line-hud:update :message "Build completed successfully! Launching app...")
                                (run-with-timer 2 nil #'kotlin-development-launch-app)))))
                 (cancel-timer kotlin-development--build-timer)
                 (setq kotlin-development--build-timer nil))))))))

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
  "Launch the installed Android app with better error handling and debugging."
  (interactive)
  (let* ((package-name (kotlin-development--get-package-name))
         (activity (kotlin-development--get-launch-activity))
         (adb-path (expand-file-name "platform-tools/adb" kotlin-development-android-sdk-path)))

    ;; Debug output
    (when kotlin-development-debug
      (message "Package name: %s" package-name)
      (message "Launch activity: %s" activity)
      (message "ADB path: %s" adb-path)

    (with-current-buffer (get-buffer-create "*Android Launch Debug*")
      (erase-buffer)
      (insert (format "Package Name: %s\n" package-name))
      (insert (format "Activity: %s\n" activity))
      (insert "Manifest contents:\n")
      (kotlin-development--debug-manifest)))

    (unless package-name
      (user-error "Could not determine package name.  Check *Android Launch Debug* buffer"))

    (unless activity
      (user-error "Could not determine launch activity.  Check *Android Launch Debug* buffer"))

    (message "Launching app: %s/%s" package-name activity)
    (let* ((full-activity (if (string-prefix-p "." activity)
                             (concat package-name activity)
                           activity))
           (launch-command (format "%s shell am start -n %s/%s"
                                 adb-path package-name full-activity))
           (result (shell-command-to-string launch-command)))

      ;; Log the launch attempt
      (when kotlin-development-debug
	(with-current-buffer "*Android Launch Debug*"
	  (insert "\nLaunch command:\n")
	  (insert launch-command)
	  (insert "\nResult:\n")
	  (insert result)))

      (if (string-match-p "Error\\|Exception" result)
          (user-error "Failed to launch app.  Check *Android Launch Debug* buffer")
        (message "App launched successfully: %s/%s" package-name full-activity)))))

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
  (eglot-ensure)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (setq-local comment-start "// "
              comment-end "")
  (when (fboundp 'mode-line-hud:notification)
    (mode-line-hud:notification
     :message (format "Initializing Kotlin LSP server: %s"
                     (propertize "kotlin-ls" 'face 'font-lock-keyword-face))
     :seconds 6)))

;;;###autoload
(defun kotlin-development-setup ()
  "Main setup function for Kotlin development environment."
  (interactive)
  ;; Verify LSP server first
  (kotlin-development-verify-lsp-server)

  ;; Register LSP server with eglot
  (add-to-list 'eglot-server-programs
               `((kotlin-mode kotlin-ts-mode) . (,kotlin-development-lsp-server-path)))

  ;; Configure tree-sitter
  (add-to-list 'treesit-language-source-alist
               '(kotlin "https://github.com/fwcd/tree-sitter-kotlin"))

  ;; Install grammar if needed
  (kotlin-development-install-tree-sitter-grammar)

  ;; Setup both modes
  (kotlin-development-common-hook))

;; Mode setup functions
;;;###autoload
(defun kotlin-development-kotlin-mode-setup ()
  "Setup for kotlin-mode."
  (use-package kotlin-mode
    :defer t
    :mode "\\.kt\\'"
    :hook (kotlin-mode . kotlin-development-setup)
    :config
    (setq-default compilation-scroll-output t)
    (setq-default kotlin-tab-width kotlin-development-indent-offset)))

;;;###autoload
(defun kotlin-development-ts-mode-setup ()
  "Setup for kotlin-ts-mode."
  (use-package kotlin-ts-mode
    :ensure t
    :mode "\\.kt\\'"
    :hook (kotlin-ts-mode . kotlin-development-setup)
    :config
    (setq-default compilation-scroll-output t)
    (setq kotlin-ts-mode-indent-offset kotlin-development-indent-offset)
    (add-to-list 'major-mode-remap-alist '(kotlin-mode . kotlin-ts-mode))))

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

(defun kotlin-development-setup-keys ()
  "Setup key bindings for Kotlin development."
  (let ((map-list '(kotlin-mode-map kotlin-ts-mode-map)))
    (dolist (map map-list)
      (when (boundp map)
	(define-key (symbol-value map) (kbd "M-s") #'kotlin-development-emulator-terminate-app)
	(define-key (symbol-value map) (kbd "M-r") #'kotlin-development-launch-app)
	(define-key (symbol-value map) (kbd "C-c C-e e") #'kotlin-development-select-emulator)
	(define-key (symbol-value map) (kbd "C-c C-e e") #'kotlin-development-select-emulator)
	(define-key (symbol-value map) (kbd "C-c C-c") #'kotlin-development-build-and-run)
	(define-key (symbol-value map) (kbd "C-c C-e s") #'kotlin-development-start-emulator)
	(define-key (symbol-value map) (kbd "C-c C-e k") #'kotlin-development-kill-emulator)
	(define-key (symbol-value map) (kbd "C-c C-e l") #'kotlin-development-list-emulators)))))

(kotlin-development-setup-keys)
;; Add this to your kotlin-development-common-hook
;; (define-key kotlin-mode-map (kbd "C-c C-c") #'kotlin-development-build-and-run)
;; (define-key kotlin-ts-mode-map (kbd "C-c C-c") #'kotlin-development-build-and-run)

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

(provide 'kotlin-development)

;;; kotlin-development.el ends here
