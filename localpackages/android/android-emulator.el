;;; android-emulator.el --- Android emulator management -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides functionality for managing Android emulators within Emacs.

;;; Code:

(require 'comint)
(require 'ansi-color)
(require 'mode-line-hud)

(defgroup android-emulator nil
  "Customization group for Android emulator management."
  :group 'android
  :prefix "android-emulator-")

(defcustom android-emulator-sdk-path
  (or (getenv "ANDROID_HOME")
      (getenv "ANDROID_SDK_ROOT")
      "~/library/android/sdk")
  "Path to Android SDK."
  :type 'string
  :group 'android-emulator)

(defcustom android-emulator-debug nil
  "Enable debug output for Android emulator."
  :type 'boolean
  :group 'android-emulator)

(defcustom android-emulator-language "Sv-SE"
  "Language to use for the emulator."
  :type 'string
  :group 'android-emulator)

(defcustom android-emulator-no-audio t
  "Whether to disable audio in the emulator.
When t, the emulator will start with -no-audio flag.
When nil, the emulator will start with audio enabled."
  :type 'boolean
  :group 'android-emulator)

(defface android-emulator-log-error-face
  '((t :foreground "red" :weight bold))
  "Face for error logs (E).")

(defface android-emulator-log-warning-face
  '((t :foreground "yellow"))
  "Face for warning logs (W).")

(defface android-emulator-log-info-face
  '((t :foreground "green"))
  "Face for info logs (I).")

(defface android-emulator-log-debug-face
  '((t :foreground "gray"))
  "Face for debug logs (D).")

(defface android-emulator-log-verbose-face
  '((t :foreground "white"))
  "Face for verbose logs (V).")

(defvar android-emulator-name nil
  "Name of the Android Virtual Device (AVD) to use.")

(defvar android-emulator--process nil
  "Process object for the running emulator.")

(defvar android-emulator--check-timer nil
  "Timer for checking emulator status.")

(defvar android-emulator-buffer-name "*Android Emulator*"
  "Name of the buffer where Android emulator output will be shown.")

(defvar android-emulator-error-buffer-name "*Android Emulator Error*"
  "Name of the buffer where Android emulator errors will be shown.")

(defvar android-emulator-logcat-buffer-name "*Android Logcat*"
  "Name of the buffer where logcat output will be shown.")

(defvar android-emulator-logcat-process nil
  "Process object for the running logcat monitor.")

(define-derived-mode android-emulator-mode comint-mode "Android Emulator"
  "Major mode for Android Emulator output."
  (ansi-color-for-comint-mode-on)
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 10000))

(define-derived-mode android-emulator-crash-mode compilation-mode "Android Crash"
  "Major mode for viewing Android crash logs."
  (setq-local font-lock-defaults (list android-emulator-crash-keywords))
  (setq-local compilation-error-regexp-alist
              '(("at \\([^(]+\\)(\\([^:]+\\):\\([0-9]+\\))" 1 2 3)))
  (read-only-mode 1))

(defcustom android-emulator-monitor-logcat t
  "Whether to automatically start monitoring logcat when emulator starts."
  :type 'boolean
  :group 'android-emulator)

(defcustom android-emulator-highlight-crashes t
  "Whether to highlight crash-related information in the emulator buffer."
  :type 'boolean
  :group 'android-emulator)

(defcustom android-emulator-crash-keywords
  '(("^\\(.*Exception.*\\)" 1 'error)
    ("^\\(.*at.*(.*)\\)" 1 'compilation-info)
    ("^\\(Caused by:.*\\)" 1 'error)
    ("^\\(FATAL EXCEPTION.*\\)" 1 'error)
    ("^\\(.*Error:.*\\)" 1 'warning))
  "Keywords for highlighting crash information."
  :type '(repeat (list string integer face))
  :group 'android-emulator)

(defcustom android-emulator-app-identifier nil
  "The application identifier (package name) to filter logcat output.
Example: com.example.myapp"
  :type '(choice (const :tag "None" nil)
                (string :tag "App Identifier"))
  :group 'android-emulator)

(defcustom android-emulator-logcat-extra-filters nil
  "Additional tags or filters for logcat.
Each element should be a string representing a tag or filter pattern."
  :type '(repeat string)
  :group 'android-emulator)

(defvar android-emulator-crash-buffer-name "*Android Crash Log*"
  "Name of the buffer where filtered crash logs will be shown.")

(defun android-emulator-set-app-identifier (identifier)
  "Set the app identifier for logcat filtering."
  (interactive "sEnter app identifier (package name): ")
  (setq android-emulator-app-identifier identifier)
  (when (and identifier android-emulator-logcat-process)
    (android-emulator-restart-logcat)))

(defun android-emulator-filter-crash-line (line)
  "Return non-nil if LINE contains crash-related information."
  (or (string-match-p "Exception" line)
      (string-match-p "^[[:space:]]*at " line)
      (string-match-p "Caused by:" line)
      (string-match-p "FATAL EXCEPTION" line)
      (string-match-p "Error:" line)))

(defun android-emulator-extract-crashes (text)
  "Extract crash information from TEXT and return relevant lines."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (crash-lines
          (in-crash nil))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
          (when (or (android-emulator-filter-crash-line line)
                   (and in-crash
                        (string-match-p "^[[:space:]]+" line)))
            (setq in-crash t)
            (push line crash-lines))
          (when (and in-crash
                     (string-match-p "^[[:space:]]*$" line))
            (setq in-crash nil)))
        (forward-line 1))
      (nreverse crash-lines))))

(defun android-emulator-show-crash-log ()
  "Show crash log in a dedicated buffer."
  (interactive)
  (when-let* ((buffer (get-buffer android-emulator-buffer-name))
              (content (with-current-buffer buffer
                        (buffer-string)))
              (crash-lines (android-emulator-extract-crashes content)))
    (with-current-buffer (get-buffer-create android-emulator-crash-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (android-emulator-crash-mode)
        (insert (mapconcat 'identity crash-lines "\n"))
        (goto-char (point-min)))
      (display-buffer (current-buffer)))))

(defun android-emulator-process-filter (proc string)
  "Process filter that handles ANSI colors and crash highlighting."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (inhibit-read-only t))
        (save-excursion
          (goto-char (process-mark proc))
          (let ((start (point)))
            (insert (ansi-color-apply string))
            ;; Apply crash highlighting if enabled
            (when android-emulator-highlight-crashes
              (save-excursion
                (goto-char start)
                (while (< (point) (process-mark proc))
                  (let ((line-end (line-end-position)))
                    (when (android-emulator-filter-crash-line
                           (buffer-substring-no-properties (line-beginning-position) line-end))
                      (font-lock-fontify-region (line-beginning-position) line-end))
                    (forward-line 1))))))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun android-emulator--expand-sdk-path ()
  "Expand the Android SDK path properly.
Throws an error if no valid SDK path is found."
  (let ((sdk-path (expand-file-name
                   (or (getenv "ANDROID_HOME")
                       (getenv "ANDROID_SDK_ROOT")
                       android-emulator-sdk-path))))
    (unless (file-directory-p sdk-path)
      (error "Android SDK not found at %s" sdk-path))
    sdk-path))

(defun android-emulator-get-avd-home ()
  "Get the AVD home directory."
  (let ((home (expand-file-name "~")))
    (or (getenv "ANDROID_AVD_HOME")
        (let ((sdk-home (getenv "ANDROID_SDK_HOME")))
          (when sdk-home
            (expand-file-name "avd" (expand-file-name sdk-home))))
        (expand-file-name ".android/avd" home))))

(defun android-emulator-get-adb-devices ()
  "Get list of connected Android devices."
  (let* ((adb-path (expand-file-name "platform-tools/adb" android-emulator-sdk-path))
         (output (shell-command-to-string (format "%s devices" adb-path))))
    (split-string output "\n" t)))

(defun android-emulator-get-available-avds ()
  "Get list of available Android Virtual Devices with debug output."
  (let* ((emulator-path (expand-file-name "emulator/emulator"
                                        (expand-file-name android-emulator-sdk-path)))
         (cmd (format "%s -list-avds" emulator-path))
         (output (shell-command-to-string cmd)))
    (when android-emulator-debug
      (message "Emulator path: %s" emulator-path)
      (message "List AVDs command: %s" cmd)
      (message "Raw AVD output: %s" output))
    (split-string output "\n" t "[ \t\n\r]+")))

(defun android-emulator-validate-avd (avd-name)
  "Validate that AVD-NAME exists and is properly configured."
  (let* ((avd-home (android-emulator-get-avd-home))
         (avd-ini (expand-file-name (concat avd-name ".ini") avd-home))
         (available-avds (android-emulator-get-available-avds)))
    (unless (member avd-name available-avds)
      (error "AVD '%s' not found in available AVDs: %s"
             avd-name
             (mapconcat 'identity available-avds ", ")))
    (unless (file-exists-p avd-home)
      (error "AVD home directory not found at: %s" avd-home))
    (unless (file-exists-p avd-ini)
      (error "AVD configuration not found at: %s" avd-ini))))

(defun android-emulator-running-p ()
  "Check if the emulator is running."
  (let ((devices (android-emulator-get-adb-devices)))
    (seq-some (lambda (device) (string-match-p "emulator-" device)) devices)))

(defun android-emulator-check-status ()
  "Check if emulator is fully booted."
  (let* ((adb-path (expand-file-name "platform-tools/adb" android-emulator-sdk-path))
         (output (shell-command-to-string
                 (format "%s shell getprop sys.boot_completed" adb-path))))
    (string-match-p "1" output)))

(defun android-emulator-get-error ()
  "Get the error message from the emulator buffer."
  (when (get-buffer android-emulator-buffer-name)
    (with-current-buffer android-emulator-buffer-name
      (buffer-string))))

(defun android-emulator-clear-buffer ()
  "Clear the Android emulator buffer."
  (interactive)
  (when (get-buffer android-emulator-buffer-name)
    (with-current-buffer android-emulator-buffer-name
      (erase-buffer))))

(defun android-emulator-stop ()
  "Stop the Android emulator."
  (interactive)
  (when android-emulator--process
    (delete-process android-emulator--process)
    (setq android-emulator--process nil)))

(defun android-emulator-cold-boot ()
  "Perform a cold boot of the Android emulator.
This function stops the current emulator if running, deletes the cache,
and starts a fresh instance of the emulator."
  (interactive)
  (when (and android-emulator-name
             (y-or-n-p "Are you sure you want to perform a cold boot? This will clear all emulator data."))
    (let ((cache-dir (expand-file-name (concat "~/.android/avd/" android-emulator-name ".avd/cache"))))
      ;; Stop current emulator if running
      (when android-emulator--process
        (android-emulator-stop))
      ;; Delete cache directory
      (when (file-exists-p cache-dir)
        (delete-directory cache-dir t))
      ;; Start emulator with -no-snapshot flag
      (let ((default-directory android-emulator-sdk-path)
            (command (list (concat (file-name-as-directory "emulator") "emulator")
                          "-avd" android-emulator-name
                          "-no-snapshot"
                          "-no-snapshot-load"
                          "-no-snapshot-save")))
        (setq android-emulator--process
              (make-process
               :name "android-emulator"
               :buffer (get-buffer-create android-emulator-buffer-name)
               :command command
               :filter #'android-emulator--process-filter
               :sentinel #'android-emulator--process-sentinel)))
      (message "Cold booting Android emulator %s..." android-emulator-name))))

(defun android-emulator-diagnose ()
  "Diagnose Android emulator setup issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*Android Emulator Diagnostic*")
    (erase-buffer)
    (insert "Android Emulator Diagnostic Report\n")
    (insert "================================\n\n")

    ;; Check environment variables
    (insert "Environment Variables:\n")
    (insert (format "ANDROID_HOME: %s\n" (or (getenv "ANDROID_HOME") "Not set")))
    (insert (format "ANDROID_SDK_ROOT: %s\n" (or (getenv "ANDROID_SDK_ROOT") "Not set")))
    (insert (format "ANDROID_AVD_HOME: %s\n" (or (getenv "ANDROID_AVD_HOME") "Not set")))
    (insert (format "ANDROID_SDK_HOME: %s\n" (or (getenv "ANDROID_SDK_HOME") "Not set")))
    (insert (format "JAVA_HOME: %s\n" (or (getenv "JAVA_HOME") "Not set")))
    (insert (format "GRADLE_JAVA_HOME: %s\n\n" (or (getenv "GRADLE_JAVA_HOME") "Not set")))

    ;; Check paths
    (let ((sdk-path android-emulator-sdk-path)
          (avd-home (android-emulator-get-avd-home))
          (emulator-path (expand-file-name "emulator/emulator" android-emulator-sdk-path)))
      (insert "Paths:\n")
      (insert (format "SDK Path: %s (exists: %s)\n"
                     sdk-path (if (file-exists-p sdk-path) "yes" "no")))
      (insert (format "AVD Home: %s (exists: %s)\n"
                     avd-home (if (file-exists-p avd-home) "yes" "no")))
      (insert (format "Emulator Path: %s (exists: %s)\n\n"
                     emulator-path (if (file-exists-p emulator-path) "yes" "no"))))

    ;; List available AVDs
    (insert "Available AVDs:\n")
    (dolist (avd (android-emulator-get-available-avds))
      (insert (format "- %s\n" avd)))
    (insert "\n")

    ;; Check current AVD
    (insert "Current AVD Configuration:\n")
    (insert (format "Selected AVD: %s\n" android-emulator-name))
    (let ((avd-ini (expand-file-name
                    (concat android-emulator-name ".ini")
                    (android-emulator-get-avd-home))))
      (insert (format "AVD Config: %s (exists: %s)\n"
                     avd-ini (if (file-exists-p avd-ini) "yes" "no"))))

    (display-buffer (current-buffer))))

(defun android-emulator-list ()
  "List available Android emulators."
  (interactive)
  (let* ((emulator-path (expand-file-name "emulator/emulator" android-emulator-sdk-path))
         (output (shell-command-to-string (format "%s -list-avds" emulator-path))))
    (with-current-buffer (get-buffer-create "*Android Emulators*")
      (erase-buffer)
      (insert "Available Android Emulators:\n\n")
      (insert output)
      (display-buffer (current-buffer)))))

(defun android-emulator-select ()
  "Select an Android emulator interactively with debug info."
  (interactive)
  (if android-emulator-name
      android-emulator-name
    (let* ((avds (android-emulator-get-available-avds)))
      (when android-emulator-debug
        (message "Available AVDs: %s" avds))
      (if (null avds)
          (error "No AVDs found. Create one using Android Studio first")
        (let ((selected (completing-read "Select emulator: " avds nil t)))
          (setq android-emulator-name selected)
          (when android-emulator-debug
            (message "Selected emulator: %s" selected)))
        android-emulator-name))))

(defun android-emulator-start ()
  "Start the Android emulator asynchronously with status updates.
Handles process cleanup and provides detailed error reporting."
  (interactive)
  (when android-emulator--process
    (android-emulator-kill))
  
  (let* ((sdk-path (android-emulator--expand-sdk-path))
         (emulator-path (expand-file-name "emulator/emulator" sdk-path))
         (default-directory (file-name-directory emulator-path))
         (android-emulator-name (android-emulator-select)))
    
    (when android-emulator-debug
      (message "Starting emulator with SDK: %s" sdk-path))

    (when android-emulator-debug
      (message "Starting emulator with:")
      (message "  SDK path: %s" sdk-path)
      (message "  Emulator path: %s" emulator-path)
      (message "  AVD name: %s" android-emulator-name)
      (message "  AVD home: %s" (android-emulator-get-avd-home)))

    (unless (file-executable-p emulator-path)
      (error "Emulator not found or not executable at %s" emulator-path))

    (let* ((avd-home (android-emulator-get-avd-home))
           (avd-ini (expand-file-name (concat android-emulator-name ".ini") avd-home))
           (avd-config (expand-file-name (concat android-emulator-name ".avd") avd-home)))

      (when android-emulator-debug
        (message "Checking AVD configuration:")
        (message "  AVD home: %s (exists: %s)" avd-home (file-exists-p avd-home))
        (message "  AVD ini: %s (exists: %s)" avd-ini (file-exists-p avd-ini))
        (message "  AVD config: %s (exists: %s)" avd-config (file-exists-p avd-config)))

      (let ((buffer (get-buffer-create android-emulator-buffer-name)))
        (with-current-buffer buffer
          (unless (eq major-mode 'android-emulator-mode)
            (android-emulator-mode))
          (erase-buffer))

        (mode-line-hud:update :message
                              (format "Starting emulator %s..." android-emulator-name))

        ;; Kill any existing process and timer
        (when android-emulator--process
          (delete-process android-emulator--process))
        (when android-emulator--check-timer
          (cancel-timer android-emulator--check-timer))

        ;; Change to the emulator directory before starting
        (let ((default-directory (file-name-directory emulator-path))
              (args (append (list "-avd" android-emulator-name)
                          (when android-emulator-no-audio (list "-no-audio"))
                          (list "-change-locale" android-emulator-language))))
          (setq android-emulator--process
                (apply #'start-process
                       "android-emulator"
                       buffer
                       emulator-path
                       args)))

        ;; Set process as the buffer's process
        (set-process-buffer android-emulator--process buffer)

        ;; Enable comint input
        (with-current-buffer buffer
          (set (make-local-variable 'comint-process-echoes) t)
          (set (make-local-variable 'comint-use-prompt-regexp) nil))

        ;; Set up process filter for ANSI color handling
	(set-process-filter
	 android-emulator--process
	 (lambda (proc string)
	   (when (buffer-live-p (process-buffer proc))
	     (with-current-buffer (process-buffer proc)
	       (let ((moving (= (point) (process-mark proc))))
		 (save-excursion
		   (goto-char (process-mark proc))
		   (insert (ansi-color-apply string))
		   (set-marker (process-mark proc) (point)))
		 (if moving (goto-char (process-mark proc))))))))

	;; Set up process sentinel with error handling
	(android-emulator--setup-process-sentinel android-emulator--process)
        ;; (display-buffer buffer)
	))))

(defun android-emulator-ensure ()
  "Ensure emulator is running, start it if not."
  (unless (android-emulator-running-p)
    (android-emulator-start)))

(defun android-emulator-open-room-db ()
  "Find the Room database file for the currently running emulator and open it in `sqlite-mode' in Emacs."
  (interactive)
  (let* ((adb-path (expand-file-name "platform-tools/adb" (android-emulator--expand-sdk-path)))
         (device (car (android-emulator-get-adb-devices)))
         (emulator-avd-home (android-emulator-get-avd-home))
         (db-path (concat emulator-avd-home "/" device "/databases"))
         (db-file (read-file-name "Select Room database file: " db-path nil t)))
    (when (and device db-file)
      (let ((local-db-file (concat "/tmp/" (file-name-nondirectory db-file))))
        (start-process "adb-pull" "*adb-pull*"
                       adb-path "pull" db-file local-db-file)
        (message "Pulling database from emulator...")
        (set-process-sentinel
         (get-process "adb-pull")
         (lambda (proc _event)
           (if (= (process-exit-status proc) 0)
               (progn
                 (find-file local-db-file)
                 (sqlite-mode))
             (message "Failed to pull the database."))))))))

(defun android-emulator-unload-function ()
  "Clean up resources when unloading the package."
  (android-emulator-kill))

(defun android-emulator-debug-on ()
  "Enable debugging output for android-emulator."
  (interactive)
  (setq android-emulator-debug t)
  (message "Android emulator debugging enabled"))

(defun android-emulator-restart-logcat ()
  "Restart the logcat monitoring with current filters."
  (interactive)
  (android-emulator-quit-logcat)
  (android-emulator-start-logcat))

(defun android-emulator-toggle-logcat ()
  "Toggle logcat monitoring on or off.
If logcat is currently running, stop it. Otherwise, start it."
  (interactive)
  (if android-emulator-logcat-process
      (progn
        (android-emulator-quit-logcat)
        (message "Logcat monitoring stopped"))
    (android-emulator-start-logcat)
    (message "Logcat monitoring started")))

(defun android-emulator-get-package-name ()
  "Get the package name from the Android project.
Checks in this order:
1. kotlin-development-package-name variable
2. AndroidManifest.xml
3. build.gradle files
4. Asks user if none found."
  (let ((package-name (or (and (boundp 'kotlin-development-package-name)
                           kotlin-development-package-name)
                         (android-emulator--get-package-from-manifest)
                         (android-emulator--get-package-from-gradle)
                         android-emulator-app-identifier)))
    (when (and android-emulator-debug package-name)
      (message "Found package name: %s" package-name))
    
    (unless package-name
      (setq package-name (read-string "Could not determine package name. Please enter: "))
      (when (boundp 'kotlin-development-package-name)
        (setq kotlin-development-package-name package-name))
      (setq android-emulator-app-identifier package-name))
    
    package-name))

(defun android-emulator--get-package-from-manifest ()
  "Extract package name from AndroidManifest.xml file."
  (let ((manifest (android-emulator--find-manifest)))
    (when manifest
      (with-temp-buffer
        (insert-file-contents manifest)
        (goto-char (point-min))
        (when (re-search-forward "package=\"\\([^\"]+\\)\"" nil t)
          (match-string 1))))))

(defun android-emulator--get-package-from-gradle ()
  "Extract package name from build.gradle or build.gradle.kts files.
Looks for applicationId or namespace in both Groovy and Kotlin DSL."
  (let* ((project-root (or (android-emulator-find-project-root)
                          (and (boundp 'kotlin-development-current-root)
                               kotlin-development-current-root)
                          default-directory))
         (gradle-files (directory-files-recursively 
                        project-root 
                        "\\(build\\.gradle\\(\\.kts\\)?\\)$")))
    (catch 'found
      (dolist (gradle-file gradle-files)
        (with-temp-buffer
          (insert-file-contents gradle-file)
          (goto-char (point-min))
          ;; Check for applicationId in Kotlin DSL
          (when (re-search-forward "applicationId\\s*=\\s*\"\\([^\"]+\\)\"" nil t)
            (throw 'found (match-string 1)))
          ;; Check for applicationId in Groovy DSL
          (goto-char (point-min))
          (when (re-search-forward "applicationId\\s+['\"]\\([^'\"]+\\)['\"]" nil t)
            (throw 'found (match-string 1)))
          ;; Check for namespace in Kotlin DSL
          (goto-char (point-min))
          (when (re-search-forward "namespace\\s*=\\s*\"\\([^\"]+\\)\"" nil t)
            (throw 'found (match-string 1)))
          ;; Check for namespace in Groovy DSL
          (goto-char (point-min))
          (when (re-search-forward "namespace\\s+['\"]\\([^'\"]+\\)['\"]" nil t)
            (throw 'found (match-string 1))))))))

(defun android-emulator-find-project-root ()
  "Find the root directory of the Android project.
Looks for common Android project indicators like settings.gradle."
  (locate-dominating-file 
   default-directory
   (lambda (dir)
     (or (file-exists-p (expand-file-name "settings.gradle" dir))
         (file-exists-p (expand-file-name "settings.gradle.kts" dir))
         (file-exists-p (expand-file-name "gradlew" dir))))))

(defun android-emulator--find-manifest ()
  "Find the AndroidManifest.xml file in the project."
  (let ((project-root (android-emulator-find-project-root)))
    (when project-root
      (car (directory-files-recursively 
            project-root 
            "AndroidManifest\\.xml$")))))

(defun android-emulator-build-logcat-command ()
  "Build the logcat command with appropriate filters."
  (let ((adb-path (expand-file-name "platform-tools/adb" android-emulator-sdk-path)))
    (if android-emulator-app-identifier
        ;; Använd logcat's inbyggda pid-filter och kombinera med grep för säkerhet
        (format "%s shell pidof %s | xargs %s logcat --pid"
                adb-path
                (shell-quote-argument android-emulator-app-identifier)
                adb-path)
      ;; Annars, använd basic logcat
      (format "%s logcat -v threadtime" adb-path))))

(defun android-emulator-logcat-filter (proc string)
  "Process filter for logcat output that handles crashes and colors."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (inhibit-read-only t)
            (buffer-read-only nil))  ; Make sure buffer is writable
        (save-excursion
          (goto-char (process-mark proc))
          (let ((start (point)))
            ;; Insert och färglägg rad för rad
            (dolist (line (split-string string "\n" t))
              (let ((colored-line (android-emulator-colorize-log-line line)))
                (put-text-property 0 (length colored-line) 'font-lock-face
                                 (get-text-property 0 'face colored-line) colored-line)
                (insert colored-line "\n")))
            (when android-emulator-highlight-crashes
              (save-excursion
                (goto-char start)
                (while (< (point) (process-mark proc))
                  (let ((line-end (line-end-position)))
                    (when (android-emulator-filter-crash-line
                           (buffer-substring-no-properties (line-beginning-position) line-end))
                      (font-lock-fontify-region (line-beginning-position) line-end))
                    (forward-line 1))))))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun android-emulator-start-logcat ()
  "Start monitoring logcat in a separate buffer with app filtering."
  (interactive)
  (let ((buffer (get-buffer-create android-emulator-logcat-buffer-name)))
    ;; Kill existing logcat process if any
    (when android-emulator-logcat-process
      (delete-process android-emulator-logcat-process))

    (with-current-buffer buffer
      (unless (eq major-mode 'android-emulator-crash-mode)
        (android-emulator-crash-mode))
      (erase-buffer)

      ;; Add header showing current filters
      (let ((inhibit-read-only t))
        (insert "=== Android Logcat Monitor ===\n")
        (when android-emulator-app-identifier
          (insert (format "Filtering for app: %s\n" android-emulator-app-identifier)))
        (insert "===========================\n\n"))

      ;; Enable visual line mode
      (setq-default truncate-lines t)
      (visual-line-mode 1))

    (let* ((command (android-emulator-build-logcat-command))
           (proc (start-process-shell-command
                 "android-logcat"
                 buffer
                 command)))

      (setq android-emulator-logcat-process proc)
      (set-process-filter proc #'android-emulator-logcat-filter)

      (set-process-sentinel
       proc
       (lambda (proc event)
         (when (string-match-p "\\(finished\\|exited\\)" event)
           (when android-emulator-debug
             (message "Logcat process ended: %s" event)))))

      (display-buffer buffer)

      (when android-emulator-debug
        (message "Started logcat with command: %s" command)))))

(defun android-emulator-quit-logcat ()
  "Stop the logcat monitoring process and kill its buffer."
  (interactive)
  (when android-emulator-logcat-process
    (delete-process android-emulator-logcat-process)
    (setq android-emulator-logcat-process nil))
  (when (get-buffer android-emulator-logcat-buffer-name)
    (kill-buffer android-emulator-logcat-buffer-name)))

(defun android-emulator-kill ()
  "Kill the running emulator and clean up processes."
  (interactive)
  (when android-emulator--check-timer
    (cancel-timer android-emulator--check-timer)
    (setq android-emulator--check-timer nil))

  (when android-emulator--process
    (delete-process android-emulator--process)
    (setq android-emulator--process nil))

  (android-emulator-quit-logcat)  ; Add this line

  (let ((adb-path (expand-file-name "platform-tools/adb" android-emulator-sdk-path)))
    (shell-command (format "%s emu kill" adb-path))
    (when android-emulator-debug
      (message "Emulator shutdown initiated."))))

(defun android-emulator--setup-process-sentinel (proc)
  "Set up the process sentinel for the emulator process PROC."
  (set-process-sentinel
   proc
   (lambda (proc event)
     (message "Emulator process status: %s" (string-trim event))
     (cond
      ;; When the emulator starts successfully
      ((string-match-p "run" event)
       (when android-emulator-monitor-logcat
         (run-with-timer 2 nil #'android-emulator-start-logcat)))
      ;; When the emulator exits abnormally
      ((and (string-match-p "exited abnormally" event)
            (buffer-live-p (process-buffer proc)))
       (let ((error-output (android-emulator-get-error)))
         (with-current-buffer (get-buffer-create android-emulator-error-buffer-name)
           (erase-buffer)
           (insert "Emulator failed to start. Error output:\n\n")
           (insert error-output)
	   (setq-default truncate-lines t)
	   (visual-line-mode 1)
           (display-buffer (current-buffer)))))))))

(defun android-emulator-send-notification (title message &optional package-name)
  "Send a notification to the Android emulator.
TITLE: Notification title.
MESSAGE: Notification content.
PACKAGE-NAME: Optional, defaults to `android-emulator-app-identifier'."
  (interactive
   (list (read-string "Notification title: ")
         (read-string "Notification message: ")
         (read-string "Package name: " android-emulator-app-identifier)))
  (let* ((adb-path (expand-file-name "platform-tools/adb" android-emulator-sdk-path))
         (package (or package-name android-emulator-app-identifier))
         (cmd (format "%s shell am broadcast -a %s.SHOW_NOTIFICATION \
--es title %s --es message %s"
                      adb-path
                      package
                      (shell-quote-argument title)
                      (shell-quote-argument message))))
    
    (unless (executable-find adb-path)
      (error "ADB not found at: %s" adb-path))
    
    (unless package
      (error "Package name required (set android-emulator-app-identifier)"))
    
    (message "Executing: %s" cmd)
    (let ((result (shell-command-to-string cmd)))
      (if (string-match "Broadcast completed" result)
          (message "Notification sent successfully!")
        (message "FAILED: %s" result))
      result)))

(defun android-emulator-debug-notification ()
  "Send a test notification with extensive debugging output."
  (interactive)
  (let* ((adb-path (expand-file-name "platform-tools/adb" android-emulator-sdk-path))
         (package-name (or android-emulator-app-identifier
                           (android-emulator-get-package-name)
                           (read-string "Package name: ")))
         (debug-buffer (get-buffer-create "*Android Notification Debug*")))
    
    (with-current-buffer debug-buffer
      (erase-buffer)
      (insert "=== Android Notification Debugging ===\n\n")
      (insert (format "ADB Path: %s\n" adb-path))
      (insert (format "Package Name: %s\n\n" package-name))
      
      ;; Check if emulator is running
      (insert "Checking emulator status...\n")
      (let ((devices (android-emulator-get-adb-devices)))
        (insert (format "Connected devices: %s\n" (mapconcat 'identity devices ", ")))
        (if (android-emulator-running-p)
            (insert "Emulator is running: YES\n")
          (insert "Emulator is running: NO\n")))
      
      ;; Check if package exists on device
      (insert "\nChecking if package exists on device...\n")
      (let ((cmd (format "%s shell pm list packages | grep %s" adb-path package-name))
            (result (shell-command-to-string (format "%s shell pm list packages | grep %s" 
                                                     adb-path package-name))))
        (insert (format "Command: %s\n" cmd))
        (insert (format "Result: %s\n" result))
        (if (string-match-p package-name result)
            (insert "Package found on device: YES\n")
          (insert "Package found on device: NO\n")))
      
      ;; Try all notification methods with detailed output
      (insert "\n=== Testing Notification Methods ===\n\n")
      
      ;; Method 1: Direct broadcast to NotificationReceiver
      (insert "Method 1: Direct broadcast to NotificationReceiver\n")
      (let* ((cmd (format "%s shell am broadcast -a android.intent.action.MAIN -e title \"Debug Title\" -e message \"Debug Message\" -n %s/.NotificationReceiver"
                          adb-path package-name))
             (result (shell-command-to-string cmd)))
        (insert (format "Command: %s\n" cmd))
        (insert (format "Result: %s\n\n" result)))
      
      ;; Method 2: Using activity manager to start notification service
      (insert "Method 2: Using activity manager to start notification service\n")
      (let* ((cmd (format "%s shell am startservice -n %s/.NotificationService --es title \"Debug Title\" --es message \"Debug Message\""
                          adb-path package-name))
             (result (shell-command-to-string cmd)))
        (insert (format "Command: %s\n" cmd))
        (insert (format "Result: %s\n\n" result)))
      
      ;; Method 3: Custom action broadcast with different quoting
      (insert "Method 3: Custom action broadcast with different quoting\n")
      (let* ((cmd (format "%s shell \"am broadcast -a '%s.SHOW_NOTIFICATION' --es title 'Debug Title' --es message 'Debug Message'\""
                          adb-path package-name))
             (result (shell-command-to-string cmd)))
        (insert (format "Command: %s\n" cmd))
        (insert (format "Result: %s\n\n" result)))
      
      ;; Method 4: Direct intent to MainActivity
      (insert "Method 4: Direct intent to MainActivity\n")
      (let* ((cmd (format "%s shell am start -a android.intent.action.MAIN -n %s/.MainActivity --es notification_title \"Debug Title\" --es notification_message \"Debug Message\""
                          adb-path package-name))
             (result (shell-command-to-string cmd)))
        (insert (format "Command: %s\n" cmd))
        (insert (format "Result: %s\n\n" result)))
      
      ;; Check for NotificationReceiver in manifest
      (insert "\n=== Checking App Configuration ===\n\n")
      (let* ((cmd (format "%s shell pm dump %s | grep -A 20 \"DUMP OF SERVICE package:\"" 
                          adb-path package-name))
             (result (shell-command-to-string cmd)))
        (insert "Package dump (partial):\n")
        (insert result)
        (insert "\n"))
      
      ;; Final instructions
      (insert "\n=== Troubleshooting ===\n\n")
      (insert "If notifications are not appearing, check:\n")
      (insert "1. The app has proper notification permissions\n")
      (insert "2. NotificationReceiver is properly registered in AndroidManifest.xml\n")
      (insert "3. The device is not in Do Not Disturb mode\n")
      (insert "4. Try rebuilding and reinstalling the app\n")
      (insert "5. Check logcat for any errors related to notifications\n"))
    
    ;; Display the debug buffer
    (display-buffer debug-buffer)
    
    ;; Also try sending a notification with the regular function
    (android-emulator-send-notification "Debug Title" "Debug Message from debug function" package-name)))

(defun android-emulator-colorize-log-line (line)
  "Add colors to a log line based on its log level."
  (cond
   ;; Error
   ((string-match "\\([[:space:]]\\|^\\)E/\\|[[:space:]]E[[:space:]]" line)
    (propertize line 'face 'android-emulator-log-error-face))
   ;; Warning
   ((string-match "\\([[:space:]]\\|^\\)W/\\|[[:space:]]W[[:space:]]" line)
    (propertize line 'face 'android-emulator-log-warning-face))
   ;; Info
   ((string-match "\\([[:space:]]\\|^\\)I/\\|[[:space:]]I[[:space:]]" line)
    (propertize line 'face 'android-emulator-log-info-face))
   ;; Debug
   ((string-match "\\([[:space:]]\\|^\\)D/\\|[[:space:]]D[[:space:]]" line)
    (propertize line 'face 'android-emulator-log-debug-face))
   ;; Verbose
   ((string-match "\\([[:space:]]\\|^\\)V/\\|[[:space:]]V[[:space:]]" line)
    (propertize line 'face 'android-emulator-log-verbose-face))
   ;; Default case
   (t line)))

(defvar android-emulator-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'android-emulator-show-crash-log)
    (define-key map (kbd "C-c C-k") 'android-emulator-clear-buffer)
    (define-key map (kbd "C-c C-l") 'android-emulator-toggle-logcat)
    (define-key map (kbd "C-c C-f") 'android-emulator-set-app-identifier)
    (define-key map (kbd "C-c C-r") 'android-emulator-restart-logcat)
    (define-key map (kbd "C-c C-n") 'android-emulator-send-notification)
    (define-key map (kbd "C-c C-d") 'android-emulator-debug-notification)
    map)
  "Keymap for Android Emulator mode.")

(provide 'android-emulator)
;;; android-emulator.el ends here
