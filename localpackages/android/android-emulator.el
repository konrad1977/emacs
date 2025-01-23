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
  "Expand the Android SDK path properly."
  (expand-file-name
   (or (getenv "ANDROID_HOME")
       (getenv "ANDROID_SDK_ROOT")
       android-emulator-sdk-path)))

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
    (insert (format "ANDROID_SDK_HOME: %s\n\n" (or (getenv "ANDROID_SDK_HOME") "Not set")))

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
  "Start the Android emulator asynchronously with status updates."
  (interactive)
  (let* ((sdk-path (android-emulator--expand-sdk-path))
         (emulator-path (expand-file-name "emulator/emulator" sdk-path))
         (default-directory (file-name-directory emulator-path))
         (android-emulator-name (android-emulator-select)))

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
        (let ((default-directory (file-name-directory emulator-path)))
          (setq android-emulator--process
                (start-process
                 "android-emulator"
                 buffer
                 emulator-path
                 "-avd" android-emulator-name
                 "-no-audio"
		 ;; "-change-locale" android-emulator-language
                 ;; "-verbose"
                 )))

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
  (android-emulator-stop-logcat)
  (android-emulator-start-logcat))

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

(defun android-emulator-stop-logcat ()
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

  (android-emulator-stop-logcat)  ; Add this line

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
    (define-key map (kbd "C-c C-l") 'android-emulator-start-logcat)
    (define-key map (kbd "C-c C-f") 'android-emulator-set-app-identifier)
    (define-key map (kbd "C-c C-r") 'android-emulator-restart-logcat)
    map)
  "Keymap for Android Emulator mode.")

(provide 'android-emulator)
;;; android-emulator.el ends here
