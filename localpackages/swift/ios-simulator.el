;;; Simulator --- A small package for viewing iOS simulator logs -*- lexical-binding: t -*-
;;; Commentary: This package provides some support for iOS Simulator
;;; Code:

(require 'nerd-icons nil t)
(require 'json)
(require 'cl-lib)
(require 'swift-cache nil t)

(with-eval-after-load 'periphery-helper
 (require 'periphery-helper))

(with-eval-after-load 'mode-line-hud
  (require 'mode-line-hud))

(with-eval-after-load 'xcode-additions
  (require 'xcode-additions))

(with-eval-after-load 'swift-project
  (require 'swift-project))

(with-eval-after-load 'json
  (require 'json))

(defcustom ios-simulator:debug nil
  "Enable debug mode for iOS simulator."
  :type 'boolean
  :group 'ios-simulator)

(defgroup ios-simulator nil
  "IOS-SIMULATOR."
  :tag "ios-simulator"
  :group 'applications)

(defcustom ios-simulator-default-language "sv-SE"
  "Default language for the simulator."
  :type 'string
  :group 'ios-simulator)

(defface ios-simulator-background-face
  `((t (:inherit default :height 150)))
  "Buffer background color."
  :group 'ios-simulator)

(defconst ios-simulator-buffer-name "*iOS Simulator*"
  "Name of the buffer.")

(defconst list-simulators-command
  "xcrun simctl list devices available -j"
  "List available simulators.")

(defconst ios-simulator:get-booted-simulator-command
  "xcrun simctl list devices | grep -m 1 \"(Booted)\" | grep -E -o -i \"([0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12})\""
  "Get booted simulator id if any.")

(defvar-local ios-simulator-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i t") #'ios-simulator:terminate-current-app)
    (define-key map (kbd "C-c i l") #'ios-simulator:change-language)
    (define-key map (kbd "C-c i c") #'ios-simulator:appcontainer)
    (define-key map (kbd "C-c i n") #'ios-simulator:send-notification)
    map)
  "Keymap for `ios-simulator-mode'.")

;;;###autoload
(define-minor-mode ios-simulator-mode
  "Minor mode for iOS simulator integration."
  :init-value nil
  :lighter " iOS-Sim"
  :keymap ios-simulator-mode-map
  :group 'ios-simulator
  (if ios-simulator-mode
      (ios-simulator:mode-setup)
    (ios-simulator:mode-teardown)))


(defvar-local ios-simulator--installation-process nil
  "Process object for the current app installation.")

(defvar-local ios-simulator--current-simulator-name nil)
(defvar-local current-simulator-id nil)
(defvar-local current-app-identifier nil)
(defvar-local current-app-name nil)
;; Removed unused variable: use-rosetta (was always nil)

;; Multi-simulator support
(defvar ios-simulator--active-simulators (make-hash-table :test 'equal)
  "Hash table mapping simulator IDs to their info (name, app, buffer).")
(defvar ios-simulator--simulator-buffers (make-hash-table :test 'equal)
  "Hash table mapping simulator IDs to their buffer names.")
(defvar ios-simulator--target-simulators nil
  "List of simulator IDs to launch app on. When non-nil, app will launch on all these simulators.")

;; Legacy cache variables - kept for compatibility but now use swift-cache
(defvar ios-simulator--cached-devices nil
  "Deprecated: Now using swift-cache. Cached list of available simulator devices.")
(defvar ios-simulator--cache-timestamp nil
  "Deprecated: Now using swift-cache. Timestamp when devices were last cached.")
(defconst ios-simulator--cache-ttl 300
  "Deprecated: Now using swift-cache. Cache TTL in seconds (5 minutes).")

(defvar ios-simulator-ready-hook nil
  "Hook run when the simulator is ready for app installation.")

(defvar-local current-root-folder-simulator nil)

(defvar-local current-language-selection ios-simulator-default-language
  "Current language selection for the simulator.")


(defun ios-simulator:mode-setup ()
  "Setup the iOS simulator mode."
  (add-hook 'kill-buffer-hook #'ios-simulator:cleanup nil t))

(defun ios-simulator:mode-teardown ()
  "Teardown the iOS simulator mode."
  (remove-hook 'kill-buffer-hook #'ios-simulator:cleanup t))

(defun ios-simulator:cleanup ()
  "Clean up simulator resources."
  (when ios-simulator--installation-process
    (delete-process ios-simulator--installation-process)))

(defun ios-simulator:reset ()
  "Reset current settings."
  (setq ios-simulator--current-simulator-name nil
        current-simulator-id nil
        current-app-identifier nil
        current-app-name nil
        current-language-selection ios-simulator-default-language
        current-root-folder-simulator nil)
  (ios-simulator:kill-buffer)
  ;; (ios-simulator:shut-down-all)
  )

(defun ios-simulator-current-sdk-version ()
  "Get the current simulator sdk-version."
  (string-trim (shell-command-to-string "xcrun --sdk iphonesimulator --show-sdk-version")))

;;;###autoload
(defun ios-simulator-sdk-path ()
  "Get the current simulator sdk-path."
  (string-trim (shell-command-to-string "xcrun --show-sdk-path --sdk iphonesimulator")))

(defun ios-simulator-current-arch ()
  "Get the current arch."
  (string-trim (shell-command-to-string "clang -print-target-triple")))

(defun ios-simulator:shut-down-all ()
  "Shut down all simulators."
  (call-process-shell-command "xcrun simctl shutdown all"))

(defun ios-simulator-target ()
  "Get the current simulator sdk."
  (let* ((target-components (split-string (ios-simulator-current-arch) "-"))
         (arch (nth 0 target-components))
         (vendor (nth 1 target-components))
         (version (ios-simulator-current-sdk-version)))
    (format "%s-%s-ios%s-simulator" arch vendor version)))

(cl-defun ios-simulator:install-and-run-app (&key rootfolder &key build-folder &key simulatorId &key appIdentifier &key terminate-first)
  "Install app in simulator with ROOTFOLDER BUILD-FOLDER SIMULATORID, APPIDENTIFIER BUFFER.
If TERMINATE-FIRST is non-nil, terminate existing app instance before installing.
If ios-simulator--target-simulators is set, launches on all specified simulators."
  (when ios-simulator:debug
    (message "Install-and-run root: %s build:%s" rootfolder build-folder))

  ;; Get the list of simulators to launch on
  (let* ((default-directory rootfolder)
         (primary-simulator-id (or simulatorId (ios-simulator:simulator-identifier)))
         (target-simulator-ids (if ios-simulator--target-simulators
                                   ios-simulator--target-simulators
                                 (list primary-simulator-id)))
         (applicationName (ios-simulator:get-app-name-fast build-folder)))

    (when ios-simulator:debug
      (message "Installing app: %s on %d simulator(s)"
               (or applicationName "Unknown")
               (length target-simulator-ids)))

    (setq current-simulator-id primary-simulator-id
          current-app-identifier appIdentifier
          current-root-folder-simulator rootfolder
          current-app-name applicationName)

    ;; Launch on all target simulators
    (dolist (simulator-id target-simulator-ids)
      (let* ((simulatorName (ios-simulator:simulator-name-from :id simulator-id))
             (buffer (if (string= simulator-id primary-simulator-id)
                        (get-buffer-create ios-simulator-buffer-name)
                      (ios-simulator:get-or-create-buffer-for-simulator simulator-id simulatorName))))

        (when ios-simulator:debug
          (message "Installing app: %s for simulator: %s (ID: %s)"
                   (or applicationName "Unknown")
                   (or simulatorName "Unknown")
                   (or simulator-id "Unknown")))

        ;; Store simulator info if not primary
        (unless (string= simulator-id primary-simulator-id)
          (puthash simulator-id
                   (list :name simulatorName
                         :app-identifier appIdentifier
                         :app-name applicationName
                         :buffer buffer)
                   ios-simulator--active-simulators))

        ;; Setup and boot simulator
        (ios-simulator:setup-simulator-dwim simulator-id)

        ;; Create closures that capture the current values
        (let ((current-sim-id simulator-id)
              (current-sim-name simulatorName)
              (current-buffer buffer)
              (current-buffer-name (buffer-name buffer))
              (current-app-name applicationName)
              (current-app-id appIdentifier)
              (current-build-folder build-folder)
              (current-terminate-first terminate-first))
          (if current-terminate-first
              ;; Terminate app asynchronously
              (make-process
               :name (format "terminate-app-%s" current-sim-id)
               :command (list "xcrun" "simctl" "terminate" current-sim-id current-app-id)
               :noquery t
               :sentinel (lambda (proc event)
                           ;; Install immediately after termination
                           (ios-simulator:install-app
                            :simulatorID current-sim-id
                            :build-folder current-build-folder
                            :appname current-app-name
                            :callback (lambda ()
                                        (when ios-simulator:debug
                                          (message "App installation completed on %s, launching app" current-sim-name))
                                        ;; Ensure buffer is still alive, recreate if needed
                                        (let ((launch-buffer (if (buffer-live-p current-buffer)
                                                                current-buffer
                                                              (get-buffer-create current-buffer-name))))
                                          (ios-simulator:launch-app
                                           :appIdentifier current-app-id
                                           :applicationName current-app-name
                                           :simulatorName current-sim-name
                                           :simulatorID current-sim-id
                                           :buffer launch-buffer
                                           :terminate-running current-terminate-first))))))
            ;; Install directly without terminating
            (ios-simulator:install-app
             :simulatorID current-sim-id
             :build-folder current-build-folder
             :appname current-app-name
             :callback (lambda ()
                         (when ios-simulator:debug
                           (message "App installation completed on %s, launching app" current-sim-name))
                         ;; Ensure buffer is still alive, recreate if needed
                         (let ((launch-buffer (if (buffer-live-p current-buffer)
                                                 current-buffer
                                               (get-buffer-create current-buffer-name))))
                           (ios-simulator:launch-app
                            :appIdentifier current-app-id
                            :applicationName current-app-name
                            :simulatorName current-sim-name
                            :simulatorID current-sim-id
                            :buffer launch-buffer
                            :terminate-running current-terminate-first))))))))))

(cl-defun ios-simulator:install-app (&key simulatorID &key build-folder &key appname &key callback)
  "Install app (as SIMULATORID and BUILD-FOLDER APPNAME) and call CALLBACK when done."
  (let* ((folder build-folder)
         ;; Remove shell quotes if present to get clean path for file operations
         (install-path (if (and folder (string-match "^['\"]\\(.*\\)['\"]$" folder))
                          (match-string 1 folder)
                        folder))
         (app-path (format "%s%s.app" install-path appname))
         (command (list "xcrun" "simctl" "install" simulatorID app-path)))
    (when ios-simulator:debug
      (message "Installing app: %s" app-path)
      (message "App path exists: %s" (file-exists-p app-path)))
    
    ;; Direct process creation without buffer
    (setq ios-simulator--installation-process
          (make-process
           :name "ios-simulator-install"
           :command command
           :noquery t
           :sentinel (lambda (process event)
                       (when ios-simulator:debug
                         (message "Installation process event: %s" event))
                       (when (string= event "finished\n")
                         (if (= 0 (process-exit-status process))
                             (when callback (funcall callback))
                           (message "App installation failed with exit code: %d" 
                                    (process-exit-status process)))))))))


(defun ios-simulator:get-app-name-fast (build-folder)
  "Get app name quickly by looking for .app files in BUILD-FOLDER."
  (when build-folder
    (let* ((folder (if (string-match "^['\"]\\(.*\\)['\"]$" build-folder)
                      (match-string 1 build-folder)
                    build-folder))
           (app-files (directory-files folder nil "\\.app$")))
      (when app-files
        (file-name-sans-extension (car app-files))))))

(defun ios-simulator:kill-buffer ()
  "Kill the ios-simulator buffer."
  (when (get-buffer ios-simulator-buffer-name)
    (kill-buffer ios-simulator-buffer-name)))

(defun ios-simulator:setup-simulator-dwim (id)
  "Setup simulator dwim (as ID)."
  (when ios-simulator:debug
    (message "Setting up simulator with id %s" id))
  (if (not (ios-simulator:is-simulator-app-running))
      (ios-simulator:start-simulator-with-id id)
    (ios-simulator:boot-simulator-with-id id)))

(cl-defun ios-simulator:simulator-name (&key callback)
  "Fetches simulator name. If CALLBACK provided, run asynchronously."
  (if callback
      ;; Asynchronous version
      (if ios-simulator--current-simulator-name
          (funcall callback ios-simulator--current-simulator-name)
        (ios-simulator:simulator-name-from 
         :id current-simulator-id
         :callback (lambda (simulator-name)
                     (if simulator-name
                         (setq ios-simulator--current-simulator-name simulator-name)
                       (setq ios-simulator--current-simulator-name "Simulator (unknown)"))
                     (funcall callback ios-simulator--current-simulator-name))))
    ;; Synchronous version (fallback)
    (unless ios-simulator--current-simulator-name
      (let ((simulator-name (ios-simulator:simulator-name-from :id current-simulator-id)))
        (if simulator-name
            (setq ios-simulator--current-simulator-name simulator-name)
          (setq ios-simulator--current-simulator-name "Simulator (unknown)"))))
    ios-simulator--current-simulator-name))

(defun ios-simulator:boot-simulator-with-id (id)
  "Simulator app is running.  Boot simulator (as ID)."
  (when ios-simulator:debug
    (message "Booting simulator with id %s" id))
  (make-process
   :name "boot-simulator"
   :command (list "sh" "-c" (ios-simulator:boot-command :id id))
   :sentinel nil))

(defun ios-simulator:start-simulator-with-id (id)
  "Launch a specific simulator with (as ID)."
  (when ios-simulator:debug
    (message "Starting simulator with id: %s" id))
  (make-process
   :name "start-simulator"
   :command (list "sh" "-c" (format "open --background -a simulator --args -CurrentDeviceUDID %s" id))
   :sentinel nil))

(cl-defun ios-simulator:boot-command (&key id)
  "Boot simulator (as ID)."
  (format "xcrun simctl boot %s" id))

(cl-defun ios-simulator:is-simulator-app-running (&key callback)
  "Check if simulator is running. If CALLBACK provided, run asynchronously."
  (if callback
      (make-process
       :name "check-simulator"
       :command (list "sh" "-c" "ps ax | grep -v grep | grep Simulator.app")
       :noquery t
       :buffer " *check-simulator-temp*"
       :sentinel (lambda (proc event)
                   (when (string= event "finished\n")
                     (with-current-buffer (process-buffer proc)
                       (let ((output (buffer-string)))
                         (kill-buffer)
                         (funcall callback (not (string= "" output))))))))
    ;; Synchronous fallback
    (let ((output (shell-command-to-string "ps ax | grep -v grep | grep Simulator.app")))
      (not (string= "" output)))))

(cl-defun ios-simulator:simulator-name-from (&key id &key callback)
  "Get simulator name (as ID). If CALLBACK provided, run asynchronously."
  (if callback
      (make-process
       :name "simulator-name"
       :command (list "sh" "-c" (format "xcrun simctl list devices | grep %s | awk -F \"(\" '{ print $1 }'" id))
       :noquery t
       :buffer " *simulator-name-temp*"
       :sentinel (lambda (proc event)
                   (when (string= event "finished\n")
                     (with-current-buffer (process-buffer proc)
                       (let ((result (string-trim (buffer-string))))
                         (kill-buffer)
                         (funcall callback result))))))
    ;; Synchronous fallback
    (string-trim
     (shell-command-to-string (format "xcrun simctl list devices | grep %s | awk -F \"(\" '{ print $1 }'" id)))))

(defun ios-simulator:available-simulators ()
  "List available simulators with iOS version displayed."
  (let* ((devices (ios-simulator:fetch-available-simulators))
         (items (seq-map
                 (lambda (device)
                   (let* ((name (cdr (assoc 'name device)))
                          (ios-version (cdr (assoc 'iosVersion device)))
                          ;; Add iOS version only if name doesn't already contain version info
                          (display-name 
                           (cond
                            ;; If name already contains version info, keep it as is
                            ((string-match-p "([0-9]+\\.[0-9]+)" name) name)
                            ((string-match-p "(iOS [0-9]+\\.[0-9]+)" name) name)
                            ;; If we have ios-version and name doesn't show it, add it
                            ((and ios-version)
                             (format "%s (iOS %s)" name ios-version))
                            ;; Default: use name as is
                            (t name))))
                     (cons display-name
                           (cdr (assoc 'udid device))))) devices)))
    items))

(defun ios-simulator:available-ios-versions ()
  "Get list of available iOS versions."
  (let* ((json (if (fboundp 'call-process-to-json)
                   (call-process-to-json list-simulators-command)
                 (ios-simulator:run-command-and-get-json list-simulators-command)))
         (devices (cdr (assoc 'devices json)))
         (versions '()))
    (dolist (runtime-entry devices)
      (let* ((runtime-key (car runtime-entry))
             (runtime-devices (cdr runtime-entry))
             (runtime-string (if (symbolp runtime-key)
                                 (symbol-name runtime-key)
                               runtime-key))
             (ios-version (when (and runtime-string
                                     (string-match "iOS-\\([0-9]+\\)-\\([0-9]+\\)" runtime-string))
                           (format "%s.%s" 
                                   (match-string 1 runtime-string)
                                   (match-string 2 runtime-string)))))
        ;; Only add version if there are available devices
        (when (and ios-version
                   (seq-some (lambda (device) (cdr (assoc 'isAvailable device))) 
                            runtime-devices))
          (unless (member ios-version versions)
            (push ios-version versions)))))
    (sort versions 'version<)))

(defun ios-simulator:devices-for-ios-version (ios-version)
  "Get available devices for a specific iOS version."
  (let* ((json (if (fboundp 'call-process-to-json)
                   (call-process-to-json list-simulators-command)
                 (ios-simulator:run-command-and-get-json list-simulators-command)))
         (devices (cdr (assoc 'devices json)))
         (matching-devices '()))
    (dolist (runtime-entry devices)
      (let* ((runtime-key (car runtime-entry))
             (runtime-devices (cdr runtime-entry))
             (runtime-string (if (symbolp runtime-key)
                                 (symbol-name runtime-key)
                               runtime-key))
             (runtime-ios-version (when (and runtime-string
                                            (string-match "iOS-\\([0-9]+\\)-\\([0-9]+\\)" runtime-string))
                                   (format "%s.%s" 
                                           (match-string 1 runtime-string)
                                           (match-string 2 runtime-string)))))
        (when (string= runtime-ios-version ios-version)
          (dolist (device (append runtime-devices nil))
            (when (and device
                       (listp device)
                       (cdr (assoc 'isAvailable device)))
              (push (cons (cdr (assoc 'name device))
                         (cdr (assoc 'udid device))) matching-devices))))))
    (nreverse matching-devices)))

(cl-defun ios-simulator:build-language-menu (&key title)
  "Build language menu (as TITLE)."
  (defconst languageList '(
                           ("🇦🇪 Arabic (United Arab Emirates)" "ar-AE")
                           ("🇦🇷 Arabic (Saudi Arabia)" "ar-SA")
                           ("🇦🇺 English (Australia)" "en-AU")
                           ("🇧🇪 Dutch (Belgium)" "nl-BE")
                           ("🇧🇭 Indonesian (Indonesia)" "id-ID")
                           ("🇧🇷 Portuguese (Brazil)" "pt-BR")
                           ("🇨🇳 Chinese (Simplified)" "zh-CN")
                           ("🇩🇪 German (Germany)" "de-DE")
                           ("🇪🇸 Spanish (Spain)" "es-ES")
                           ("🇫🇷 French (France)" "fr-FR")
                           ("🇬🇧 English (UK)" "en-UK")
                           ("🇮🇳 Hindi (India)" "hi-IN")
                           ("🇮🇹 Italian (Italy)" "it-IT")
                           ("🇯🇵 Japanese (Japan)" "ja-JP")
                           ("🇰🇷 Korean (Korea)" "ko-KR")
                           ("🇳🇱 Dutch (Netherlands)" "nl-NL")
                           ("🇳🇴 Norwegian (Bokmål)" "nb-NO")
                           ("🇵🇱 Polish (Poland)" "pl-PL")
                           ("🇷🇺 Russian (Russia)" "ru-RU")
                           ("🇸🇦 Arabic (Saudi Arabia)" "ar-EG")
                           ("🇸🇪 Swedish (Sweden)" "sv-SE")
                           ("🇹🇷 Turkish (Turkey)" "tr-TR")
                           ("🇹🇼 Chinese (Traditional)" "zh-TW")
 ("🇩🇰 Danish (Denmark)" "da-DK")
                           ))
    (progn
    (let* ((choices (seq-map (lambda (item) item) languageList))
           (choice (completing-read title choices)))
      (car (cdr (assoc choice choices))))))

(cl-defun ios-simulator:build-selection-menu (&key title &key list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) item) list))
             (choice (completing-read title choices)))
        (cdr (assoc choice choices))))))

(cl-defun ios-simulator:simulator-identifier (&key callback)
  "Get the booted simulator id or fetch a suitable one. If CALLBACK provided, run asynchronously."
  (if callback
      ;; Asynchronous version
      (if current-simulator-id
          (progn
            (ios-simulator:setup-simulator-dwim current-simulator-id)
            (funcall callback current-simulator-id))
        (let ((device-id (ios-simulator:get-or-choose-simulator)))
          (when ios-simulator:debug
            (message "Selected simulator ID: %s" device-id))
          (funcall callback device-id)))
    ;; Synchronous version (fallback)
    (if current-simulator-id
        (progn
          (ios-simulator:setup-simulator-dwim current-simulator-id)
          current-simulator-id)
      (progn
        (let ((device-id (ios-simulator:get-or-choose-simulator)))
          (when ios-simulator:debug
            (message "Selected simulator ID: %s" device-id))
          device-id)))))

(defun ios-simulator:get-or-choose-simulator ()
  "Get booted simulator or let user choose one.
If target simulators are configured, ensures primary is set correctly.
If exactly one simulator is booted, use it automatically.
If multiple simulators are booted, let user choose which is the main one."
  (if current-simulator-id
      (progn
        (ios-simulator:setup-language)
        (ios-simulator:setup-simulator-dwim current-simulator-id)
        ;; If we have target simulators configured, make sure current is in the list
        (when (and ios-simulator--target-simulators
                   (not (member current-simulator-id ios-simulator--target-simulators)))
          (push current-simulator-id ios-simulator--target-simulators))
        current-simulator-id)
    (let ((booted-simulators (ios-simulator:get-all-booted-simulators)))
      (cond
       ;; No simulators booted - let user choose and boot one
       ((null booted-simulators)
        (ios-simulator:choose-simulator))

       ;; Exactly one simulator booted - use it automatically
       ((= (length booted-simulators) 1)
        (let ((booted-id (cdar booted-simulators))
              (booted-name (caar booted-simulators)))
          (when ios-simulator:debug
            (message "Auto-selecting booted simulator: %s" booted-name))
          (setq current-simulator-id booted-id)
          (ios-simulator:setup-language)
          (ios-simulator:setup-simulator-dwim booted-id)
          (message "Using booted simulator: %s" booted-name)
          booted-id))

       ;; Multiple simulators booted - let user choose the main one
       (t
        (let* ((choice (completing-read
                       (format "Multiple simulators running. Choose main simulator (%d booted): "
                               (length booted-simulators))
                       booted-simulators nil t))
               (chosen-id (cdr (assoc choice booted-simulators))))
          (when ios-simulator:debug
            (message "User selected main simulator: %s (ID: %s)" choice chosen-id))
          (setq current-simulator-id chosen-id)
          (ios-simulator:setup-language)
          (ios-simulator:setup-simulator-dwim chosen-id)
          (message "Using simulator: %s" choice)
          chosen-id))))))

(defun ios-simulator:choose-simulator ()
  "Choose a simulator using two-step process: iOS version first, then device."
  (let* ((ios-versions (ios-simulator:available-ios-versions))
         (chosen-ios-version (completing-read "Choose iOS version: " ios-versions nil t))
         (devices-for-version (ios-simulator:devices-for-ios-version chosen-ios-version))
         (device-choice (if (= (length devices-for-version) 1)
                           ;; If only one device, use it automatically
                           (car devices-for-version)
                         ;; If multiple devices, let user choose
                         (let* ((choices devices-for-version)
                                (choice (completing-read 
                                        (format "Choose device for iOS %s: " chosen-ios-version) 
                                        choices nil t)))
                           (assoc choice choices))))
         (device-id (cdr device-choice)))
    (when ios-simulator:debug
      (message "Selected iOS %s with device: %s (ID: %s)" 
               chosen-ios-version (car device-choice) device-id))
    (setq current-simulator-id device-id)
    (ios-simulator:setup-language)
    (ios-simulator:setup-simulator-dwim device-id)
    device-id))

(defun ios-simulator:get-all-booted-simulators ()
  "Get list of all currently booted simulators.
Returns list of (name . id) pairs."
  (let* ((output (shell-command-to-string "xcrun simctl list devices | grep '(Booted)'"))
         (lines (split-string output "\n" t))
         (simulators '()))
    (dolist (line lines)
      (when (string-match "\\s-*\\(.+?\\)\\s-*(\\([A-F0-9-]+\\))\\s-*(Booted)" line)
        (let ((name (string-trim (match-string 1 line)))
              (id (match-string 2 line)))
          (push (cons name id) simulators))))
    (nreverse simulators)))

(cl-defun ios-simulator:booted-simulator (&key callback)
  "Get booted simulator if any. If CALLBACK provided, run asynchronously."
  (if callback
      (make-process
       :name "booted-simulator"
       :command (list "sh" "-c" ios-simulator:get-booted-simulator-command)
       :noquery t
       :buffer " *booted-simulator-temp*"
       :sentinel (lambda (proc event)
                   (when (string= event "finished\n")
                     (with-current-buffer (process-buffer proc)
                       (let ((device-id (string-trim (buffer-string))))
                         (kill-buffer)
                         (funcall callback (if (string= "" device-id) nil device-id)))))))
    ;; Synchronous fallback
    (let ((device-id (shell-command-to-string ios-simulator:get-booted-simulator-command)))
      (if (not (string= "" device-id))
          (string-trim device-id)
        nil))))

(defun ios-simulator:terminate-current-app ()
  "Terminate the current app running in simulator."
  (interactive)
  (if current-app-identifier
      (ios-simulator:terminate-app-with :appIdentifier current-app-identifier)))

(defun ios-simulator:change-language ()
  "Reset current language for simulator."
  (interactive)
  (setq current-language-selection (ios-simulator:build-language-menu :title "Choose simulator language")))

(defun ios-simulator:setup-language ()
  "Setup language if it isnt set."
  (unless current-language-selection
    (setq current-language-selection (ios-simulator:build-language-menu :title "Choose simulator language"))))

(cl-defun ios-simulator:launch-app (&key appIdentifier &key applicationName &key simulatorName &key simulatorID &key buffer &key terminate-running)
  "Launch app (as APPIDENTIFIER APPLICATIONNAME SIMULATORNAME SIMULATORID) and display output in BUFFER.
If TERMINATE-RUNNING is non-nil, terminate any running instance before launching."
  ;; Run immediately without idle timer
  (ios-simulator:setup-language)
  (mode-line-hud:updateWith
   :message (format "%s|%s"
                    (propertize applicationName 'face 'font-lock-constant-face)
                    (propertize simulatorName 'face 'font-lock-function-name-face))
   :delay 2.0)

  (let ((command (append (list "xcrun" "simctl" "launch" "--console-pty"
                               (or simulatorID "booted")
                               appIdentifier)
                         (when terminate-running (list "--terminate-running-process"))
                         (list "-AppleLanguages" (format "(%s)" current-language-selection)))))
    ;; Prepare the buffer
    (with-current-buffer buffer
      (erase-buffer)
      (setq-local mode-line-format nil)
      (setq-local left-fringe-width 5
                  right-fringe-width 5
                  buffer-face-mode-face 'ios-simulator-background-face
                  window-point-insertion-type t
                  kill-buffer-query-functions nil)
      (buffer-face-mode 1)
      (read-only-mode -1)
      (visual-line-mode 1))

    (display-buffer buffer '(display-buffer-pop-up-window))

    (let ((process (make-process
                       :name "ios-simulator-launch"
                       :command command
                       :buffer buffer
                       :noquery t
                       :filter (lambda (proc string)
                             (when (buffer-live-p (process-buffer proc))
                               (with-current-buffer (process-buffer proc)
                                 (let ((inhibit-read-only t)
                                       (at-end (= (point) (point-max))))
                                   (save-excursion
                                     (goto-char (point-max))
                                     (insert (ios-simulator:remove-control-m string)))
                                   (when at-end
                                     (goto-char (point-max))
                                     (dolist (window (get-buffer-window-list (current-buffer) nil t))
                                       (set-window-point window (point-max))))))))
                       :sentinel (lambda (process event)
                                     (when (and (string= event "finished\n")
                                        (buffer-live-p (process-buffer process)))
                               (with-current-buffer (process-buffer process)
                                 (let ((inhibit-read-only t))
                                   (goto-char (point-max))
                                   (insert "\n\nProcess finished\n"))
                                 (read-only-mode 1)))))))
      ;; Don't ask when killing the process
      (set-process-query-on-exit-flag process nil))))

(defun ios-simulator:run-command-and-get-json (command)
  "Run a shell COMMAND and return the JSON output as a string."
  (let* ((json-output (shell-command-to-string command))
         (json-data (json-read-from-string json-output)))
    json-data))

(cl-defun ios-simulator:terminate-app-with (&key appIdentifier)
  "Terminate runnings apps (as APPIDENTIFIER)."
  (setq current-app-identifier appIdentifier)
  ;; Run in an idle timer to avoid blocking
  (run-with-idle-timer 0 nil
    (lambda ()
      (ios-simulator:terminate-app :simulatorID current-simulator-id :appIdentifier appIdentifier))))

(cl-defun ios-simulator:terminate-app (&key simulatorID &key appIdentifier)
  "Terminate app (as APPIDENTIFIER as SIMULATORID)."
  (when ios-simulator:debug
    (message "%s %s" simulatorID appIdentifier))
  (inhibit-sentinel-messages #'call-process-shell-command
                             (string-trim
                              (concat
                               (if simulatorID
                                   (format "xcrun simctl terminate %s %s" simulatorID appIdentifier)
                                 (format "xcrun simctl terminate booted %s" appIdentifier))))))

(defun ios-simulator:send-notification ()
  "Send a notification to the current simulator and app."
  (interactive)
  (unless current-simulator-id
    (error "No simulator selected"))
  (unless current-app-identifier
    (error "No app selected"))

  (let* ((text (read-string "Notification text: "))
         (payload (format "{\"aps\":{\"alert\":\"%s\",\"sound\":\"default\"}}" text))
         (temp-file (make-temp-file "ios-notification" nil ".json" payload)))
    (when (and text (not (string-empty-p text)))
      (let ((command (format "xcrun simctl push %s %s %s"
                            current-simulator-id
                            current-app-identifier
                            temp-file)))
        (async-shell-command command)
        (run-at-time 2 nil (lambda () (delete-file temp-file)))
      (message "Notification sent to %s" current-app-name)))))

(defun ios-simulator:appcontainer ()
  "Get the app container of the current app (as SIMULATORID, APPIDENTIFIER)."
  (interactive)
  (if-let* ((identifier current-app-identifier)
           (id current-simulator-id)
           (command (shell-command-to-string (format "xcrun simctl get_app_container %s %s data" id identifier))))
      (async-shell-command (concat "open " command))))

(defun ios-simulator:fetch-available-simulators ()
  "List available simulators, using cached results if valid."
  (if (fboundp 'swift-cache-with)
      ;; Use unified cache system if available
      (swift-cache-with "ios-simulator:available-devices" 300  ; 5 minute cache
        (when ios-simulator:debug
          (message "Refreshing simulator device cache..."))
        (let* ((json (if (fboundp 'call-process-to-json)
                         (call-process-to-json list-simulators-command)
                       (ios-simulator:run-command-and-get-json list-simulators-command)))
               (devices (cdr (assoc 'devices json)))
               (flattened-devices '()))
          ;; Process each runtime and its devices
          (dolist (runtime-entry devices)
            (let* ((runtime-key (car runtime-entry))
                   (runtime-devices (cdr runtime-entry))
                   ;; Extract iOS version from runtime key
                   (runtime-string (if (symbolp runtime-key)
                                       (symbol-name runtime-key)
                                     runtime-key))
                   (ios-version (when (and runtime-string
                                           (string-match "iOS-\\([0-9]+\\)-\\([0-9]+\\)" runtime-string))
                                 (format "%s.%s" 
                                         (match-string 1 runtime-string)
                                         (match-string 2 runtime-string)))))
              ;; Process each device in this runtime
              (dolist (device (append runtime-devices nil))
                (when (and device
                           (listp device)
                           (cdr (assoc 'isAvailable device)))
                  ;; Create a new device entry with iOS version
                  (let ((device-with-version (copy-alist device)))
                    (when ios-version
                      (setcdr device-with-version
                              (cons (cons 'iosVersion ios-version)
                                    (cdr device-with-version))))
                    (push device-with-version flattened-devices))))))
          (nreverse flattened-devices)))
    ;; Fallback to legacy caching if swift-cache not available
    (let ((now (float-time)))
      (when (or (null ios-simulator--cached-devices)
                (null ios-simulator--cache-timestamp)
                (> (- now ios-simulator--cache-timestamp) ios-simulator--cache-ttl))
        (when ios-simulator:debug
          (message "Refreshing simulator device cache..."))
        (let* ((json (if (fboundp 'call-process-to-json)
                         (call-process-to-json list-simulators-command)
                       (ios-simulator:run-command-and-get-json list-simulators-command)))
               (devices (cdr (assoc 'devices json)))
               (flattened-devices '()))
          ;; Process each runtime and its devices
          (dolist (runtime-entry devices)
            (let* ((runtime-key (car runtime-entry))
                   (runtime-devices (cdr runtime-entry))
                   ;; Extract iOS version from runtime key
                   (runtime-string (if (symbolp runtime-key)
                                       (symbol-name runtime-key)
                                     runtime-key))
                   (ios-version (when (and runtime-string
                                           (string-match "iOS-\\([0-9]+\\)-\\([0-9]+\\)" runtime-string))
                                 (format "%s.%s" 
                                         (match-string 1 runtime-string)
                                         (match-string 2 runtime-string)))))
              ;; Process each device in this runtime
              (dolist (device (append runtime-devices nil))
                (when (and device
                           (listp device)
                           (cdr (assoc 'isAvailable device)))
                  ;; Create a new device entry with iOS version
                  (let ((device-with-version (copy-alist device)))
                    (when ios-version
                      (setcdr device-with-version
                              (cons (cons 'iosVersion ios-version)
                                    (cdr device-with-version))))
                    (push device-with-version flattened-devices))))))
          (setq ios-simulator--cached-devices (nreverse flattened-devices)
                ios-simulator--cache-timestamp now)))
      (when ios-simulator:debug
        (message "Using %s cached simulators" (length ios-simulator--cached-devices)))
      ios-simulator--cached-devices)))

(defun ios-simulator:invalidate-cache ()
  "Force refresh of simulator device cache."
  (interactive)
  (if (fboundp 'swift-cache-invalidate)
      (swift-cache-invalidate "ios-simulator:available-devices")
    ;; Fallback to legacy cache invalidation
    (setq ios-simulator--cached-devices nil
          ios-simulator--cache-timestamp nil))
  (message "Simulator cache invalidated"))

(defun ios-simulator:toggle-buffer ()
  "Toggle visibility of the iOS Simulator buffer window."
  (interactive)
  (if-let* ((buffer (get-buffer ios-simulator-buffer-name)))
      (if (get-buffer-window buffer)
          (delete-window (get-buffer-window buffer))
        (display-buffer buffer))
    (message "Buffer %s does not exist" ios-simulator-buffer-name)))

(defun ios-simulator:remove-control-m (string)
  "Remove ^M characters from STRING."
  (replace-regexp-in-string "\r" "" string))

(defun ios-simulator:get-or-create-buffer-for-simulator (simulator-id simulator-name)
  "Get existing or create buffer for SIMULATOR-ID with SIMULATOR-NAME.
Reuses existing buffer if already created for this simulator."
  (let* ((buffer-name (format "*iOS Simulator - %s*" simulator-name))
         (existing-buffer (get-buffer buffer-name)))
    (puthash simulator-id buffer-name ios-simulator--simulator-buffers)
    (or existing-buffer (get-buffer-create buffer-name))))

(cl-defun ios-simulator:install-and-run-on-additional-simulator (&key rootfolder &key build-folder &key appIdentifier &key terminate-first)
  "Install and run app on an additional simulator.
ROOTFOLDER: Project root directory
BUILD-FOLDER: Path to build artifacts
APPIDENTIFIER: Bundle identifier
TERMINATE-FIRST: Whether to terminate existing app instance"
  (interactive)
  (when ios-simulator:debug
    (message "Installing on additional simulator - root: %s build:%s" rootfolder build-folder))

  ;; Let user choose a different simulator
  (let* ((additional-simulator-id (ios-simulator:choose-simulator))
         (simulator-name (ios-simulator:simulator-name-from :id additional-simulator-id))
         (buffer (ios-simulator:get-or-create-buffer-for-simulator additional-simulator-id simulator-name))
         (applicationName (ios-simulator:get-app-name-fast build-folder)))

    (when ios-simulator:debug
      (message "Installing app: %s for additional simulator: %s (ID: %s)"
               (or applicationName "Unknown")
               (or simulator-name "Unknown")
               (or additional-simulator-id "Unknown")))

    ;; Store simulator info
    (puthash additional-simulator-id
             (list :name simulator-name
                   :app-identifier appIdentifier
                   :app-name applicationName
                   :buffer buffer)
             ios-simulator--active-simulators)

    ;; Setup and boot the additional simulator
    (ios-simulator:setup-simulator-dwim additional-simulator-id)

    ;; Install and launch
    (if terminate-first
        (make-process
         :name "terminate-app-additional"
         :command (list "xcrun" "simctl" "terminate" additional-simulator-id appIdentifier)
         :noquery t
         :sentinel (lambda (proc event)
                     (ios-simulator:install-app
                      :simulatorID additional-simulator-id
                      :build-folder build-folder
                      :appname applicationName
                      :callback (lambda ()
                                  (when ios-simulator:debug
                                    (message "Additional simulator app installation completed"))
                                  (ios-simulator:launch-app
                                   :appIdentifier appIdentifier
                                   :applicationName applicationName
                                   :simulatorName simulator-name
                                   :simulatorID additional-simulator-id
                                   :buffer buffer
                                   :terminate-running terminate-first)))))
      (ios-simulator:install-app
       :simulatorID additional-simulator-id
       :build-folder build-folder
       :appname applicationName
       :callback (lambda ()
                   (when ios-simulator:debug
                     (message "Additional simulator app installation completed"))
                   (ios-simulator:launch-app
                    :appIdentifier appIdentifier
                    :applicationName applicationName
                    :simulatorName simulator-name
                    :simulatorID additional-simulator-id
                    :buffer buffer
                    :terminate-running terminate-first))))

    (message "Installing app on additional simulator: %s" simulator-name)
    additional-simulator-id))

;;;###autoload
(defun ios-simulator:add-target-simulator ()
  "Add a simulator to the target list. App will launch on all target simulators when built."
  (interactive)
  (let* ((additional-simulator-id (ios-simulator:choose-simulator))
         (simulator-name (ios-simulator:simulator-name-from :id additional-simulator-id)))

    ;; Initialize list if needed and ensure primary simulator is included
    (unless ios-simulator--target-simulators
      (setq ios-simulator--target-simulators (list (or current-simulator-id
                                                       (ios-simulator:simulator-identifier)))))

    ;; Add new simulator if not already in list
    (unless (member additional-simulator-id ios-simulator--target-simulators)
      (push additional-simulator-id ios-simulator--target-simulators)
      (message "Added simulator: %s. App will now launch on %d simulator(s)"
               simulator-name
               (length ios-simulator--target-simulators)))

    ;; Store info for management
    (puthash additional-simulator-id
             (list :name simulator-name
                   :app-identifier current-app-identifier
                   :app-name current-app-name)
             ios-simulator--active-simulators)))

;;;###autoload
(defun ios-simulator:remove-target-simulator ()
  "Remove a simulator from the target list."
  (interactive)
  (if (or (not ios-simulator--target-simulators)
          (<= (length ios-simulator--target-simulators) 1))
      (message "Cannot remove - need at least one simulator")
    (let* ((choices (mapcar (lambda (sim-id)
                             (cons (ios-simulator:simulator-name-from :id sim-id) sim-id))
                           ios-simulator--target-simulators))
           (selected-name (completing-read "Remove simulator: " choices nil t))
           (selected-id (cdr (assoc selected-name choices))))

      (setq ios-simulator--target-simulators
            (remove selected-id ios-simulator--target-simulators))
      (remhash selected-id ios-simulator--active-simulators)
      (remhash selected-id ios-simulator--simulator-buffers)

      (message "Removed simulator: %s. App will now launch on %d simulator(s)"
               selected-name
               (length ios-simulator--target-simulators)))))

;;;###autoload
(defun ios-simulator:list-target-simulators ()
  "List all target simulators where app will launch."
  (interactive)
  (if (not ios-simulator--target-simulators)
      (message "No target simulators configured. App will launch on default simulator only.")
    (with-current-buffer (get-buffer-create "*Target Simulators*")
      (erase-buffer)
      (insert (format "App will launch on %d simulator(s):\n\n"
                     (length ios-simulator--target-simulators)))
      (dolist (sim-id ios-simulator--target-simulators)
        (let ((name (ios-simulator:simulator-name-from :id sim-id)))
          (insert (format "- %s\n  ID: %s\n\n" name sim-id))))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun ios-simulator:clear-target-simulators ()
  "Clear all target simulators and reset to single simulator mode."
  (interactive)
  (setq ios-simulator--target-simulators nil)
  (clrhash ios-simulator--active-simulators)
  (clrhash ios-simulator--simulator-buffers)
  (message "Cleared all target simulators. App will launch on default simulator only."))

;;;###autoload
(defun ios-simulator:run-on-additional-simulator ()
  "Run current app on an additional simulator while keeping the current one running."
  (interactive)
  (unless current-root-folder-simulator
    (error "No app currently installed. Please run the app first."))
  (unless current-build-folder
    (error "No build folder configured"))
  (unless current-app-identifier
    (error "No app identifier configured"))

  (ios-simulator:install-and-run-on-additional-simulator
   :rootfolder current-root-folder-simulator
   :build-folder current-build-folder
   :appIdentifier current-app-identifier
   :terminate-first nil))

;;;###autoload
(defun ios-simulator:list-active-simulators ()
  "List all active simulators with running apps."
  (interactive)
  (if (hash-table-empty-p ios-simulator--active-simulators)
      (message "No active simulators")
    (with-current-buffer (get-buffer-create "*Active Simulators*")
      (erase-buffer)
      (insert "Active Simulators:\n\n")
      (maphash (lambda (sim-id info)
                 (insert (format "Simulator: %s\n" (plist-get info :name)))
                 (insert (format "  ID: %s\n" sim-id))
                 (insert (format "  App: %s (%s)\n"
                               (plist-get info :app-name)
                               (plist-get info :app-identifier)))
                 (insert (format "  Buffer: %s\n\n"
                               (buffer-name (plist-get info :buffer)))))
               ios-simulator--active-simulators)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun ios-simulator:terminate-app-on-simulator (simulator-id)
  "Terminate app on a specific SIMULATOR-ID."
  (interactive
   (list (completing-read "Select simulator: "
                         (let (sims)
                           (maphash (lambda (id info)
                                      (push (cons (plist-get info :name) id) sims))
                                    ios-simulator--active-simulators)
                           sims)
                         nil t)))
  (when-let* ((info (gethash simulator-id ios-simulator--active-simulators))
              (app-id (plist-get info :app-identifier)))
    (call-process-shell-command
     (format "xcrun simctl terminate %s %s" simulator-id app-id))
    (message "Terminated app on simulator: %s" (plist-get info :name))))

;;;###autoload
(defun ios-simulator:shutdown-simulator (simulator-id)
  "Shutdown a specific SIMULATOR-ID and remove from active list."
  (interactive
   (list (completing-read "Select simulator to shutdown: "
                         (let (sims)
                           (maphash (lambda (id info)
                                      (push (cons (plist-get info :name) id) sims))
                                    ios-simulator--active-simulators)
                           sims)
                         nil t)))
  (when-let* ((info (gethash simulator-id ios-simulator--active-simulators)))
    (call-process-shell-command (format "xcrun simctl shutdown %s" simulator-id))
    (remhash simulator-id ios-simulator--active-simulators)
    (remhash simulator-id ios-simulator--simulator-buffers)
    (message "Shutdown simulator: %s" (plist-get info :name))))

;;;###autoload
(defun ios-simulator:test-two-step-selection ()
  "Test the new two-step selection process."
  (interactive)
  (let ((ios-simulator:debug t))
    (message "Testing two-step simulator selection:")
    (message "Available iOS versions: %s" (string-join (ios-simulator:available-ios-versions) ", "))
    (message "Example: Devices for iOS 17.2:")
    (let ((devices (ios-simulator:devices-for-ios-version "17.2")))
      (dolist (device devices)
        (message "  - %s" (car device))))))

(provide 'ios-simulator)

;;; ios-simulator.el ends here

