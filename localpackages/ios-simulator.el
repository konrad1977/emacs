;;; Simulator --- A small package for viewing iOS simulator logs -*- lexical-binding: t -*-
;;; Commentary: This package provides some support for iOS Simulator
;;; Code:

(require 'nerd-icons)

(with-eval-after-load 'periphery-helper
 (require 'periphery-helper))

(with-eval-after-load 'mode-line-hud
  (require 'mode-line-hud))

(with-eval-after-load 'xcode-additions
  (require 'xcode-additions))

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
  '((t (:inherit default :height 150)))
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
(defvar-local use-rosetta nil)

(defvar ios-simulator-ready-hook nil
  "Hook run when the simulator is ready for app installation.")

(defvar-local current-root-folder-simulator nil)

(defvar-local current-language-selection ios-simulator-default-language
  "Current language selection for the simulator.")

(defvar secondary-simulator-id nil)

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
        current-app-name nil)
  (ios-simulator:kill-buffer)
  (ios-simulator:shut-down-all))

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

(cl-defun ios-simulator:install-and-run-app (&key rootfolder &key build-folder &key simulatorId &key appIdentifier)
  "Install app in simulator with ROOTFOLDER BUILD-FOLDER SIMULATORID, APPIDENTIFIER BUFFER."
  (when ios-simulator:debug
    (message "Install-and-run root: %s build:%s" rootfolder build-folder))

  (ios-device:kill-buffer)
  (let* ((default-directory rootfolder)
         (simulator-id (or simulatorId (ios-simulator:simulator-identifier)))
         (buffer (get-buffer-create ios-simulator-buffer-name))
         (applicationName (xcode-additions:product-name))
         (simulatorName (ios-simulator:simulator-name-from :id simulator-id)))

  (when ios-simulator:debug
    (message "Installing app: %s for simulator: %s (ID: %s)"
             (or applicationName "Unknown")
             (or simulatorName "Unknown")
             (or simulator-id "Unknown")))

    (setq current-simulator-id simulator-id
          current-app-identifier appIdentifier
          current-root-folder-simulator rootfolder
          current-app-name applicationName)

    (ios-simulator:terminate-app-with :appIdentifier appIdentifier)

  (condition-case err
      (ios-simulator:install-app
       :simulatorID simulator-id
       :build-folder build-folder
       :appname applicationName
       :callback (lambda ()
                   (when ios-simulator:debug
                     (message "App installation completed, launching app"))
                   (ios-simulator:launch-app
                    :appIdentifier appIdentifier
                    :applicationName applicationName
                    :simulatorName simulatorName
                    :simulatorID simulator-id
                    :buffer buffer)))
    (error
     (message "Error during app installation: %s" (error-message-string err))))))

(cl-defun ios-simulator:install-app (&key simulatorID &key build-folder &key appname &key callback)
  "Install app (as SIMULATORID and BUILD-FOLDER APPNAME) and call CALLBACK when done."
  (let* ((folder build-folder)
         (install-path folder)
         (app-path (format "%s%s.app" install-path appname))
         (command (format "xcrun simctl install %s \"%s\"" simulatorID app-path)))
    (when ios-simulator:debug
      (message "Installing app with command: %s" command)
      (message "App path exists: %s" (file-exists-p app-path)))
    (setq ios-simulator--installation-process
          (make-process
           :name "ios-simulator-install"
           :command (list "sh" "-c" command)
           :buffer (get-buffer-create "*iOS Simulator Install*")
           :sentinel (lambda (process event)
                       (when ios-simulator:debug
                         (message "Installation process event: %s" event))
                       (cond
                        ((string= event "finished\n")
                         (if (= 0 (process-exit-status process))
                             (progn
                               (when callback (funcall callback)))
                           (message "App installation failed with exit code: %d" (process-exit-status process))
                           (with-current-buffer (process-buffer process)
                             (message "Installation output: %s" (buffer-string)))))
                        ((string-prefix-p "exited abnormally" event)
                         (message "Installation process crashed: %s" event)
                         (with-current-buffer (process-buffer process)
                           (message "Installation output: %s" (buffer-string))))))))))


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
    (ios-simulator:boot-simuator-with-id id)))

(defun ios-simulator:simulator-name ()
  "Fetches simulator name."
  (unless ios-simulator--current-simulator-name
    (let ((simulator-name (ios-simulator:simulator-name-from :id current-simulator-id)))
      (if simulator-name
          (setq ios-simulator--current-simulator-name simulator-name)
        (setq ios-simulator--current-simulator-name "Simulator (unknown)"))))
  ios-simulator--current-simulator-name)

(defun ios-simulator:boot-simuator-with-id (id)
  "Simulator app is running.  Boot simulator (as ID)."
  (when ios-simulator:debug
    (message "Booting simulator with id %s" id))
  (make-process
   :name "boot-simulator"
   :command (list "sh" "-c" (ios-simulator:boot-command :id id :rosetta use-rosetta))
   :sentinel nil))

(defun ios-simulator:start-simulator-with-id (id)
  "Launch a specific simulator with (as ID)."
  (when ios-simulator:debug
    (message "Starting simulator with id: %s" id))
  (make-process
   :name "start-simulator"
   :command (list "sh" "-c" (format "open --background -a simulator --args -CurrentDeviceUDID %s" id))
   :sentinel nil))

(cl-defun ios-simulator:boot-command (&key id &key rosetta)
  "Boot simulator with or without support for x86 (as ID and ROSETTA)."
  (if rosetta
      (format "xcrun simctl boot %s --arch=x86_64" id)
      (format "xcrun simctl boot %s " id)))

(defun ios-simulator:is-simulator-app-running ()
  "Check if simulator is running."
  (let ((output (shell-command-to-string "ps ax | grep -v grep | grep Simulator.app")))
    (not (string= "" output))))

(cl-defun ios-simulator:simulator-name-from (&key id)
  "Get simulator name (as ID)."
  (string-trim
   (shell-command-to-string (format "xcrun simctl list devices | grep %s | awk -F \"(\" '{ print $1 }'" id))))

(defun ios-simulator:available-simulators ()
  "List available simulators."
  (let* ((devices (ios-simulator:fetch-available-simulators))
         (items (seq-map
                 (lambda (device)
                   (cons (cdr (assoc 'name device))
                         (cdr (assoc 'udid device)))) devices)))
    items))

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

(defun ios-simulator:simulator-identifier ()
  "Get the booted simulator id or fetch a suitable one."
  (if current-simulator-id
      (progn
        (ios-simulator:setup-simulator-dwim current-simulator-id)
        current-simulator-id)
    (progn
      (let ((device-id (ios-simulator:get-or-choose-simulator)))
        (when ios-simulator:debug
          (message "Selected simulator ID: %s" device-id))
        device-id))))

(defun ios-simulator:get-or-choose-simulator ()
  "Get booted simulator or let user choose one."
  (let ((booted-id (ios-simulator:booted-simulator)))
    (if booted-id
        (progn
          (setq current-simulator-id booted-id)
          (ios-simulator:setup-language)
          (ios-simulator:setup-simulator-dwim booted-id)
          booted-id)
      (ios-simulator:choose-simulator))))

(defun ios-simulator:choose-simulator ()
  "Choose a simulator."
  (let* ((available-simulators (ios-simulator:fetch-available-simulators))
         (choices (mapcar (lambda (device)
                            (cons (cdr (assoc 'name device))
                                  (cdr (assoc 'udid device))))
                          available-simulators))
         (choice (completing-read "Choose a simulator:" choices nil t))
         (device-id (cdr (assoc choice choices))))
    (setq current-simulator-id device-id)
    (ios-simulator:setup-language)
    (ios-simulator:setup-simulator-dwim device-id)
    device-id))

(defun ios-simulator:booted-simulator ()
  "Get booted simulator if any."
  (let ((device-id (shell-command-to-string ios-simulator:get-booted-simulator-command)))

(defcustom ios-simulator-default-language "sv-SE"
  "Default language for the simulator."
  :type 'string
  :group 'ios-simulator)

(defcustom ios-simulator-default-language "sv-SE"
  "Default language for the simulator."
  :type 'string
  :group 'ios-simulator)
    (if (not (string= "" device-id))
        (string-trim device-id)
      nil)))

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

(cl-defun ios-simulator:launch-app (&key appIdentifier &key applicationName &key simulatorName &key simulatorID &key buffer)
  "Launch app (as APPIDENTIFIER APPLICATIONNAME SIMULATORNAME SIMULATORID) and display output in BUFFER."
  (ios-simulator:setup-language)
  (mode-line-hud:updateWith
   :message (format "%s %s|%s"
                    (nerd-icons-mdicon "nf-md-progress_check" :v-adjust 0.0 :face 'success)
                    (propertize applicationName 'face 'font-lock-constant-face)
                    (propertize simulatorName 'face 'font-lock-function-name-face))
   :delay 2.0)

  (let ((command (format "xcrun simctl launch --console-pty %s %s --terminate-running-process -AppleLanguages \"(%s)\""
                         (or simulatorID "booted")
                         appIdentifier
                         current-language-selection)))
    (with-current-buffer buffer
      (erase-buffer)
      (setq-local mode-line-format nil)
      (setq-local left-fringe-width 0
                  right-fringe-width 0
                  buffer-face-mode-face 'ios-simulator-background-face
                  window-point-insertion-type t
                  kill-buffer-query-functions nil) ; Lägg till denna rad
      (buffer-face-mode 1)
      (read-only-mode -1)
      (visual-line-mode 1))

    (display-buffer buffer '(display-buffer-pop-up-window))

    (let ((process (make-process
                   :name "ios-simulator-launch"
                   :command (list "sh" "-c" command)
                   :buffer buffer
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
      ;; Lägg till process-property för att hantera stängning
      (set-process-query-on-exit-flag process nil))))

(cl-defun ios-simulator:launch-wait-for-debugger (&key identifier)
  "Launch the current configured simulator (as IDENTIFIER) and wait for debugger."
  (mode-line-hud:update :message "Debuggin on Simulator")
  (setq current-app-identifier identifier)
  (setq command
        (format "xcrun simctl launch -w --terminate-running-process %s %s -AppleLanguages \"\(%s\)\""
                (ios-simulator:simulator-identifier)
                identifier
                current-language-selection))
  ;; (async-start-command :command command :callback callback))
  (inhibit-sentinel-messages #'call-process-shell-command command))

(defun ios-simulator:run-command-and-get-json (command)
  "Run a shell COMMAND and return the JSON output as a string."
  (let* ((json-output (shell-command-to-string command))
         (json-data (json-read-from-string json-output)))
    json-data))

(cl-defun ios-simulator:terminate-app-with (&key appIdentifier)
  "Terminate runnings apps (as APPIDENTIFIER)."
  (setq current-app-identifier appIdentifier)
  (ios-simulator:terminate-app :simulatorID current-simulator-id :appIdentifier appIdentifier)
  (ios-simulator:terminate-app :simulatorID secondary-simulator-id :appIdentifier appIdentifier))

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

(defun ios-simulator:appcontainer ()
  "Get the app container of the current app (as SIMULATORID, APPIDENTIFIER)."
  (interactive)
  (if-let* ((identifier current-app-identifier)
           (id current-simulator-id)
           (command (shell-command-to-string (format "xcrun simctl get_app_container %s %s data" id identifier))))
      (async-shell-command (concat "open " command))))

(defun ios-simulator:fetch-available-simulators ()
  "List available simulators."
  (let* ((json (call-process-to-json list-simulators-command))
         (devices (cdr (assoc 'devices json)))
         (flattened (apply 'seq-concatenate 'list (seq-map 'cdr devices)))
         (available-devices
          (seq-filter (lambda (device) (cdr (assoc 'isAvailable device))) flattened))
         ) available-devices))

(defun ios-simulator:remove-control-m (string)
  "Remove ^M characters from STRING."
  (replace-regexp-in-string "\r" "" string))

(provide 'ios-simulator)
;;; ios-simulator.el ends here

