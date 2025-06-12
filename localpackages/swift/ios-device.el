;;; ios-device.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Package for handling an iOS device.

;;; Code:

(require 'periphery-helper)
(require 'cl-lib)

(defgroup ios-device nil
  "Customization group for ios-device package."
  :group 'tools)

(defcustom ios-device:buffer-name "*ios-device-"
  "Base name of the buffer used for iOS device operations."
  :type 'string
  :group 'ios-device)

(defcustom ios-device:debug t
  "Enable debug output for ios-device operations."
  :type 'boolean
  :group 'ios-device)

(defvar ios-device-current-buffer-name nil)
(defvar ios-device-current-device-id nil)
(defvar ios-device-current-install-command nil)
(defvar ios-device-current-app-identifier nil)
(defvar ios-device-current-buffer nil)

(defun ios-device:format-id (id)
  "Format device id (as ID)."
  (if id
      (if (not
           (string-match-p (regexp-quote "-") id))
          (concat (substring id 0 8) "-" (substring id 6))
        id)))

(cl-defun ios-device:install-app (&key buildfolder appIdentifier)
  "Install app on device (BUILDFOLDER APPIDENTIFIER)."
  (let* ((default-directory buildfolder)
         (appname (ios-device:app-name-from :buildfolder buildfolder))
         (buffer-name (concat ios-device:buffer-name appname "*"))
         (buffer (get-buffer-create buffer-name))
         (device-id (ios-device:identifier))
         (command (ios-device:install-cmd :identifier device-id :appname appname)))

    (when ios-device:debug
      (message "install-app: %s" command)
      (message "build-folder: %s" buildfolder)
      (message "app-name: %s" appname))

    (with-current-buffer buffer
      (erase-buffer)
      (setq-local mode-line-format nil)
      (setq-local left-fringe-width 0)
      (setq-local right-fringe-width 0)
      (setq-local ios-device-current-device-id device-id)
      (setq-local ios-device-current-app-identifier appIdentifier)
      ;; (add-hook 'kill-buffer-hook #'ios-device:cleanup nil t)
      )

    (async-start-command-to-string
     :command command
     :callback (lambda (output)
                 (let ((run-command (ios-device:run-cmd :identifier device-id :appIdentifier appIdentifier)))
                   (with-current-buffer buffer
                     (erase-buffer)
                     (insert output)
                     (insert "\n\nRunning app...\n\n")
                     (inhibit-sentinel-messages #'async-shell-command
                                                run-command
                                                buffer)))))

    (pop-to-buffer buffer)))

(defun ios-device:fetch-device-id ()
  "Fetch the identifier of a connected iPhone device using awk.
Returns the UUID identifier string if found, nil otherwise."
  (let ((result (string-trim
                 (shell-command-to-string
                  "xcrun devicectl list devices | awk '/iPhone/ && /connected/ {for(i=1;i<=NF;i++) if($i ~ /^[[:xdigit:]]{8}-([[:xdigit:]]{4}-){3}[[:xdigit:]]{12}$/) {print $i; exit}}'"))))
    (if (string= result "") nil result)))

(defun ios-device:identifier ()
  "Get iOS device identifier."
  (if ios-device-current-device-id
      ios-device-current-device-id
    (setq ios-device-current-device-id (ios-device:fetch-device-id))
    ios-device-current-device-id))

(defun ios-device:connected-device-id ()
  "Get the id of the connected device."
  (let ((result
         (string-trim
          (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
    (if (string= result "") nil result)))

(defun ios-device:kill-buffer ()
  "Kill the ios-device buffer."
  (when (and ios-device-current-buffer-name (get-buffer ios-device-current-buffer-name))
    (kill-buffer ios-device-current-buffer-name)))

(cl-defun ios-device:install-cmd (&key identifier appname)
  "Install app on device (IDENTIFIER APPNAME)."
  (format "xcrun devicectl device install app --device %s '%s.app'"
          identifier
          appname))

(cl-defun ios-device:run-cmd (&key identifier appIdentifier)
  "Generate run command for device (IDENTIFIER APPIDENTIFIER)."
  (when ios-device:debug
    (message "xcrun devicectl device process launch --terminate-existing --device %s %s" identifier appIdentifier))
  (format "sh -c '\
    xcrun devicectl device process launch --terminate-existing --console --device %s %s & \
    wait'"
          identifier
          appIdentifier
          identifier
          appIdentifier
          identifier
          (file-name-base appIdentifier)))

(defun ios-device:reset()
  "Reset the iOS device."
  (ios-device:kill-buffer)
  (setq ios-device-current-buffer-name nil)
  (setq ios-device-current-device-id nil)
  (setq ios-device-current-install-command nil)
  (setq ios-device-current-app-identifier nil)
  (setq ios-device-current-buffer nil))

(defun ios-device:cleanup ()
  "Cleanup function to terminate the app when the buffer is closed."
  (when (and ios-device-current-device-id ios-device-current-app-identifier)
    (message "Cleaning up iOS device session...")
    (let ((cleanup-command
           (format "xcrun devicectl device process terminate --device %s %s"
                   ios-device-current-device-id
                   ios-device-current-app-identifier)))
      (message "Executing cleanup command: %s" cleanup-command)
      (shell-command cleanup-command)
      (message "Cleanup completed for device %s and app %s"
               ios-device-current-device-id
               ios-device-current-app-identifier))
    (setq ios-device-current-device-id nil)
    (setq ios-device-current-app-identifier nil)))

(cl-defun ios-device:install-app-async (&key buildfolder &key appIdentifier)
  "Install app on device (PROJECT-ROOT BUILDFOLDER APPNAME)."
  (ios-device:kill-buffer)
  (let* ((default-directory buildfolder)
         (identifier (ios-device:identifier))
         (buffer (get-buffer-create (concat ios-device:buffer-name appname "*"))))
    (setq ios-device-current-buffer-name buffer)
    (async-shell-command command buffer)))

(cl-defun ios-device:app-name-from (&key buildfolder)
  "Get compiled app name from (BUILDFOLDER)."
  (if (file-exists-p buildfolder)
      (let ((binary-name (directory-files buildfolder nil "\\.app$")))
        (file-name-sans-extension (car binary-name)))
    nil))

(provide 'ios-device)
;;; ios-device.el ends here
