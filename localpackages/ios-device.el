;;; ios-device.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Package for handling an iOS device.

;;; Code:

(require 'periphery-helper)

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
(defvar ios-device-current-device-identifier nil)
(defvar ios-device-current-app-identifier nil)
(defvar ios-device-current-buffer nil)

(defun ios-device:format-id (id)
  "Format device id (as ID)."
  (if id
      (if (not
           (string-match-p (regexp-quote "-") id))
          (concat (substring id 0 8) "-" (substring id 6))
        id)))

(defun ios-device:id ()
  "Get the id of the connected device."
  (let ((device-id
         (string-trim
          (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
    (if (= (length device-id) 0)
        (setq ios-device-current-device-id nil)
      (setq ios-device-current-device-id (ios-device:format-id device-id))))
  ios-device-current-device-id)

(cl-defun ios-device:install-app (&key buildfolder &key appIdentifier)
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
      (add-hook 'kill-buffer-hook #'ios-device:cleanup nil t))

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
    "Fetch the device id."
  (string-trim
   (shell-command-to-string
    "xcrun devicectl list devices | awk 'NR>3 && /iPhone/ { match($0, /[[:xdigit:]]{8}-([[:xdigit:]]{4}-){3}[[:xdigit:]]{12}/); print substr($0, RSTART, RLENGTH) }'")))

(defun ios-device:identifier ()
  "Get iOS device identifier."
  (if ios-device-current-device-identifier
      ios-device-current-device-identifier
    (setq ios-device-current-device-identifier (ios-device:fetch-device-id))
    ios-device-current-device-identifier))

(defun ios-device:kill-buffer ()
  "Kill the ios-device buffer."
  (when (and ios-device-current-buffer-name (get-buffer ios-device-current-buffer-name))
    (kill-buffer ios-device-current-buffer-name)))

(cl-defun ios-device:install-cmd (&key identifier &key appname)
  "Install app on device (IDENTIFIER APPNAME)."
  (format "xcrun devicectl device install app --device %s '%s.app'"
          identifier
          appname))

(cl-defun ios-device:run-cmd (&key identifier appIdentifier)
  "Generate run command for device IDENTIFIER and APP-IDENTIFIER."
  (when ios-device:debug
    (message "xcrun devicectl device process launch --device %s %s" identifier appIdentifier))
  (format "sh -c '\
    trap \"xcrun devicectl device process terminate --device %s --bundle-identifier %s; exit\" EXIT INT TERM; \
    xcrun devicectl device process launch --device %s %s & \
    echo \"App launched, capturing log...\"; \
    xcrun devicectl device console show --device %s | grep --line-buffered %s & \
    wait'"
          identifier
          appIdentifier
          identifier
          appIdentifier
          identifier
          (file-name-base appIdentifier)))

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
  "Get compiled app name from (buildfolder)."
  (if (file-exists-p buildfolder)
      (let ((binary-name (directory-files buildfolder nil "\\.app$")))
        (file-name-sans-extension (car binary-name)))
    nil))


(provide 'ios-device)
;;; ios-device.el ends here
