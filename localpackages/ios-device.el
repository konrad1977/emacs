;;; ios-device.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; commentary:

;;; package for handling a ios device.
;;; code:

(require 'periphery-helper)

(defconst ios-device:buffer-name "*ios-device-"
  "Name of the buffer.")

(defvar current-buffer-name nil)
(defvar current-device-id nil)
(defvar current-install-command nil)
(defvar current-device-identifier nil)
(defvar current-app-identifier nil)
(defvar current-buffer nil)
(defvar DEBUG t)

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
         (clean-up-newlines
          (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
    (if (= (length device-id) 0)
        (setq current-device-id nil)
      (setq current-device-id (ios-device:format-id device-id))))
  current-device-id)

(cl-defun ios-device:install-app (&key buildfolder &key appIdentifier)
  "Install app on device (BUILDFOLDER APPIDENTIFIER)."
  (ios-device:kill-buffer)
  (setq current-app-identifier appIdentifier)
  (let* ((default-directory buildfolder)
         (appname (ios-device:app-name-from :buildfolder buildfolder))
         (command (ios-device:install-cmd :identifier (ios-device:identifier) :appname appname))
         (buffer (get-buffer-create (concat ios-device:buffer-name appname))))

    (setq current-buffer buffer)
    (when DEBUG
      (message "install-app: %s" command)
      (message "build-folder: %s" buildfolder)
      (message "app-name: %s" appname))

    (async-start-command-to-string
     :command command
     :callback '(lambda (output)
                  (let ((run-command (ios-device:run-cmd :identifier (ios-device:identifier) :appIdentifier current-app-identifier)))
                    (inhibit-sentinel-messages #'async-shell-command
                                               run-command
                                               current-buffer))))

    (with-current-buffer buffer
      (setq-local mode-line-format nil)
      (setq-local left-fringe-width 0)
      (setq-local right-fringe-width 0))))

(defun ios-device:fetch-device-id ()
    "Fetch the device id."
  (clean-up-newlines
   (shell-command-to-string
    "xcrun devicectl list devices | awk 'NR>3 { match($0, /[[:xdigit:]]{8}-([[:xdigit:]]{4}-){3}[[:xdigit:]]{12}/); print substr($0, RSTART, RLENGTH) }'")))

(defun ios-device:identifier ()
  "Get iOS device identifier."
  (if current-device-identifier
      current-device-identifier
    (setq current-device-identifier (ios-device:fetch-device-id))
    current-device-identifier))

(defun ios-device:kill-buffer ()
  "Kill the ios-device buffer."
  (when (and current-buffer-name (get-buffer current-buffer-name))
    (kill-buffer current-buffer-name)))

(cl-defun ios-device:install-cmd (&key identifier &key appname)
  "Install app on device (IDENTIFIER APPNAME)."
  (format "xcrun devicectl device install app --device %s '%s.app'"
          identifier
          appname))

(cl-defun ios-device:run-cmd (&key identifier &key appIdentifier)
  "Run app on device (IDENTIFIER APPIDENTIFIER)."
  (format "xcrun devicectl device process launch --device %s %s"
          identifier
          appIdentifier))

(cl-defun ios-device:install-app-async (&key buildfolder &key appIdentifier)
  "Install app on device (PROJECT-ROOT BUILDFOLDER APPNAME)."
  (ios-device:kill-buffer)
  (let* ((default-directory buildfolder)
         (identifier (ios-device:identifier))
         (buffer (get-buffer-create (concat ios-device:buffer-name appname))))
    (setq current-buffer-name buffer)
    (async-shell-command command buffer)))

(cl-defun ios-device:app-name-from (&key buildfolder)
  "Get compiled app name from (buildfolder)."
  (if (file-exists-p buildfolder)
      (let ((binary-name (directory-files buildfolder nil "\\.app$")))
        (file-name-sans-extension (car binary-name)))
    nil))


(provide 'ios-device)
;;; ios-device.el ends here
