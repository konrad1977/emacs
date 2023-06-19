;;; ios-device.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; commentary:

;;; package for handling a ios device.
;;; code:

(require 'periphery-helper)

(defconst ios-device:buffer-name "*ios-device-"
  "Name of the buffer.")

(defvar current-buffer-name nil)
(defvar current-device-id nil)
(defvar current-install-command)

(defun ios-device:format-id (id)
  "Format device id (as ID)."
  (if id
      (if (not
           (string-match-p (regexp-quote "-") id))
          (concat (substring id 0 8) "-" (substring id 6))
        id)))

(defun ios-device:id ()
  "Get the id of the connected device."
  (if current-device-id
      current-device-id
    (progn
      (let ((device-id
             (clean-up-newlines
              (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
        (if (= (length device-id) 0)
            nil
          (setq current-device-id (ios-device:format-id device-id))))
      current-device-id)))

(cl-defun ios-device:install-app (&key project-root &key buildfolder &key appname)
  "Install an app on device (PROJECT-ROOT BUILDFOLDER APPNAME)."
  (ios-device:kill-buffer)
  (let* ((folder buildfolder)
         (app-name (ios-simulator:app-name-from :folder folder))
         (install-path (concat current-project-root "/" folder))
         (default-directory install-path)
         (command (format "ios-deploy -b %s.app -d" app-name))
         (buffer (get-buffer-create (concat ios-device:buffer-name app-name))))

    (message-with-color :tag "[Installing]" :text (format "%s onto physical device. Will launch app when done." app-name) :attributes 'warning)
    (setq current-buffer-name buffer)
    (inhibit-sentinel-messages #'async-shell-command
                               command
                               buffer)
    (with-current-buffer buffer
      (setq-local mode-line-format nil)
      (setq-local left-fringe-width 0)
      (setq-local right-fringe-width 0))))


(defun ios-device:kill-buffer ()
  "Kill the ios-device buffer."
  (when (and current-buffer-name (get-buffer current-buffer-name))
      (kill-buffer current-buffer-name)))

(provide 'ios-device)
;;; ios-device.el ends here
