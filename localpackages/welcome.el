;;; welcome.el --- Simple welcome screen -*- lexical-binding: t; -*-

;;; Commentary:
;; Welcome screen

;;; code:

(require 'all-the-icons)
(require 'async)
(require 'json)
;; (require 'projectile)
(require 'recentf)
(require 'url)

(defvar welcome-mode nil)
(defvar welcome-recentfiles '()
  "Recent list.")
(defvar recent-projects '()
  "List of recent projects.")

(defvar temperature nil)
(defvar weatherdescription nil)
(defvar weathericon nil)

(defcustom welcome-title "Quick access [C-number to open file]"
  "Welcome title."
  :group 'welcome
  :type '(string))

(defcustom welcome-min-left-padding 10
  "Minimum left padding when resizing window."
  :group 'welcome
  :type '(natnum))

(defcustom welcome-path-max-length 72
  "Latitude for weather information."
  :group 'welcome
  :type '(natnum))

(defcustom welcome-latitude nil
  "Latitude for weather information."
  :group 'welcome
  :type '(float))

(defcustom welcome-longitude nil
  "Longitude for weather information in welcome package."
  :group 'welcome
  :type '(float))

(defcustom welcome-image-file ""
  "Image file in welcome package."
  :group 'welcome
  :type '(file))

(defcustom welcome-image-width 200
  "Image width for weather information."
  :group 'welcome
  :type '(natnum))

(defcustom welcome-image-height 200
  "Image width for weather information."
  :group 'welcome
  :type '(natnum))

(defgroup welcome nil
  "Welcome group."
  :group 'applications)

(defconst welcome-buffer "*welcome*"
  "Welcome buffer name.")

(defvar welcome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'welcome--open-recent-file)
    (define-key map (kbd "<return>") 'welcome--open-recent-file)
    (define-key map (kbd "o") 'welcome--open-recent-file)

    ;; Add shortcuts for file indexes
    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (concat "C-" (number-to-string i)))
        `(lambda ()
           (interactive)
           (welcome--open-recent-file-at-index ,i))))
    map)
  "Keymap for `welcome-mode'.")

(define-derived-mode welcome-mode fundamental-mode "Welcome"
  "Major mode for the welcome screen."
  :group 'welcome
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local global-hl-line-mode nil)
  (use-local-map welcome-mode-map))

(defface welcome-title-face
  '((t :inherit link :height 1.2))
  "Face added to code-usage display."
  :group 'welcome)

(defface welcome-info-face
  '((t :foreground "#F66D86" :height 0.9 :bold t :italic t))
  "Face added to code-usage display."
  :group 'welcome)

(defface welcome-text-info-face
  '((t :foreground "#ADB5D0" :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'welcome)

(defface welcome-path-face
  '((t :foreground "#63677D" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for the file path."
  :group 'welcome)

(defface welcome-filename-face
  '((t :inherit default :weight semi-bold :italic nil))
  "Face for the file name."
  :group 'welcome)

(defface welcome-time-face
  '((t :inherit font-lock-comment-face))
  "Face for time."
  :group 'welcome)

(defface welcome-weather-description-face
  '((t :inherit font-lock-constant-face :height 0.9))
  "Face for time."
  :group 'welcome)

(defface welcome-weather-icon-face
  '((t :inherit default :height 0.9))
  "Face for time."
  :group 'welcome)

(defface welcome-weather-temperature-face
  '((t :inherit font-lock-function-name-face :height 0.9))
  "Face for time."
  :group 'welcome)

(defun weather-icon-from-code (code)
  "Maps a weather code to a corresponding string."
  (pcase code
    (`0 "wi-day-sunny")
    ((or `1 `2 `3) "wi-day-cloudy")
    ((or `45 `48) "wi-day-fog")
    ((or `51 `53 `55) "wi-sprinkle")
    ((or `56 `57) "wi-snow")
    ((or `61 `63 `65) "wi-day-rain")
    ((or `66 `67) "wi-day-rain-mix")
    ((or `71 `73 `75) "wi-snow")
    (`77 "wi-snow")
    ((or `80 `81 `82) "wi-rain")
    ((or `85 `86) "wi-rain-mix")
    ((or `95 `96 `99) "wi-thunderstorm")
    (_ "Unknown")))

(defun weather-code-to-string (code)
  "Maps a weather code to a corresponding string."
  (pcase code
    (`0 "Clear sky")
    ((or `1 `2 `3) "Partly cloudy")
    ((or `45 `48) "Fog")
    ((or `51 `53 `55) "Drizzle")
    ((or `56 `57) "Freezing drizzle")
    ((or `61 `63 `65) "Rain")
    ((or `66 `67) "Freezing rain")
    ((or `71 `73 `75) "Snowfall")
    (`77 "Snow grains")
    ((or `80 `81 `82) "Rain showers")
    ((or `85 `86) "Snow showers")
    ((or `95 `96 `99) "Thunderstorm")
    (_ "Unknown")))

(defun welcome--insert-centered (text)
  "Insert TEXT at the center of the current line."
  (let ((width (window-width)))
    (insert (make-string (/ (- width (length text)) 2) ?\ ))
    (insert text)))

(defun welcome--open-recent-file ()
  "Open the recent file on the current line."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (prop-pos (next-single-property-change line-start 'path nil line-end)))
    (when prop-pos
      (let ((file (get-text-property prop-pos 'path)))
        (message file)
        (if (file-exists-p file)
            (find-file file)
          (error "File %s does not exist" file))))))

(defun welcome--open-recent-file-at-index (index)
  "Open the recent file at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((files welcome-recentfiles))
    (when (<= 1 index (length files))
      (find-file (nth (1- index) files)))))

(defun welcome:truncate-path-in-middle (path n)
  "Truncate the middle of PATH to length N by removing characters and adding an ellipsis."
  (if (<= (length path) n)
      path
    (let* ((left (/ (- n 3) 2))
           (right (- n left 3))
           (head (substring path 0 (+ left 1)))
           (tail (substring path (- (length path) right)))
           (ellipsis "..."))
      (concat head ellipsis tail))))

(defun welcome--insert-recent-files ()
  "Insert the first 9 recent files with icons in the welcome buffer."
  (recentf-mode)
  (insert "\n")
  (let* ((files welcome-recentfiles)
         (left-margin (welcome:calculate-padding-left)))
    (dolist (file files)
      (let* ((index (cl-position file files :test #'equal))
             (full-path (file-truename file))
             (shortcut (format "%d" (+ index +1)))
             (file-name (file-name-nondirectory file))
             (file-dir (file-name-directory file))
             (title (format "%s %s%s"
                    (propertize (all-the-icons-icon-for-file file :v-adjust -0.05) 'face '(:family "all-the-icons" :height 1.0))
                    (propertize (welcome:truncate-path-in-middle file-dir welcome-path-max-length) 'face 'welcome-path-face)
                    (propertize file-name 'face 'welcome-filename-face)))
             (title-with-path (propertize title 'path full-path))
             (title-with-path-and-shortcut (concat title-with-path (propertize (format " [%s]" shortcut) 'face '(:height 0.9 :inherit font-lock-constant-face)))))
        (insert (format "%s%s\n" (make-string left-margin ?\s) title-with-path-and-shortcut))))))

(defun welcome:calculate-padding-left ()
  "Calculate padding for left side."
  (let* ((max-length (apply 'max (mapcar (lambda (path) (length (welcome:truncate-path-in-middle path welcome-path-max-length))) welcome-recentfiles)))
         (filenames (mapcar (lambda (path) (file-name-nondirectory path)) welcome-recentfiles))
         (max-filename-length (/ (apply 'max (mapcar 'length filenames)) 2))
         (left-margin (max (+ welcome-min-left-padding max-filename-length) (/ (- (window-width) max-length) 2))))
    (- left-margin max-filename-length)))

(defun welcome--insert-text (text)
  "Insert (as TEXT)."
  (let ((left-margin (welcome:calculate-padding-left)))
    (insert (format "%s%s\n" (make-string left-margin ?\s) text))))

(defun welcome--redisplay-buffer-on-resize (&rest _)
  "Resize current buffer."
  (when (equal (buffer-name) welcome-buffer)
    (welcome--refresh-screen)))

(defun fetch-weather-data ()
  "Fetch weather data from API."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true" welcome-latitude welcome-longitude)))
    (url-retrieve url
                  (lambda (_)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
                           (json-obj (json-read-from-string json-data))
                           (current-weather (cdr (assoc 'current_weather json-obj)))
                           (temp (cdr (assoc 'temperature current-weather)))
                           (weather-code (cdr (assoc 'weathercode current-weather)))
                           (weather-icon (all-the-icons-icon-for-weather
                                          (weather-icon-from-code weather-code))))
                      (setq weathericon weather-icon)
                      (setq temperature (format "%s" temp))
                      (setq weatherdescription (format "%s" (weather-code-to-string weather-code))))
                    (welcome--refresh-screen))
                  nil
                  t)))


;; (defun get-last-projects ()
;;   "Get a list of the last 3 projects and their recently opened files."
;;   (let ((projects (projectile-relevant-known-projects)))
;;     (mapcar (lambda (project)
;;               (cons (projectile-project-name project)))
;;             projects)))

;;;###autoload
(defun welcome-create-welcome-hook ()
  "Setup welcome screen."
  (when (< (length command-line-args) 2)
    (add-hook 'switch-to-buffer 'welcome--redisplay-buffer-on-resize)
    (add-hook 'window-size-change-functions 'welcome--redisplay-buffer-on-resize)
    (add-hook 'emacs-startup-hook (lambda ()
                                    (welcome--refresh-screen)
                                    (fetch-weather-data)))))

(defun insert-startup-time ()
  "Insert startup time."
  (welcome--insert-text (format "%s %s %s %s"
                                (propertize (all-the-icons-octicon "clock")
                                            'face `(:family ,(all-the-icons-octicon-family) :height 1.0)
                                            'display '(raise 0))
                                (propertize "Startup time:" 'face 'welcome-text-info-face)
                                (propertize (emacs-init-time "%.2f") 'face 'welcome-info-face)
                                (propertize "seconds" 'face 'welcome-text-info-face))))


(defun insert-package-info (packages)
  "Insert package info as (PACKAGES)."
  (welcome--insert-text (format "%s %s %s"
                                (propertize (all-the-icons-octicon "package")
                                            'face `(:family ,(all-the-icons-octicon-family) :height 1.0)
                                            'display '(raise 0))
                                (propertize packages 'face 'welcome-info-face)
                                (propertize "packages loaded" 'face 'welcome-text-info-face))))

(defun insert-weather-info ()
  "Insert weather info."
  (if weatherdescription
    (welcome--insert-text (format "%s %s, %s%s"
                                  (propertize weathericon 'face '(:family "Weather icons" :height 1.0) 'display '(raise 0))
                                  (propertize weatherdescription 'face 'welcome-weather-description-face)
                                  (propertize temperature 'face 'welcome-weather-temperature-face)
                                  (propertize "℃" 'face 'welcome-text-info-face)))
    (welcome--insert-text (propertize "Loading weather data..." 'face 'welcome-weather-temperature-face))))

(defun insert-recent-projects ()
  "Insert recent projects."
  (projectile-mode +1)
  (setq recent-projects (projectile-relevant-known-projects))
  (dolist (project (seq-take recent-projects 3))
    (welcome--insert-text (projectile-project-name project))))

(defun welcome--refresh-screen ()
  "Show the welcome screen."
  (setq welcome-recentfiles (seq-take recentf-list 9))
  (with-current-buffer (get-buffer-create welcome-buffer)
    (let* ((buffer-read-only)
           (image (create-image welcome-image-file 'png nil :width welcome-image-width :height welcome-image-height))
           (size (image-size image))
           (width (car size))
           (left-margin (max welcome-min-left-padding (floor (/ (- (window-width) width) 2))))
           (packages (format "%d" (length package-activated-list))))
      (erase-buffer)
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (insert "\n")
        (welcome--insert-text (propertize welcome-title 'face 'welcome-title-face))
        (welcome--insert-recent-files)
        (setq cursor-type nil)
        (insert "\n\n")

        (insert-startup-time)
        (insert-package-info packages)
        (insert-weather-info)

        ;; (insert "\n")
        ;; (insert-recent-projects)

        (insert "\n\n")
        (insert (make-string left-margin ?\ ))
        (insert-image image)
        (insert "\n\n")
        (welcome--insert-centered (propertize (format-time-string "%A, %B %d %H:%M") 'face 'welcome-time-face))
        (switch-to-buffer welcome-buffer)
        (read-only-mode +1)
        (welcome-mode)
        (goto-char (point-min))

        (forward-line 3)))))

(provide 'welcome)
;;; welcome.el ends here
