;;; music-control.el --- Control Music.app from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name <your.email@example.com>
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (nerd-icons "0.0.1"))
;; Keywords: multimedia
;; URL: https://github.com/yourusername/music-control

;;; Commentary:

;; This package provides functions to control Music.app from Emacs.
;; It allows you to:
;; - Search and filter songs/playlists
;; - Control playback (play/pause/next/previous)
;; - Display current track information in the modeline
;; - Adjust volume
;; - Browse music by artist, album, or playlist

;;; Code:

(require 'subr-x)
(require 'nerd-icons)

(defgroup music-control nil
  "Control Music.app from Emacs."
  :group 'multimedia)

(defcustom music-control-show-in-modeline t
  "Whether to show current track in the modeline."
  :type 'boolean
  :group 'music-control)

(defcustom music-control-modeline-update-interval 5
  "Interval in seconds to update the modeline track information."
  :type 'integer
  :group 'music-control)

(defcustom music-control-volume-step 10
  "Step size for volume adjustment (1-100)."
  :type 'integer
  :group 'music-control)

(defvar music-control--current-track nil
  "Current track information.")

(defvar music-control--modeline-timer nil
  "Timer for updating modeline track information.")

(defun music-control--run-osascript (script)
  "Run SCRIPT using osascript and return the result.
Returns nil if there was an error."
  (condition-case err
      (let ((result (string-trim
                     (shell-command-to-string
                      (format "osascript -e %s"
                              (shell-quote-argument script))))))
        (if (string-match-p "execution error:" result)
            (progn
              (message "Music.app error: %s" result)
              nil)
          result))
    (error
     (message "Error running AppleScript: %s" (error-message-string err))
     nil)))

(defun music-control--is-music-running ()
  "Check if Music.app is running."
  (string= "true"
           (music-control--run-osascript
            "tell application \"System Events\" to return (exists process \"Music\")")))

(defun music-control--ensure-music-running ()
  "Ensure Music.app is running, start it if needed."
  (unless (music-control--is-music-running)
    (message "Starting Music.app...")
    (music-control--run-osascript "tell application \"Music\" to activate")))

(defun music-control-play-pause ()
  "Toggle play/pause in Music.app."
  (interactive)
  (music-control--ensure-music-running)
  (music-control--run-osascript
   "tell application \"Music\" to playpause")
  (music-control--update-current-track))

(defun music-control-next-track ()
  "Play next track in Music.app."
  (interactive)
  (music-control--ensure-music-running)
  (music-control--run-osascript
   "tell application \"Music\" to play next track")
  (music-control--update-current-track))

(defun music-control-previous-track ()
  "Play previous track in Music.app."
  (interactive)
  (music-control--ensure-music-running)
  (music-control--run-osascript
   "tell application \"Music\" to play previous track")
  (music-control--update-current-track))

(defun music-control-volume-up ()
  "Increase Music.app volume."
  (interactive)
  (music-control--ensure-music-running)
  (music-control--run-osascript
   (format "
tell application \"Music\"
  set currentVolume to sound volume
  set newVolume to currentVolume + %d
  if newVolume > 100 then
    set newVolume to 100
  end if
  set sound volume to newVolume
  return newVolume
end tell" music-control-volume-step))
  (message "Volume: %s%%"
           (music-control--run-osascript "tell application \"Music\" to return sound volume")))

(defun music-control-volume-down ()
  "Decrease Music.app volume."
  (interactive)
  (music-control--ensure-music-running)
  (music-control--run-osascript
   (format "
tell application \"Music\"
  set currentVolume to sound volume
  set newVolume to currentVolume - %d
  if newVolume < 0 then
    set newVolume to 0
  end if
  set sound volume to newVolume
  return newVolume
end tell" music-control-volume-step))
  (message "Volume: %s%%"
           (music-control--run-osascript "tell application \"Music\" to return sound volume")))

(defun music-control-get-current-track ()
  "Get information about the current track in Music.app."
  (when (music-control--is-music-running)
    (let ((script "
tell application \"Music\"
  if player state is playing or player state is paused then
    set trackName to name of current track
    set trackArtist to artist of current track
    set trackAlbum to album of current track
    set trackDuration to duration of current track
    set trackPosition to player position
    set trackState to player state
    return trackName & \"::\" & trackArtist & \"::\" & trackAlbum & \"::\" & trackDuration & \"::\" & trackPosition & \"::\" & trackState
  else
    return \"\"
  end if
end tell"))
      (let ((result (music-control--run-osascript script)))
        (when (and result (not (string= result "")))
          (let* ((parts (split-string result "::"))
                 (name (nth 0 parts))
                 (artist (nth 1 parts))
                 (album (nth 2 parts))
                 (duration (string-to-number (nth 3 parts)))
                 (position (string-to-number (nth 4 parts)))
                 (state (nth 5 parts)))
            (list :name name :artist artist :album album 
                  :duration duration :position position :state state)))))))

(defun music-control--format-time (seconds)
  "Format SECONDS as mm:ss."
  (format "%d:%02d" 
          (/ seconds 60)
          (mod seconds 60)))

(defun music-control--update-current-track ()
  "Update the current track information."
  (setq music-control--current-track (music-control-get-current-track))
  (force-mode-line-update))

(defun music-control-display-current-track ()
  "Display information about the current track."
  (interactive)
  (let ((track (music-control-get-current-track)))
    (if track
        (let ((name (plist-get track :name))
              (artist (plist-get track :artist))
              (album (plist-get track :album))
              (duration (plist-get track :duration))
              (position (plist-get track :position))
              (state (plist-get track :state)))
          (message "%s %s - %s (%s) [%s/%s]"
                   (if (string= state "playing")
                       (nerd-icons-faicon "nf-fa-play")
                     (nerd-icons-faicon "nf-fa-pause"))
                   name artist album
                   (music-control--format-time position)
                   (music-control--format-time duration)))
      (message "No track is currently playing"))))

;; Information retrieval functions

(defun music-control-get-all-playlists ()
  "Get a list of all playlists in Music.app."
  (delete-dups
   (split-string
    (music-control--run-osascript
     "tell application \"Music\"
        set output to {}
        repeat with aPlaylist in user playlists
          copy (name of aPlaylist as string) to end of output
        end repeat
        return output
      end tell")
    ", ")))

(defun music-control-get-playlist-tracks (playlist-name)
  "Get a list of all tracks in the playlist with PLAYLIST-NAME."
  (let* ((script (format "
tell application \"Music\"
    set output to {}
    set theTracks to (every track of playlist \"%s\")
    repeat with aTrack in theTracks
        set trackName to (name of aTrack as string)
        set trackArtist to (artist of aTrack as string)
        copy (trackName & \" - \" & trackArtist) to end of output
    end repeat
    return output
end tell" playlist-name))
         (tracks (split-string (music-control--run-osascript script) ", ")))
    tracks))

(defun music-control-play-playlist (playlist-name &optional track-name)
  "Play the playlist with PLAYLIST-NAME, optionally starting from TRACK-NAME."
  (let ((script (if track-name
                    (format "
tell application \"Music\"
    set thePlaylist to playlist \"%s\"
    set theTrack to (first track of thePlaylist whose name contains \"%s\")
    play theTrack
    set shuffle enabled to false
    set song repeat to all
end tell" playlist-name (car (split-string track-name " - ")))
                  (format "
tell application \"Music\"
    set thePlaylist to playlist \"%s\"
    play thePlaylist
    set shuffle enabled to false
    set song repeat to all
end tell" playlist-name))))
    (music-control--run-osascript script)))

(defun music-control-get-all-artists ()
  "Get a list of all unique artists in Music.app."
  (delete-dups
   (split-string
    (music-control--run-osascript
     "tell application \"Music\" to get artist of every track")
    ", ")))

(defun music-control-get-artist-albums (artist)
  "Get a list of all albums by ARTIST."
  (delete-dups
   (split-string
    (music-control--run-osascript
     (format "tell application \"Music\" to get album of every track whose artist is \"%s\""
             artist))
    ", ")))

(defun music-control-get-album-tracks (album artist)
  "Get a list of all tracks in ALBUM by ARTIST."
  (let* ((script (format "
tell application \"Music\"
    set output to {}
    set theTracks to (every track whose album is \"%s\" and artist is \"%s\")
    repeat with aTrack in theTracks
        set trackNum to (track number of aTrack as string)
        if length of trackNum is 1 then
            set trackNum to \"0\" & trackNum
        end if
        set trackName to (name of aTrack as string)
        copy (trackNum & \". \" & trackName) to end of output
    end repeat
    return output
end tell" album artist))
         (tracks (split-string (music-control--run-osascript script) ", ")))
    (sort tracks
          (lambda (a b)
            (< (string-to-number (car (split-string a "\\. ")))
               (string-to-number (car (split-string b "\\. "))))))))

(defun music-control-play-track (track-name album artist)
  "Play the track with TRACK-NAME from ALBUM by ARTIST and continue with album."
  (music-control--ensure-music-running)
  (let ((clean-name (if (string-match "[0-9]+\\. \\(.*\\)" track-name)
                        (match-string 1 track-name)
                      track-name)))
    (music-control--run-osascript
     (format "
tell application \"Music\"
  -- Find and play the track first
  set selectedTrack to (first track whose name is \"%s\" and album is \"%s\" and artist is \"%s\")
  play selectedTrack

  -- Remove old playlists
  if exists playlist \"temp_playlist\" then
    delete playlist \"temp_playlist\"
  end if

  -- Create new playlist and add current track first
  set newList to make new playlist with properties {name:\"temp_playlist\"}
  duplicate (current track) to playlist \"temp_playlist\"

  -- Then try with the rest of the album
  tell current track
    set currentAlbum to album
  end tell

  duplicate (every track whose album is currentAlbum) to playlist \"temp_playlist\"

  -- Set settings
  set shuffle enabled to false
  set song repeat to all
  play playlist \"temp_playlist\"
end tell" clean-name album artist)))
  (music-control--update-current-track))

;; Interactive functions

(defun music-control-browse-playlist ()
  "Browse and play music from playlists."
  (interactive)
  (let* ((playlists (music-control-get-all-playlists))
         (playlist (completing-read "Choose playlist: " playlists))
         (tracks (music-control-get-playlist-tracks playlist))
         (action (completing-read "Action: " '("Play whole playlist" "Choose track"))))
    (if (string= action "Play whole playlist")
        (progn
          (music-control-play-playlist playlist)
          (message "Playing playlist: %s" playlist))
      (let* ((tracks-table (lambda (string pred action)
                            (if (eq action 'metadata)
                                '(metadata (display-sort-function . identity))
                              (complete-with-action action tracks string pred))))
             (track (completing-read "Choose track: " tracks-table)))
        (when track
          (music-control-play-playlist playlist track)
          (message "Playing: %s from playlist %s" track playlist))))))

(defun music-control-browse-artist ()
  "Browse music by selecting artist, then album or songs."
  (interactive)
  (let* ((artists (music-control-get-all-artists))
         (artist (completing-read "Choose artist: " artists))
         (albums (music-control-get-artist-albums artist))
         (album (completing-read "Choose album: " albums))
         (tracks (music-control-get-album-tracks album artist))
         (tracks-table (lambda (string pred action)
                        (if (eq action 'metadata)
                            '(metadata (display-sort-function . identity))
                          (complete-with-action action tracks string pred))))
         (track (completing-read "Choose track: " tracks-table)))
    (when track
      (music-control-play-track track album artist)
      (message "Playing: %s - %s - %s" artist album track))))

(defun music-control-modeline ()
  "Return a string for the modeline."
  (when (and music-control-mode
             music-control-show-in-modeline
             music-control--current-track)
    (let* ((track music-control--current-track)
           (name (plist-get track :name))
           (artist (plist-get track :artist))
           (state (plist-get track :state)))
      (when name
        (concat " "
                (if (string= state "playing")
                    (nerd-icons-faicon "nf-fa-play")
                  (nerd-icons-faicon "nf-fa-pause"))
                " "
                (truncate-string-to-width
                 (format "%s - %s" name artist)
                 30 nil nil "...")
                " ")))))

(defun music-control--start-modeline-timer ()
  "Start the timer for updating the modeline."
  (when (and music-control-show-in-modeline
             (not music-control--modeline-timer))
    (setq music-control--modeline-timer
          (run-with-timer 0 music-control-modeline-update-interval
                          #'music-control--update-current-track))))

(defun music-control--stop-modeline-timer ()
  "Stop the modeline update timer."
  (when music-control--modeline-timer
    (cancel-timer music-control--modeline-timer)
    (setq music-control--modeline-timer nil)))

;; Mode definition

(defvar music-control-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m p") #'music-control-play-pause)
    (define-key map (kbd "C-c m n") #'music-control-next-track)
    (define-key map (kbd "C-c m b") #'music-control-previous-track)
    (define-key map (kbd "C-c m a") #'music-control-browse-artist)
    (define-key map (kbd "C-c m l") #'music-control-browse-playlist)
    (define-key map (kbd "C-c m i") #'music-control-display-current-track)
    (define-key map (kbd "C-c m +") #'music-control-volume-up)
    (define-key map (kbd "C-c m -") #'music-control-volume-down)
    map)
  "Keymap for music-control-mode.")

;; Add to the global mode line format
(unless (member '(:eval (music-control-modeline)) global-mode-string)
  (setq global-mode-string
        (append global-mode-string
                '((:eval (music-control-modeline))))))

;;;###autoload
(define-minor-mode music-control-mode
  "Minor mode for controlling Music.app from Emacs.

\\{music-control-mode-map}"
  :lighter " Music"
  :keymap music-control-mode-map
  :global t
  (if music-control-mode
      (progn
        (music-control--update-current-track)
        (music-control--start-modeline-timer))
    (music-control--stop-modeline-timer)))

(provide 'music-control)

;;; music-control.el ends here
