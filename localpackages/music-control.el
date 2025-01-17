;;; music-control.el --- Control Music.app from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name <your.email@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia
;; URL: https://github.com/yourusername/music-control

;;; Commentary:

;; This package provides functions to control Music.app from Emacs.
;; It allows you to:
;; - Search and filter songs/playlists
;; - Control playback (play/pause/next)
;; - Display current track information

;;; Code:

(require 'subr-x)
(require 'nerd-icons)

(defgroup music-control nil
  "Control Music.app from Emacs."
  :group 'multimedia)

(defun music-control--run-osascript (script)
  "Run SCRIPT using osascript and return the result."
  (string-trim
   (shell-command-to-string
    (format "osascript -e %s"
            (shell-quote-argument script)))))

(defun music-control-play-pause ()
  "Toggle play/pause in Music.app."
  (interactive)
  (music-control--run-osascript
   "tell application \"Music\" to playpause"))

(defun music-control-next-track ()
  "Play next track in Music.app."
  (interactive)
  (music-control--run-osascript
   "tell application \"Music\" to play next track"))

(defun music-control-previous-track ()
  "Play previous track in Music.app."
  (interactive)
  (music-control--run-osascript
   "tell application \"Music\" to play previous track"))

;; Information retrieval functions

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

(defun music-control--run-osascript (script)
  "Run SCRIPT using osascript and return the result."
  (string-trim
   (shell-command-to-string
    (format "osascript -e %s"
            (shell-quote-argument script)))))

(defun music-control-play-track (track-name album artist)
  "Play the track with TRACK-NAME from ALBUM by ARTIST and continue with album."
  (let ((clean-name (if (string-match "[0-9]+\\. \\(.*\\)" track-name)
                        (match-string 1 track-name)
                      track-name)))
    (music-control--run-osascript
     (format "
tell application \"Music\"
  -- Hitta och spela spåret först
  set selectedTrack to (first track whose name is \"%s\" and album is \"%s\" and artist is \"%s\")
  play selectedTrack

  -- Ta bort gamla spellistor
  if exists playlist \"temp_playlist\" then
    delete playlist \"temp_playlist\"
  end if

  -- Skapa ny och lägg till aktuellt spår först
  set newList to make new playlist with properties {name:\"temp_playlist\"}
  duplicate (current track) to playlist \"temp_playlist\"

  -- Sen försöker vi med resten av albumet
  tell current track
    set currentAlbum to album
  end tell

  duplicate (every track whose album is currentAlbum) to playlist \"temp_playlist\"

  -- Sätt inställningar
  set shuffle enabled to false
  set song repeat to all
  play playlist \"temp_playlist\"
end tell" clean-name album artist))))

;; Interactive functions
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
;; Mode definition

(defvar music-control-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m p") #'music-control-play-pause)
    (define-key map (kbd "C-c m n") #'music-control-next-track)
    (define-key map (kbd "C-c m b") #'music-control-previous-track)
    (define-key map (kbd "C-c m a") #'music-control-browse-artist)
    map)
  "Keymap for music-control-mode.")

;;;###autoload
(define-minor-mode music-control-mode
  "Minor mode for controlling Music.app from Emacs."
  :lighter " Music"
  :keymap music-control-mode-map
  :global t)

(provide 'music-control)

;;; music-control.el ends here
