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

;; Mode definition

(defvar music-control-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m p") #'music-control-play-pause)
    (define-key map (kbd "C-c m n") #'music-control-next-track)
    (define-key map (kbd "C-c m b") #'music-control-previous-track)
    (define-key map (kbd "C-c m a") #'music-control-browse-artist)
    (define-key map (kbd "C-c m l") #'music-control-browse-playlist)
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
