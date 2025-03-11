# Music Control for Emacs

Control Apple Music.app directly from Emacs.

## Features

- Play/pause, next/previous track controls
- Browse and play music by artist, album, or playlist
- Display current track information in the modeline
- Volume control
- Customizable display options

## Installation

1. Clone this repository or copy `music-control.el` to your Emacs load path
2. Add to your Emacs configuration:

```elisp
(require 'music-control)
(music-control-mode 1)  ;; Enable the global minor mode
```

## Key Bindings

| Key       | Function                         | Description                      |
|-----------|----------------------------------|----------------------------------|
| `C-c m p` | `music-control-play-pause`       | Toggle play/pause                |
| `C-c m n` | `music-control-next-track`       | Play next track                  |
| `C-c m b` | `music-control-previous-track`   | Play previous track              |
| `C-c m a` | `music-control-browse-artist`    | Browse and play by artist/album  |
| `C-c m l` | `music-control-browse-playlist`  | Browse and play from playlists   |
| `C-c m i` | `music-control-display-current-track` | Show current track details  |
| `C-c m +` | `music-control-volume-up`        | Increase volume                  |
| `C-c m -` | `music-control-volume-down`      | Decrease volume                  |

## Customization

```elisp
;; Show current track in modeline
(setq music-control-show-in-modeline t)

;; Set modeline update interval (seconds)
(setq music-control-modeline-update-interval 5)

;; Set volume adjustment step
(setq music-control-volume-step 5)
```

## Requirements

- macOS with Music.app
- Emacs 27.1 or later
- nerd-icons package

❤️ [Please sponsor me if you like this package](https://github.com/sponsors/konrad1977)
