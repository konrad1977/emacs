(require 'autothemer)

(autothemer-deftheme
catppuccin "A theme to set the mood for Halloween"

 ((((class color) (min-colors #xFFFFFF))) ;; We're only concerned with graphical Emacs

  ;; Define our color palette
  (rosewater  "#F5E0DC")
  (flamingo   "#F2CDCD")
  (magenta    "#C6AAE8")
  (pink       "#E5B4E2")
  (maroon     "#E49CB3")
  (red        "#E38C8F")
  (peach      "#F7BE95")
  (yellow     "#ECDDAA")
  (green      "#B1E1A6")
  (teal       "#B7E5E6")
  (blue       "#A3B9EF")
  (sky        "#92D2E8")
  (lavender   "#C9CBFF")
  (dark       "#0C0A10")
  (black      "#15121C")
  (black-1    "#1B1923")
  (black-2    "#1E1E28")
  (black-3    "#332E41")
  (black-4    "#575268")
  (gray       "#6E6C7E")
  (gray-1     "#988BA2")
  (gray-2     "#C3BAC6")
  (white      "#DFDEF1")
  )

 ;; Customize faces
  (
  (default                           (:foreground white :background black-2))
  (cursor                            (:background red))
  (line-number                       (:foreground black-4 :background black-2))
  (line-number-current-line          (:foreground green))
  (fringe                            (:background black-2))
  (region                            (:background black-4))
  (mode-line                         (:background black-1))
  (mode-line-inactive                (:background black-2 :foreground gray-1))
  (mode-line-highlight               (:foreground lavender))

  ;; current line
  (hl-line                           (:background black))
  (vertical-border                   (:foreground black-3 :background nil))
  (window-divider-first-pixel        (:foreground black-2))
  (window-divider-last-pixel         (:background black-2))
  (separator-line                    (:background black-2))
  (border                            (:background black-2))
  (window-border                     (:background black-2))
  (internal-border                   (:background black-2))
  (child-frame-border                (:foreground black-2))

  (highlight-quoted-symbol           (:foreground maroon))
  (minibuffer-prompt-end             (:background dark))

  (font-lock-comment-face            (:foreground gray))
  (font-lock-type-face               (:foreground teal))
  (font-lock-constant-face           (:foreground blue))
  (font-lock-keyword-face            (:foreground red))
  (font-lock-constant-face           (:foreground lavender))
  (font-lock-string-face             (:foreground green))
  (font-lock-builtin-face            (:foreground peach))
  (font-lock-function-name-face      (:foreground magenta))
  (font-lock-variable-name-face      (:foreground yellow))
  (elisp-shorthand-font-lock-face    (:foreground yellow))

  (info-xref  (:foreground yellow))
  (minibuffer-prompt (:foreground pink))

  (trailing-whitespace               (:background red))
  ;; Battery colors
  (doom-modeline-battery-critical    (:foreground red))
  (doom-modeline-battery-warning     (:foreground red))
  (doom-modeline-battery-charging    (:foreground green))
  (doom-modeline-battery-error       (:foreground red))
  (doom-modeline-battery-normal      (:foreground teal))
  (doom-modeline-battery-full        (:foreground green))

  ;; Doom visual state
  (doom-modeline-evil-motion-state   (:foreground teal))
  (doom-modeline-evil-emacs-state    (:foreground blue))
  (doom-modeline-evil-insert-state   (:foreground green))
  (doom-modeline-evil-normal-state   (:foreground pink))
  (doom-modeline-evil-visual-state   (:foreground sky))
  (doom-modeline-evil-replace-state  (:foreground red))
  (doom-modeline-evil-operator-state (:foreground blue))

  (doom-modeline-project-dir         (:bold t :foreground teal))
  (doom-modeline-buffer-path         (:inherit 'bold :foreground green))
  (doom-modeline-buffer-file         (:inherit 'bold :foreground red))
  (doom-modeline-buffer-modified     (:inherit 'bold :foreground yellow))
  (doom-modeline-error               (:background red))
  (doom-modeline-buffer-major-mode   (:foreground green :bold t))
  (doom-modeline-info                (:bold t :foreground teal))
  (doom-modeline-bar                 (:background yellow))
  (doom-modeline-panel               (:background green :foreground white))

  (doom-modeline-project-dir         (:bold t :foreground peach))
  (doom-modeline-buffer-path         (:inherit 'bold :foreground red))
  (doom-modeline-buffer-file         (:inherit 'bold :foreground rosewater))
  (doom-modeline-buffer-modified     (:inherit 'bold :foreground yellow))
  (doom-modeline-error               (:background black))
  (doom-modeline-buffer-major-mode   (:foreground green :bold t))
  (doom-modeline-info                (:bold t :foreground peach))
  (doom-modeline-bar                 (:background green))
  (doom-modeline-panel               (:background green :foreground white))

  ;; which-key
  (which-key-key-face                   (:foreground green))
  (which-key-group-description-face     (:foreground red))
  (which-key-command-description-face   (:foreground blue))
  (which-key-local-map-description-face (:foreground yellow))

  ;; swiper
  (swiper-line-face                     (:foreground yellow))
  (counsel-outline-default              (:foreground yellow))
  (comint-highlight-prompt (:foreground yellow))
  (info-header-xref                     (:foreground yellow))

  ;; counsel
  (company-preview-common               (:foreground red))
  (company-tooltip-common               (:foreground peach))
  (company-tooltip-common-selection     (:foreground peach))
  (company-tooltip-annotation           (:foreground peach))
  (company-tooltip-annotation-selection (:foreground peach))
  (company-scrollbar-bg                 (:background black-3))
  (company-scrollbar-fg                 (:background teal))
  (company-tooltip-selection            (:background black))
  (company-tooltip-mouse                (:background black-2 :foreground nil))

  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground black-3))
  (highlight-indent-guides-stack-character-face (:foreground black-3))
  (highlight-indent-guides-stack-odd-face       (:foreground black-3))
  (highlight-indent-guides-stack-even-face      (:foreground black-4))
  (highlight-indent-guides-stack-character-face (:foreground black-3))
  (highlight-indent-guides-even-face            (:foreground black-2))
  (highlight-indent-guides-odd-face             (:foreground black-4))

   ;;;; ivy
  (ivy-current-match                            (:background black-3))
  (ivy-subdir                                   (:background nil :foreground peach))
  (ivy-action                                   (:background nil :foreground peach))
  (ivy-grep-line-number                         (:background nil :foreground peach))
  (ivy-minibuffer-match-face-1                  (:background nil :foreground yellow))
  (ivy-minibuffer-match-face-2                  (:background nil :foreground yellow))
  (ivy-minibuffer-match-highlight               (:foreground teal))

  ;; evil
  (evil-ex-substitute-replacement (:foreground peach :strike-through nil :inherit 'evil-ex-substitute-matches))
  (evil-search-highlight-persist-highlight-face (:background yellow))
  (button (:foreground yellow :background red))

  ))

(provide-theme 'catppuccin)
