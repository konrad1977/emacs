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
  (default                             (:foreground rosewater :background black-2))
  (cursor                              (:background red))
  (line-number                         (:foreground black-4 :background nil))
  (line-number-current-line            (:foreground green))
  (fringe                              (:background black-2))
  (region                              (:background black-4))
  (success                             (:foreground green))
  (error                               (:foreground red))
  (warning                             (:foreground maroon))
  (mode-line                           (:background black-1))
  (mode-line-inactive                  (:background black-1 :foreground gray-1))
  (mode-line-highlight                 (:foreground rosewater))
  (button                              (:foreground green))

  ;; current line
  (hl-line                             (:background black))
  (vertical-border                     (:foreground black-3 :background nil))
  (window-divider-first-pixel          (:foreground black-2))
  (window-divider-last-pixel           (:background black-2))
  (separator-line                      (:background black-2))
  (border                              (:background black-2))
  (window-border                       (:background black-2))
  (internal-border                     (:background black-2))
  (child-frame-border                  (:foreground black-2))


  (font-lock-type-face                 (:foreground teal))
  (font-lock-constant-face             (:foreground blue))
  (font-lock-keyword-face              (:bold t :foreground maroon))
  (font-lock-constant-face             (:foreground lavender))
  (font-lock-string-face               (:foreground green))
  (font-lock-builtin-face              (:foreground yellow))
  (font-lock-function-name-face        (:foreground blue))
  (font-lock-variable-name-face        (:foreground peach))
  (font-lock-comment-face              (:foreground gray))
  (font-lock-comment-delimiter-face    (:foreground gray))
  (font-lock-doc-face                  (:foreground gray))
  (font-lock-doc-markup-face           (:foreground gray))
  (font-lock-warning-face              (:foreground maroon))
  (font-lock-negation-char-face        (:foreground red))
  (font-lock-regexp-grouping-backslash (:foreground teal))
  (elisp-shorthand-font-lock-face      (:foreground yellow))

  (info-xref                           (:foreground yellow))
  (highlight-quoted-symbol             (:foreground maroon))
  (minibuffer-prompt-end               (:background dark))
  (minibuffer-prompt                   (:foreground lavender :background black-1))
  (epa-mark                            (:foreground pink))
  (dired-mark                          (:foreground pink))

  (trailing-whitespace                 (:background red))
  ;; Battery colors
  (doom-modeline-battery-critical      (:foreground red))
  (doom-modeline-battery-warning       (:foreground maroon))
  (doom-modeline-battery-charging      (:foreground gray-2))
  (doom-modeline-battery-error         (:foreground red))
  (doom-modeline-battery-normal        (:foreground gray-2))
  (doom-modeline-battery-full          (:foreground green))

  ;; Doom visual state
  (doom-modeline-evil-motion-state     (:foreground teal))
  (doom-modeline-evil-emacs-state      (:foreground blue))
  (doom-modeline-evil-insert-state     (:foreground green))
  (doom-modeline-evil-normal-state     (:foreground pink))
  (doom-modeline-evil-visual-state     (:foreground sky))
  (doom-modeline-evil-replace-state    (:foreground red))
  (doom-modeline-evil-operator-state   (:foreground blue))

  (doom-modeline-project-dir           (:bold t :foreground green))
  (doom-modeline-buffer-path           (:inherit 'bold :foreground green))
  (doom-modeline-buffer-file           (:inherit 'bold :foreground maroon))
  (doom-modeline-buffer-modified       (:inherit 'bold :foreground yellow))
  (doom-modeline-error                 (:background red))
  (doom-modeline-buffer-major-mode     (:foreground green :bold t))
  (doom-modeline-info                  (:bold t :foreground teal))
  (doom-modeline-bar                   (:background yellow))
  (doom-modeline-panel                 (:background green :foreground white))

  (doom-modeline-project-dir           (:bold t :foreground peach))
  (doom-modeline-buffer-path           (:inherit 'bold :foreground red))
  (doom-modeline-buffer-file           (:inherit 'bold :foreground rosewater))
  (doom-modeline-buffer-modified       (:inherit 'bold :foreground yellow))
  (doom-modeline-error                 (:background black))
  (doom-modeline-buffer-major-mode     (:foreground green :bold t))
  (doom-modeline-info                  (:bold t :foreground peach))
  (doom-modeline-bar                   (:background green))
  (doom-modeline-panel                 (:background green :foreground white))

  ;;elfeed
  (elfeed-search-feed-face             (:foreground lavender))
  (elfeed-search-tag-face              (:foreground green))

  ;; message colors
  (message-header-name                  (:foreground gray-2))
  (message-header-other                 (:foreground peach))
  (message-header-subject               (:foreground yellow))
  (message-header-to                    (:foreground white))
  (message-header-cc                    (:foreground green))
  (message-header-xheader               (:foreground white))
  (custom-link                          (:foreground blue))
  (link                                 (:foreground blue))

  (org-date                             (:foreground magenta))
  (org-footnote                         (:foreground green))

  ;; which-key
  (which-key-key-face                   (:foreground green))
  (which-key-group-description-face     (:foreground red))
  (which-key-command-description-face   (:foreground blue))
  (which-key-local-map-description-face (:foreground yellow))

  ;; swiper
  (swiper-line-face                     (:foreground yellow))
  (counsel-outline-default              (:foreground yellow))
  (info-header-xref                     (:foreground yellow))
  (xref-file-header                     (:foreground yellow))

  ;; rainbow delimiter
  (rainbow-delimiters-base-error-face    (:foreground red))
  (rainbow-delimiters-base-face          (:foreground gray-2))
  (rainbow-delimiters-depth-1-face       (:foreground peach))
  (rainbow-delimiters-depth-2-face       (:foreground maroon))
  (rainbow-delimiters-depth-3-face       (:foreground pink))
  (rainbow-delimiters-depth-4-face       (:foreground blue))
  (rainbow-delimiters-depth-5-face       (:foreground lavender))
  (rainbow-delimiters-depth-6-face       (:foreground sky))
  (rainbow-delimiters-depth-7-face       (:foreground green))
  (rainbow-delimiters-depth-8-face       (:foreground yellow))
  (rainbow-delimiters-depth-9-face       (:foreground magenta))
  (rainbow-delimiters-mismatched-face    (:foreground red))
  (rainbow-delimiters-unmatched-face     (:foreground red))

  ;; counsel
  (company-preview-common               (:foreground red))
  (company-application-name             (:foreground yellow))
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
  (ivy-grep-info                                (:foreground teal))

  ;; lets support solaire mode
  (solaire-default-face (:background black-3))
  
  ;; evil
  (evil-ex-substitute-replacement (:foreground peach :strike-through nil :inherit 'evil-ex-substitute-matches))
  (evil-search-highlight-persist-highlight-face (:background yellow))

  ))

(provide-theme 'catppuccin)
