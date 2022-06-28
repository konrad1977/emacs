;;; commentary: amarena based theme

(require 'autothemer)

;;; code:

(autothemer-deftheme
amarena "A theme based on amarena amazing color scheme"

 ((((class color) (min-colors #xFFFFFF))) ;; We're only concerned with graphical Emacs

;;   *background:	#1A2026
;; *foreground:	#FFFFFF
;; *cursorColor:	#FFFFFF
;; *color0:	#242D35
;; *color1:	#FB6396
;; *color2:	#94CF95
;; *color3:	#F692B2
;; *color4:	#6EC1D6
;; *color5:	#CD84C8
;; *color6:	#7FE4D2
;; *color7:	#FFFFFF
;; *color8:	#526170
;; *color9:	#F92D72
;; *color10:	#6CCB6E
;; *color11:	#F26190
;; *color12:	#4CB9D6
;; *color13:	#C269BC
;; *color14:	#58D6BF
;; *color15:	#F4F5F2
  
  ;; Define our color palette
  (rosewater  "#F5E0DC")
  (flamingo   "#F2CDCD")
  (mauve      "#DDB6F2")
  (pink       "#F5C2E7")
  (maroon     "#E8A2AF")
  (red        "#F28FAD")
  (peach      "#F8BD96")
  (yellow     "#FAE3B0")
  (green      "#6ccb6e")
  (teal       "#B5E8E0")
  (blue       "#96CDFB")
  (sky        "#89DCEB")
  (lavender   "#C9CBFF")

  ;; Dark - monochrome:ish
  (dark       "#0C0A10")
  (black      "#15121C")
  (black-1    "#1B1923")
  (black-2    "#1a2026")
  (black-3    "#332E41")
  (black-4    "#575268")
  (gray       "#6E6C7E")
  (gray-1     "#988BA2")
  (gray-2     "#C3BAC6")
  (white      "#DFDEF1")
  )

 ;; Customize faces
  (
  (border                               (:background black-2 :foreground black))
  (button                               (:foreground green))
  (child-frame		                    (:background black :foreground black-1))
  (child-frame-border                   (:background black-2 :foreground black-1))
  (cursor                               (:background peach :foreground black-1))
  (default								(:foreground rosewater :background black-2))
  (error                                (:foreground red))
  (fringe                               (:background black-2 :foreground gray-1))
  (glyph-face                           (:background red))
  (glyphless-char                       (:foreground gray))
  (header-line							(:background black))
  (highlight                            (:background black-4 :foreground lavender))
  (hl-line                              (:background black-1))
  (homoglyph                            (:foreground teal))
  (internal-border                      (:background black-2 :foreground black-2))
  (line-number                          (:foreground black-4 :background nil))
  (line-number-current-line             (:foreground green :background black-3))
  (lv-separator                         (:foreground gray :background black-2))
  (match                                (:background green :foreground black))
  (menu                                 (:background black :foreground rosewater))
  (mode-line                            (:background black))
  (mode-line-highlight                  (:foreground rosewater))
  (mode-line-inactive                   (:background black-2 :foreground gray-1))
  (numbers                              (:background green :foreground black))
  (region                               (:background peach :foreground black-1))
  (separator-line                       (:background black-2))
  (shadow                               (:background black))
  (success                              (:foreground green))
  (vertical-border                      (:foreground black-3 :background nil))
  (warning                              (:foreground maroon))
  (window-border                        (:background black-2 :foreground black-2))
  (window-divider-first-pixel           (:foreground black-2))
  (window-divider-last-pixel            (:background black-2))

  ;; Font lock
  (font-lock-type-face                  (:foreground teal))
  (font-lock-constant-face              (:foreground blue))
  (font-lock-keyword-face               (:bold t :foreground maroon))
  (font-lock-constant-face              (:foreground lavender))
  (font-lock-string-face                (:foreground green))
  (font-lock-builtin-face               (:foreground yellow))
  (font-lock-function-name-face         (:foreground blue))
  (font-lock-variable-name-face         (:foreground peach))
  (font-lock-comment-face               (:foreground gray))
  (font-lock-comment-delimiter-face     (:foreground gray))
  (font-lock-doc-face                   (:foreground gray))
  (font-lock-doc-markup-face            (:foreground gray))
  (font-lock-warning-face               (:foreground maroon))
  (font-lock-negation-char-face         (:foreground red))
  (font-lock-regexp-grouping-backslash  (:foreground teal))
  (elisp-shorthand-font-lock-face       (:foreground yellow))
  (font-lock-reference-face				(:foreground yellow))
  (font-lock-preprocessor-face	   		(:foreground gray))

  (info-xref                            (:foreground yellow))
  (highlight-quoted-symbol              (:foreground maroon))
  (minibuffer-prompt-end                (:background dark :foreground peach))
  (minibuffer-prompt                    (:foreground peach :background black))
  (epa-mark                             (:foreground pink))
  (dired-mark                           (:foreground pink))

  (trailing-whitespace                  (:background black-3))

  ;; Battery colors
  (doom-modeline-battery-critical       (:foreground red))
  (doom-modeline-battery-warning        (:foreground maroon))
  (doom-modeline-battery-charging       (:foreground gray-2))
  (doom-modeline-battery-error          (:foreground red))
  (doom-modeline-battery-normal         (:foreground gray-2))
  (doom-modeline-battery-full           (:foreground green))

  ;; Doom visual state
  (doom-modeline-evil-motion-state      (:foreground teal))
  (doom-modeline-evil-emacs-state       (:foreground blue))
  (doom-modeline-evil-insert-state      (:foreground mauve))
  (doom-modeline-evil-normal-state      (:foreground rosewater))
  (doom-modeline-evil-visual-state      (:foreground sky))
  (doom-modeline-evil-replace-state     (:foreground red))
  (doom-modeline-evil-operator-state    (:foreground blue))

  (doom-modeline-project-dir            (:bold t :foreground green))
  (doom-modeline-buffer-path            (:inherit 'bold :foreground green))
  (doom-modeline-buffer-file            (:inherit 'bold :foreground maroon))
  (doom-modeline-buffer-modified        (:inherit 'bold :foreground yellow))
  (doom-modeline-error                  (:background red))
  (doom-modeline-buffer-major-mode      (:foreground green :bold t))
  (doom-modeline-info                   (:bold t :foreground teal))
  (doom-modeline-bar                    (:background yellow))
  (doom-modeline-panel                  (:background green :foreground white))
  (doom-modeline-project-dir            (:bold t :foreground peach))
  (doom-modeline-buffer-path            (:inherit 'bold :foreground red))
  (doom-modeline-buffer-file            (:inherit 'bold :foreground rosewater))
  (doom-modeline-buffer-modified        (:inherit 'bold :foreground yellow))
  (doom-modeline-error                  (:background black))
  (doom-modeline-buffer-major-mode      (:foreground green :bold t))
  (doom-modeline-info                   (:bold t :foreground peach))
  (doom-modeline-bar                    (:background green))
  (doom-modeline-panel                  (:background green :foreground white))

  ;;elfeed
  (elfeed-search-feed-face              (:foreground lavender))
  (elfeed-search-tag-face               (:foreground green))

  ;; message colors
  (message-header-name                  (:foreground gray-2))
  (message-header-other                 (:foreground peach))
  (message-header-subject               (:foreground yellow))
  (message-header-to                    (:foreground white))
  (message-header-cc                    (:foreground green))
  (message-header-xheader               (:foreground white))
  (custom-link                          (:foreground blue))
  (link                                 (:foreground blue))

  ;; org-mode
  (org-done                             (:foreground gray-1))
  (org-code                             (:background black))
  (org-block                            (:background black))
  (org-block-begin-line                 (:background black :foreground gray-1))
  (org-block-end-line	                (:background black :foreground gray-1))
  (org-headline-done                    (:foreground gray-1 :strike-through t))
  (org-todo                             (:foreground green :bold t))
  (org-headline-todo                    (:foreground black-2))
  (org-upcoming-deadline                (:foreground red))
  (org-footnote                         (:foreground green))
  (org-indent                           (:background black-2 :foreground black-2))
  (org-hide	                            (:background black-2 :foreground black-2))
  (org-date                             (:foreground gray))
  (org-level-1                          (:foreground red :height 1.2 :bold t))
  (org-level-2                          (:foreground mauve :height 1.1))
  (org-level-3                          (:foreground rosewater))
  (org-level-4                          (:foreground gray-2))
  (org-level-5                          (:foreground lavender))
  (org-level-6                          (:foreground yellow))
  (org-level-7                          (:foreground peach))
  (org-level-8                          (:foreground maroon))

  ;; which-key
  (which-key-key-face                   (:foreground green :bold t))
  (which-key-group-description-face     (:foreground pink))
  (which-key-command-description-face   (:foreground blue))
  (which-key-local-map-description-face (:foreground yellow))
  (which-key-posframe					(:background dark))
  (which-key-posframe-border			(:background dark))

  ;; swiper
  (swiper-line-face                     (:foreground yellow))
  (swiper-background-match-face-1       (:background peach :foreground black))
  (swiper-background-match-face-2       (:background mauve :foreground black))
  (swiper-background-match-face-3       (:background flamingo :foreground black))
  (swiper-background-match-face-4       (:background red :foreground black))
  (swiper-match-face-1					(:inherit 'swiper-background-match-face-1))
  (swiper-match-face-2					(:inherit 'swiper-background-match-face-2))
  (swiper-match-face-3					(:inherit 'swiper-background-match-face-3))
  (swiper-match-face-4					(:inherit 'swiper-background-match-face-4))

  (counsel-outline-default              (:foreground yellow))
  (info-header-xref                     (:foreground yellow))
  (xref-file-header                     (:foreground yellow))
  (xref-match		                    (:foreground peach))

  ;; rainbow delimiter
  (rainbow-delimiters-mismatched-face   (:foreground red))
  (rainbow-delimiters-unmatched-face    (:foreground green))
  (rainbow-delimiters-base-error-face   (:foreground red))
  (rainbow-delimiters-base-face         (:foreground gray-2))
  (rainbow-delimiters-depth-1-face      (:foreground lavender))
  (rainbow-delimiters-depth-2-face      (:foreground peach))
  (rainbow-delimiters-depth-3-face      (:foreground blue))
  (rainbow-delimiters-depth-4-face      (:foreground maroon))
  (rainbow-delimiters-depth-5-face      (:foreground green))
  (rainbow-delimiters-depth-6-face      (:foreground yellow))
  (rainbow-delimiters-depth-7-face      (:foreground pink))
  (rainbow-delimiters-depth-8-face      (:foreground teal))
  (rainbow-delimiters-depth-9-face      (:foreground mauve))

  ;; show-paren
  (show-paren-match								(:background teal :foreground black))
  (show-paren-match-expression					(:background black))
  (show-paren-mismatch							(:background red :foreground white))

										; company-box
  (company-echo									(:background red))
  (company-echo-common							(:foreground red))
  (company-preview								(:background red))
  (company-preview-common						(:foreground rosewater))
  (company-preview-search						(:background yellow))
  (company-template-field						(:foreground dark :background red))
  (company-tooltip								(:foreground rosewater))
  (company-tooltip-annotation					(:foreground green))
  (company-tooltip-annotation-selection			(:inherit 'company-tooltip-annotation))
  (company-tooltip-common						(:foreground blue :underline t))
  (company-tooltip-common-selection				(:background peach :foreground black :underline t :bold t))
  (company-tooltip-scrollbar-thumb				(:background red :foreground red))
  (company-tooltip-scrollbar-track				(:background yellow :foreground yellow))
  (company-tooltip-selection					(:background blue :foreground black))

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
  (ivy-grep-line-number                         (:foreground mauve))
  (ivy-confirm-face                             (:foreground green))

  ;; posframe's
  (ivy-posframe                                 (:background black))
  (ivy-posframe-border                          (:inherit 'ivy-posframe))

  (treemacs-directory-collapsed-face			(:foreground rosewater))
  (treemacs-directory-face						(:foreground rosewater))
  (treemacs-file-face							(:foreground rosewater))

  (treemacs-git-added-face						(:foreground peach))
  (treemacs-git-renamed-face				   	(:foreground rosewater))
  (treemacs-git-ignored-face				   	(:foreground gray-2))
  (treemacs-git-unmodified-face		   			(:foreground rosewater))
  (treemacs-git-renamed-face		   			(:foreground rosewater))
  (treemacs-git-modified-face		   			(:foreground maroon))

  ;; lets support solaire mode
  (solaire-default-face (:background black-1))
  ;; lsp
  (lsp-headerline-breadcrumb-path-error-face (:underline (:color maroon :style 'wave)
                                                         :foreground gray-2 :background black))
  (lsp-headerline-breadcrumb-path-face				(:background black))
  (lsp-headerline-breadcrumb-path-hint-face	   		(:background black))
  (lsp-headerline-breadcrumb-path-info-face	   		(:background black))
  (lsp-headerline-breadcrumb-separator-face			(:background black))
  (lsp-headerline-breadcrumb-symbols-face			(:background black))
  (lsp-headerline-breadcrumb-project-prefix-face	(:background black))
  (lsp-headerline-breadcrumb-symbols-error-face     (:foreground red))

  (lsp-ui-doc-background							(:background black :foreground red))
  (lsp-ui-doc-header								(:background black :foreground red))
  (lsp-ui-doc-border								(:background nil :foreground nil))
  (lsp-ui-peek-filename								(:foreground teal))
  (lsp-ui-sideline-code-action			   			(:foreground yellow))
  (lsp-ui-sideline-current-symbol					(:foreground sky))
  (lsp-ui-sideline-symbol							(:foreground gray-1))

  ;; dashboard
  (dashboard-heading							(:foreground maroon :bold t))

  ;; all-the-icons
  (all-the-icons-dgreen							(:foreground green))
  (all-the-icons-green							(:foreground green))
  (all-the-icons-dpurple						(:foreground mauve))
  (all-the-icons-purple							(:foreground mauve))

  ;; evil
  (evil-ex-substitute-replacement (:foreground peach :strike-through nil :inherit 'evil-ex-substitute-matches))
  (evil-search-highlight-persist-highlight-face (:background yellow))

  (term (:background dark :foreground dark))
  (term-color-blue (:background blue :foreround blue))
  (term-color-bright-blue (:inherit 'term-color-blue))
  (term-color-green (:background green :foreround green))
  (term-color-bright-green (:inherit 'term-color-green))
  (term-color-black (:background dark :foreground black))
  (term-color-bright-black (:background black-1 :foreground black-1))
  (term-color-white (:background rosewater :foreground rosewater))
  (term-color-bright-white (:background white :foreground white))
  (term-color-red (:background red :foreground red))
  (term-color-bright-red (:background maroon :foreground maroon))
  (term-color-yellow (:background yellow :foreground yellow))
  (term-color-bright-yellow (:background yellow :foreground yellow))
  (term-color-cyan (:background sky :foreground sky))
  (term-color-bright-cyan (:background sky :foreground sky))
  (term-color-magenta (:background mauve :foreground mauve))
  (term-color-bright-magenta (:background mauve :foreground mauve))

  (popup-face (:background black :foreground rosewater))
  (popup-menu-face (:background black :foreground rosewater))
  (popup-menu-mouse-face (:background blue :foreground black :bolt t))
  (popup-menu-selection-face (:background flamingo :foreground black))
  (popup-isearch-match (:background mauve :foreground black :bold t))
  
))

(provide-theme 'amarena)
;;; amarena-theme.el ends here
