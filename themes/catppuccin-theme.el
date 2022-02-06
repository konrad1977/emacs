;;; commentary: catppuccin based theme


;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


(autothemer-deftheme
	catppuccin "A theme based on catppuccin's amazing color scheme"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (rosewater  "#F5E0DC" "#ffffff")
  (flamingo   "#F2CDCD" "#ffd7df")
  (mauve      "#DDB6F2" "#d7afd7")
  (pink       "#F5C2E7" "#d7afaf")
  (maroon     "#E8A2AF" "#ffafaf")
  (red        "#F28FAD" "#ff87af")
  (peach      "#F8BD96" "#ffaf87")
  (yellow     "#FAE3B0" "#ffd7af")
  (green      "#ABE9B3" "#afd7af")
  (teal       "#B5E8E0" "#afd7d7")
  (blue       "#96CDFB" "#afd7ff")
  (sky        "#89DCEB" "#afffff")
  (lavender   "#C9CBFF" "#d7d7ff")

  ;; Dark - monochrome:ish
  (dark       "#0C0A10" "#000000")
  (black      "#15121C" "#080808")
  (black-1    "#1B1923" "#080808")
  (black-2    "#1E1E28" "#121212")
  (black-3    "#332E41" "#303030")
  (black-4    "#575268" "#4e4e4e")
  (gray       "#6E6C7E" "#585858")
  (gray-1     "#988BA2" "#8a8a8a")
  (gray-2     "#C3BAC6" "#9e9e9e")
  (white      "#DFDEF1" "#eeeeee")
  (yellow-bg  "#383227" "#875f5f")

  )

 ;; Customize faces
  (
  (border                               (:background black-2 :foreground black))
  (button                               (:foreground green))
  (child-frame		                    (:background black :foreground black-1))
  (child-frame-border                   (:background black-2 :foreground black-1))
  (cursor                               (:background mauve :foreground black))
  (default								(:foreground rosewater :background black-2))
  (error                                (:foreground red))
  (fringe                               (:background black-2 :foreground gray))
  (glyph-face                           (:background red))
  (glyphless-char                       (:foreground gray))
  (header-line							(:background black))
  (highlight                            (:background black-4 :foreground lavender))
  (hl-line                              (:background black-1))
  (homoglyph                            (:foreground teal))
  (internal-border                      (:background black-2 :foreground black-2))
  (line-number                          (:foreground black-4 :background nil))
  (line-number-current-line             (:foreground mauve :background black-3 :bold t))
  (lv-separator                         (:foreground gray :background black-2))
  (match                                (:background yellow :foreground black))
  (menu                                 (:background black :foreground rosewater))
  (mode-line                            (:background black))
  (mode-line-inactive                   (:background nil :foreground gray-1 :bold nil))
  (mode-line-active		                (:background black :foreground rosewater :bold nil))
  (mode-line-highlight                  (:foreground flamingo))
  (mode-line-buffer-id                  (:foreground green :bold t))
  (numbers                              (:background green :foreground black))
  (region                               (:background rosewater :foreground black-2))
  (separator-line                       (:background black-2))
  (shadow                               (:background black))
  (success                              (:foreground green))
  (vertical-border                      (:foreground black-3 :background nil))
  (warning                              (:foreground maroon))
  (window-border                        (:background black-2 :foreground black-2))
  (window-divider-first-pixel           (:foreground black-2))
  (window-divider-last-pixel            (:background black-2))

  ;; Font lock
  (font-lock-type-face                  (:foreground lavender))
  (font-lock-regexp-grouping-backslash  (:foreground yellow))
  (font-lock-keyword-face               (:bold t :foreground maroon))
  (font-lock-warning-face               (:foreground red))
  (font-lock-constant-face              (:foreground lavender))
  (font-lock-string-face                (:foreground green))
  (font-lock-builtin-face               (:foreground yellow))
  (font-lock-reference-face				(:foreground sky))
  (font-lock-constant-face              (:foreground flamingo))
  (font-lock-function-name-face         (:foreground blue))
  (font-lock-variable-name-face         (:foreground mauve))
  (font-lock-negation-char-face         (:foreground red))
  (font-lock-comment-face               (:foreground gray))
  (font-lock-comment-delimiter-face     (:foreground gray))
  (font-lock-doc-face                   (:foreground gray))
  (font-lock-doc-markup-face            (:foreground gray))
  (font-lock-preprocessor-face	   		(:foreground gray))
  (elisp-shorthand-font-lock-face       (:foreground peach))

  (info-xref                            (:foreground yellow))
  (highlight-quoted-symbol              (:foreground maroon))
  (minibuffer-prompt-end                (:background dark :foreground peach))
  (minibuffer-prompt                    (:foreground peach :background black))
  (epa-mark                             (:foreground pink))
  (dired-mark                           (:foreground pink))

  (trailing-whitespace                  (:background black-4))

  (mode-line (:background black :foreground rosewater :bold t))
  
  ;; ;; Battery colors
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
  (doom-modeline-project-dir            (:bold t :foreground peach))
  (doom-modeline-bar                    (:bold t :background lavender))
  (doom-modeline-panel                  (:inherit 'bold :background flamingo :foreground black-2))

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
  (org-ellipsis                         (:foreground gray :bold t))
  (org-level-1                          (:foreground red :height 1.3 :bold t))
  (org-level-2                          (:foreground mauve :height 1.15 :bold t))
  (org-level-3                          (:foreground flamingo :height 1.05))
  (org-level-4                          (:foreground rosewater))
  (org-level-5                          (:foreground rosewater))
  (org-level-6                          (:foreground yellow))
  (org-level-7                          (:foreground peach))
  (org-level-8                          (:foreground maroon))

  ;; which-key
  (which-key-key-face                   (:inherit 'font-lock-variable-name-face))
  (which-func							(:inherit 'font-lock-function-name-face :bold t))
  (which-key-group-description-face     (:foreground pink))
  (which-key-command-description-face   (:foreground blue))
  (which-key-local-map-description-face (:foreground yellow))
  (which-key-posframe					(:background dark))
  (which-key-posframe-border			(:background dark))

  ;; swiper
  (swiper-line-face                     (:foreground yellow))
  (swiper-background-match-face-1       (:background peach :foreground black))
  (swiper-background-match-face-2       (:background blue :foreground black))
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
  (show-paren-match								(:background peach :foreground dark :bold t))
  (show-paren-match-expression					(:background peach :foreground dark :bold t))
  (show-paren-mismatch							(:background red :foreground white))

  (tooltip (:background black))

  ;; company-box  (company-tooltip            :inherit 'tooltip)
  (company-tooltip			  (:background black))
  (company-box				  (:background black))
  (company-tooltip-common     (:foreground yellow))
  (company-tooltip-search     (:background red :foreground black :distant-foreground rosewater))
  (company-tooltip-selection  (:background black-4 :foreground red :bold t))
  (company-tooltip-mouse      (:background red :foreground black :distant-foreground rosewater))
  (company-tooltip-annotation (:foreground green))
  (company-scrollbar-bg       (:inherit 'tooltip))
  (company-scrollbar-fg       (:background red))
  (company-preview            (:foreground yellow))
  (company-preview-common     (:background red :foreground black))
  (company-preview-search     (:inherit 'company-tooltip-search))
  (company-template-field     (:inherit 'match))

  (flycheck-posframe-background-face	(:background black))
  (flycheck-posframe-face				(:background black))
  (flycheck-posframe-info-face  		(:background black :foreground teal))
  (flycheck-posframe-warning-face  		(:background black :foreground yellow))
  (flycheck-posframe-error-face  		(:background black :foreground mauve))
  (flycheck-fringe-warning				(:foreground yellow :background black-2))
  (flycheck-fringe-error				(:foreground red :background black-2))
  (flycheck-fringe-info					(:foreground blue :background black-2))
  (flycheck-error-list-warning          (:foreground yellow :bold t))
  (flycheck-error-list-error            (:foreground red :bold t))
  (flycheck-error-list-info             (:foreground blue :bold t))

  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground black-3))
  (highlight-indent-guides-stack-character-face (:foreground black-3))
  (highlight-indent-guides-stack-odd-face       (:foreground black-3))
  (highlight-indent-guides-stack-even-face      (:foreground black-4))
  (highlight-indent-guides-stack-character-face (:foreground black-3))
  (highlight-indent-guides-even-face            (:foreground black-2))
  (highlight-indent-guides-odd-face             (:foreground black-4))

   ;;;; ivy
  (ivy-current-match                            (:background blue :foreground black :bold t))
  (ivy-action                                   (:background nil :foreground lavender))
  (ivy-grep-line-number                         (:background nil :foreground peach))
  (ivy-minibuffer-match-face-1                  (:background nil :foreground blue :bold t))
  (ivy-minibuffer-match-face-2                  (:background nil :foreground green))
  (ivy-minibuffer-match-highlight               (:foreground blue))
  (ivy-grep-info                                (:foreground blue))
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

  (tree-sitter-hl-face:constant					(:background red))
  (tree-sitter-hl-face:constant.builtin			(:background red))
  (tree-sitter-hl-face:function.call			(:background mauve))

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
  (dashboard-heading								(:foreground mauve :bold t))
  (dashboard-items-face								(:bold nil :foreground rosewater))
  (dashboard-banner-logo-title						(:bold t :height 200))
  (dashboard-no-items-face							(:foreground gray-2))

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

  (popup-face (:inherit 'tooltip))
  (popup-selection-face (:inherit 'tooltip))
  (popup-tip-face (:inherit 'tooltip))


  (anzu-match-1 (:foreground green :background black-2))
  (anzu-match-2 (:foreground yellow :background black-2))
  (anzu-match-3 (:foreground teal :background black-2))

  (anzu-mode-line		(:foreground black :background mauve))
  (anzu-mode-no-match	(:foreground rosewater :background red))
  (anzu-replace-to		(:foreground yellow :background yellow-bg))

  (ace-jump-face-background (:foreground gray))
  (ace-jump-face-foreground (:foreground red :background black :bold t))

  (vertico-current (:background black-3 :foreground mauve :bold t))

  (hydra-face-amaranth		(:foreground mauve))
  (hydra-face-blue			(:foreground blue))
  (hydra-face-pink			(:foreground pink))
  (hydra-face-red			(:foreground red))
  (hydra-face-teal			(:foreground teal))

  (centaur-tabs-active-bar-face				(:background black-2 :foreground rosewater))
  (centaur-tabs-selected					(:background black-2 :foreground rosewater :bold t))
  (centaur-tabs-selected-modified			(:background black-2 :foreground rosewater))
  (centaur-tabs-modified-marker-selected	(:background black-2 :foreground rosewater))
  (centaur-tabs-close-selected				(:inherit 'centaur-tabs-selected))

  (centaur-tabs-unselected					(:background black-1 :foreground gray-2))
  (centaur-tabs-unselected-modified			(:background black-1 :foreground mauve))
  (centaur-tabs-modified-marker-unselected	(:background black-1 :foreground gray-2))
  (centaur-tabs-close-unselected			(:background black-1 :foreground gray-2))

  (centaur-tabs-close-mouse-face			(:background nil :foreground red))
  (centaur-tabs-default						(:background black-1))
  (centaur-tabs-name-mouse-face				(:foreground blue :bold t))
 ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'catppuccin)
;;; catppuccin-theme.el ends here
