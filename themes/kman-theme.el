;;; commentary: kman based theme

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


(autothemer-deftheme
	kman "A dark KMan theme"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette

  (rosewater  "#f5e0dc" "#ffffff")
  (flamingo   "#f2cdcd" "#ffd7df")
  (pink       "#F8A5B5" "#d7afaf")
  (mauve      "#cba6f7" "#d7afd7")
  (red        "#D22B13" "#ff87af")
  (maroon     "#E47288" "#ffafaf")
  (peach      "#EB9969" "#ffaf87")
  (yellow     "#DCBB6D" "#ffd7af")
  (green      "#B6D189" "#87afaf")
  (teal       "#69C5B6" "#afd7d7")
  (sky        "#9BC5BE" "#afffff")
  (sapphire   "#74c7ec" "#afffff")
  (blue       "#9BB2C5" "#00d7ff")
  (lavender   "#b4befe" "#d7d7ff")
  (text       "#B4B6E0" "#ffffff")
  (subtext1   "#bac2de" "#ffffff")
  (subtext0   "#a6adc8" "#ffffff")
  (overlay2   "#9399b2" "#ffffff")
  (overlay1   "#7f849c" "#ffffff")
  (overlay0   "#6c7086" "#ffffff")
  (surface2   "#585b70" "#ffffff")
  (surface1   "#45475a" "#ffffff")
  (surface0   "#313244" "#ffffff")
  (base       "#181922" "#ffffff")
  (mantle     "#13141D" "#ffffff")
  (crust      "#11111b" "#ffffff"))

 ;; Customize faces
 (
  (default                              (:background base :foreground text))
  (border                               (:foreground overlay1))
  (bookmark-face                        (:foreground red))
  (button                               (:foreground blue))
  (child-frame                          (:foreground mantle))
  (child-frame-border                   (:foreground mantle))
  (cursor                               (:background blue :foreground crust))
  (error                                (:foreground red))
  (link                                 (:foreground blue))
  (fringe                               (:foreground surface0))
  (file-name-shadow                     (:foreground overlay2))
  (glyph-face                           (:background red :foreground overlay0))
  (glyphless-char                       (:foreground overlay2))
  (header-line                          (:background mantle :foreground maroon :weight 'semi-bold :italic t))
  (highlight                            (:background mantle :foreground blue :distant-foreground crust))
  (hl-line                              (:background surface0))
  (homoglyph                            (:foreground teal))
  (internal-border                      (:background base :foreground red))
  (line-number                          (:foreground surface1))
  (line-number-current-line             (:background surface0 :foreground mauve :bold t))
  (lv-separator                         (:background base :foreground red))
  (match                                (:background yellow :foreground crust))
  (menu                                 (:background base :foreground rosewater))
  (fill-column-indicator                (:foreground surface0))
  (mode-line                            (:background crust :foreground overlay2))
  (mode-line-inactive                   (:background surface0))
  (mode-line-active                     (:foreground subtext0 :background crust))
  (mode-line-highlight                  (:foreground flamingo))
  (mode-line-buffer-id                  (:foreground text :bold t))
  (numbers                              (:background peach))
  (region                               (:background text :foreground crust))
  (tooltip                              (:background overlay2 :foreground crust))
  (separator-line                       (:background base))
  (shadow                               (:foreground overlay2))
  (success                              (:foreground green))
  (vertical-border                      (:foreground surface0))
  (warning                              (:foreground yellow))
  (window-divider                       (:foreground base :distant-foreground mantle))

  (whitespace-newline                   (:foreground surface2))
  (whitespace-space                     (:foreground surface2))
  (whitespace-trailing                  (:foreground mantle :background red))

  ;; ;; Font lock
  (font-lock-keyword-face               (:foreground mauve :weight 'semi-bold))
  (font-lock-type-face                  (:foreground pink :weight 'semi-bold))
  (font-lock-regexp-grouping-backslash  (:foreground blue :weight 'semi-bold))
  (font-lock-warning-face               (:inherit 'warning))
  (font-lock-string-face                (:foreground green :italic t))
  (font-lock-builtin-face               (:foreground pink))
  (font-lock-reference-face				(:foreground sky))
  (font-lock-constant-face              (:foreground yellow :bold t))
  (font-lock-function-name-face         (:foreground blue))
  (font-lock-variable-name-face         (:foreground flamingo))
  (font-lock-negation-char-face         (:foreground red))
  (font-lock-comment-face               (:foreground overlay1 :italic t))
  (font-lock-comment-delimiter-face     (:foreground overlay2 :italic t))
  (font-lock-doc-face                   (:foreground overlay2))
  (font-lock-doc-markup-face            (:foreground overlay2))
  (font-lock-preprocessor-face	   		(:foreground overlay2))
  (elisp-shorthand-font-lock-face       (:foreground peach))

  (highlight-operators-face             (:foreground red))
  (highlight-quoted-symbol              (:foreground maroon))
  (highlight-numbers-face               (:foreground pink))
  (highlight-symbol-face                (:background surface2 :foreground text))
  (info-xref                            (:foreground yellow))

  (minibuffer-prompt-end                (:foreground red))
  (minibuffer-prompt                    (:foreground red))
  (epa-mark                             (:foreground pink))
  (dired-mark                           (:foreground pink))
  (dired-ignored                        (:background peach))

  (iedit-occurrence                     (:background blue :foreground crust))
  (iedit-read-only-occurrence           (:background green :foreground crust))
  
  (trailing-rosewaterspace              (:background surface1))

  ;; ;; Battery colors
  (doom-modeline-battery-critical       (:inherit 'error))
  (doom-modeline-battery-warning        (:inherit 'warning))
  (doom-modeline-battery-charging       (:foreground overlay2))
  (doom-modeline-battery-error          (:inherit 'eror))
  (doom-modeline-battery-normal         (:foreground overlay2))
  (doom-modeline-battery-full           (:foreground overlay1))

  ;; Doom visual state
  (doom-modeline-evil-motion-state      (:foreground teal))
  (doom-modeline-evil-emacs-state       (:foreground blue))
  (doom-modeline-evil-insert-state      (:foreground maroon))
  (doom-modeline-evil-normal-state      (:foreground subtext0))
  (doom-modeline-evil-visual-state      (:foreground sky))
  (doom-modeline-evil-replace-state     (:foreground red))
  (doom-modeline-evil-operator-state    (:foreground blue))

  (doom-modeline-project-dir            (:foreground overlay2))
  (doom-modeline-buffer-path            (:foreground overlay2))
  (doom-modeline-buffer-file            (:foreground text :bold t))
  (doom-modeline-buffer-major-mode      (:foreground maroon :bold t))
  (doom-modeline-buffer-modified        (:foreground text :italic t :bold t))
  (doom-modeline-error                  (:background red))
  (doom-modeline-info                   (:foreground subtext1 :bold t))
  (doom-modeline-time                   (:foreground overlay2 :weight 'semi-bold))
  (doom-modeline-project-dir            (:foreground blue))
  (doom-modeline-bar                    (:background yellow))
  (doom-modeline-bar-inactive           (:inherit 'mode-line-inactive :background base))
  (doom-modeline-panel                  (:background blue :foreground crust :bold t))
  (doom-modeline                        (:foreground overlay1))
  (doom-themes-visual-bell              (:background red))


  (telephone-line-accent-active         (:background surface0 :foreground subtext1))
  (telephone-line-accent-inactive       (:background surface0 :foreground subtext0))
  (telephone-line-evil-normal           (:background overlay1 :foreground crust :bold t))
  (telephone-line-evil-visual           (:background yellow :foreground crust :bold t))
  (telephone-line-evil-motion           (:background blue :foreground crust :bold t))
  (telephone-line-evil-insert           (:background green :foreground crust :bold t))

  ;;elfeed
  (elfeed-search-feed-face              (:foreground lavender))
  (elfeed-search-tag-face               (:foreground green))

  ;; message colors
  (message-header-name                  (:foreground overlay2))
  (message-header-other                 (:foreground peach))
  (message-header-subject               (:foreground yellow))
  (message-header-to                    (:foreground rosewater))
  (message-header-cc                    (:foreground green))
  (message-header-xheader               (:foreground rosewater))
  (custom-link                          (:foreground blue :underline t))
  
  ;; org-mode
  (org-done                             (:foreground overlay2))
  (org-code                             (:background crust))
  (org-meta-line                        (:background surface1 :foreground blue))
  (org-block                            (:background crust))
  (org-block-begin-line                 (:background crust :foreground overlay2))
  (org-block-end-line	                (:background crust :foreground overlay2))
  (org-headline-done                    (:foreground overlay2 :strike-through t))
  (org-todo                             (:foreground green :bold t))
  (org-headline-todo                    (:foreground base))
  (org-upcoming-deadline                (:foreground red))
  (org-footnote                         (:foreground green))
  (org-indent                           (:background base :foreground base))
  (org-hide	                            (:background base :foreground base))
  (org-date                             (:foreground overlay2))
  (org-ellipsis                         (:foreground overlay2 :bold t))
  (org-level-1                          (:foreground red :height 1.3 :bold t))
  (org-level-2                          (:foreground mauve :height 1.15 :bold t))
  (org-level-3                          (:foreground flamingo :height 1.05))
  (org-level-4                          (:foreground text))
  (org-level-5                          (:foreground text))
  (org-level-6                          (:foreground yellow))
  (org-level-7                          (:foreground peach))
  (org-level-8                          (:foreground maroon))

  ;; which-key
  (which-key-key-face                   (:inherit 'font-lock-variable-name-face))
  (which-func							(:inherit 'font-lock-function-name-face :bold t))
  (which-key-group-description-face     (:foreground pink))
  (which-key-command-description-face   (:foreground blue))
  (which-key-local-map-description-face (:foreground yellow))
  (which-key-posframe					(:background crust))
  (which-key-posframe-border			(:background crust))

  ;; swiper
  (swiper-line-face                     (:foreground yellow))
  (swiper-background-match-face-1       (:background peach :foreground crust))
  (swiper-background-match-face-2       (:background blue :foreground crust))
  (swiper-background-match-face-3       (:background flamingo :foreground crust))
  (swiper-background-match-face-4       (:background red :foreground crust))
  (swiper-match-face-1					(:inherit 'swiper-background-match-face-1))
  (swiper-match-face-2					(:inherit 'swiper-background-match-face-2))
  (swiper-match-face-3					(:inherit 'swiper-background-match-face-3))
  (swiper-match-face-4					(:inherit 'swiper-background-match-face-4))

  (counsel-outline-default              (:foreground yellow))
  (info-header-xref                     (:foreground yellow))
  (xref-file-header                     (:foreground yellow))
  (xref-match		                    (:foreground peach))

  ;; rainbow delimiter
  (rainbow-delimiters-mismatched-face   (:foreground crust :background red))
  (rainbow-delimiters-unmatched-face    (:foreground crust :background red))
  (rainbow-delimiters-base-error-face   (:foreground crust :background red))

  (rainbow-delimiters-base-face         (:foreground overlay2))

  (rainbow-delimiters-depth-1-face      (:foreground blue))
  (rainbow-delimiters-depth-2-face      (:foreground flamingo))
  (rainbow-delimiters-depth-3-face      (:foreground teal))
  (rainbow-delimiters-depth-4-face      (:foreground sky))
  (rainbow-delimiters-depth-5-face      (:foreground mauve))
  (rainbow-delimiters-depth-6-face      (:foreground sapphire))
  (rainbow-delimiters-depth-7-face      (:foreground green))
  (rainbow-delimiters-depth-8-face      (:foreground yellow))
  (rainbow-delimiters-depth-9-face      (:foreground sky))

  ;; show-paren
  (show-paren-match						(:background maroon :foreground crust :bold t))
  (show-paren-match-expression			(:background maroon :foreground crust :bold t))
  (show-paren-mismatch					(:background red))

  (company-tooltip                          (:background surface0 :foreground text))
  (company-tooltip-common                   (:foreground peach :distant-foreground crust :bold t))
  (company-tooltip-search                   (:foreground yellow))
  (company-tooltip-selection                (:background surface1 :bold t :underline t))
  (company-tooltip-mouse                    (:foreground crust :distant-foreground text))
  (company-tooltip-annotation               (:foreground overlay0 :distant-foreground yellow))
  (company-tooltip-scrollbar-track          (:background peach))
  (company-tooltip-scrollbar-thumb          (:background flamingo))
  (company-tooltip-quick-access             (:foreground surface0))
  (company-tooltip-quick-access-selection   (:foreground peach))
  (company-scrollbar-bg                     (:inherit 'tooltip))
  (company-scrollbar-fg                     (:background red))
  (company-preview                          (:foreground red))
  (company-preview-common                   (:background red :foreground crust))
  (company-preview-search                   (:inherit 'company-tooltip-search))
  (company-template-field                   (:inherit 'match))

  (markdown-hr-face                     (:foreground surface0))

  ;; Flycheck
  (flycheck-posframe-background-face    (:background crust))
  (flycheck-posframe-face               (:background crust))
  (flycheck-posframe-info-face          (:foreground blue :background "#1B2431" :height 160))
  (flycheck-posframe-warning-face       (:foreground "#FFF" :background "#2F3E56" :height 160 :weight 'semi-light))
  (flycheck-posframe-error-face         (:foreground "#FFF" :background "#2D1E28" :height 160 :weight 'semi-light))
  (flycheck-fringe-warning              (:inherit 'warning))
  (flycheck-fringe-error                (:inherit 'error))
  (flycheck-fringe-info                 (:inherit 'info ))
  (flycheck-error-list-warning          (:inherit 'warning :bold t))
  (flycheck-error-list-error            (:inheirt 'error :bold t))
  (flycheck-error-list-info             (:background blue :bold t))
  (flycheck-inline-error                (:background red :foreground red :height 128))
  (flycheck-inline-info                 (:background blue :foreground blue :height 128))
  (flycheck-inline-warning              (:background yellow :foreground yellow :height 128))

  ;; (periphery-mark-face-full (:foreground mantle :background surface1 :bold t))
  
  ;; (periphery-todo-face-full (:foreground mantle :background blue :bold t))
  ;; (periphery-todo-face (:foreground blue))
  
  ;; (periphery-fix-face (:foreground teal))
  ;; (periphery-fix-face-full (:foreground mantle :background teal :bold t))
  
  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground red :background red))
  (highlight-indent-guides-stack-character-face (:foreground red :background red))
  (highlight-indent-guides-top-character-face   (:foreground red :background red))
  (highlight-indent-guides-stack-odd-face       (:foreground red :background red))
  (highlight-indent-guides-stack-even-face      (:foreground red :background red))
  (highlight-indent-guides-even-face            (:foreground red :background red))
  (highlight-indent-guides-odd-face             (:foreground red :background red))
  (highlight-indent-guides-top-odd-face         (:foreground red :background red))
  (highlight-indent-guides-top-even-face        (:foreground red :background red))

   ;;;; ivy
  (ivy-current-match                            (:background blue :foreground crust :bold t))
  (ivy-action                                   (:background crust :foreground lavender))
  (ivy-grep-line-number                         (:background crust :foreground peach))
  (ivy-minibuffer-match-face-1                  (:background crust :foreground blue :bold t))
  (ivy-minibuffer-match-face-2                  (:background crust :foreground green))
  (ivy-minibuffer-match-highlight               (:foreground blue))
  (ivy-grep-info                                (:foreground blue))
  (ivy-grep-line-number                         (:foreground mauve))
  (ivy-confirm-face                             (:foreground green))

  (vertico-multiline                            (:background crust))
  (vertico-group-title                          (:foreground subtext1 :weight 'semi-bold :height 180))
  (vertico-group-separator                      (:foreground overlay1 :strike-through t))
  (vertico-current                              (:background surface0 :distant-foreground text :bold t :foreground base))

  (vertico-posframe-border                      (:background overlay0))
  (vertico-posframe                             (:background mantle :foreground subtext0))

  (corfu-annotations                            (:foreground overlay0))
  (corfu-current                                (:inherit 'vertico-current))
  (corfu-border                                 (:background surface1))
  (corfu-bar                                    (:background yellow))
  (corfu-default                                (:background crust :foreground text))
  (corfu-popupinfo                              (:background surface1 :foreground text :weight 'light :italic t))
  
  ;; posframe's
  (ivy-posframe                                 (:background surface2))
  (ivy-posframe-border                          (:inherit 'ivy-posframe))

  (orderless-match-face-0                       (:foreground pink :background "#211A1F" :weight 'semi-bold))
  (orderless-match-face-1                       (:foreground blue :background "#222d3e" :weight 'semi-bold))
  (orderless-match-face-2                       (:foreground yellow :background "#221F18" :weight 'semi-bold))
  (orderless-match-face-3                       (:foreground mauve :background "#292233" :weight 'semi-bold))

  (comint-highlight-prompt                      (:background peach :foreground crust))

  (completions-annotations                      (:foreground subtext0 :italic t))
  (completions-highlight                        (:foreground blue :italic t))
  (completions-common-part                      (:foreground yellow :distant-foreground crust :distant-background green :bold t :italic t))
  (completions-first-difference                 (:foreground red))
  (consult-file                                 (:foreground subtext0 :distant-foreground crust))

  (diff-added (:background green :foreground text))
  (diff-changed (:background yellow :foreground crust))
  
  (treemacs-directory-collapsed-face			(:foreground subtext0))
  (treemacs-directory-face						(:foreground subtext1))
  (treemacs-file-face							(:foreground subtext1))
  (treemacs-fringe-indicator-face               (:foreground red))

  (treemacs-git-added-face						(:foreground peach))
  (treemacs-git-renamed-face				   	(:foreground rosewater))
  (treemacs-git-ignored-face				   	(:foreground overlay2))
  (treemacs-git-unmodified-face		   			(:foreground text))
  (treemacs-git-renamed-face		   			(:foreground text))
  (treemacs-git-modified-face		   			(:foreground maroon))

  ;; lets support solaire mode
  (solaire-default-face (:background mantle))
  ;; lsp
  (lsp-headerline-breadcrumb-path-error-face (:underline (:color maroon :style 'wave)
                                                         :foreground overlay2 :background crust))

  (lsp-headerline-breadcrumb-path-face				(:background overlay0))
  (lsp-headerline-breadcrumb-path-hint-face	   		(:background crust))
  (lsp-headerline-breadcrumb-path-info-face	   		(:background overlay1))
  (lsp-headerline-breadcrumb-separator-face			(:background overlay2))
  (lsp-headerline-breadcrumb-symbols-face			(:background blue))
  (lsp-headerline-breadcrumb-project-prefix-face	(:background peach))
  (lsp-headerline-breadcrumb-symbols-error-face     (:foreground red))

  (lsp-ui-doc-background							(:background crust :foreground red))
  (lsp-ui-doc-header								(:background crust :foreground red))
  (lsp-ui-peek-filename								(:foreground teal))
  (lsp-ui-sideline-code-action			   			(:foreground yellow))
  (lsp-ui-sideline-current-symbol					(:foreground sky))
  (lsp-ui-sideline-symbol							(:foreground overlay1))

  ;; dashboard
  (dashboard-items-face								(:weight 'light :height 150))
  (dashboard-banner-logo-title						(:weight 'thin :height 320))
  (dashboard-heading								(:foreground subtext1 :weight 'thin :height 170))
  (dashboard-no-items-face							(:foreground overlay2))

  ;; all-the-icons
  (all-the-icons-dgreen							(:foreground green))
  (all-the-icons-green							(:foreground green))
  (all-the-icons-dpurple						(:foreground mauve))
  (all-the-icons-purple							(:foreground mauve))

  ;; evil
  (evil-ex-lazy-highlight           (:foreground crust :background yellow :bold t))
  (evil-ex-substitute-matches       (:foreground red :strike-through t))
  (evil-ex-substitute-replacement   (:foreground blue :bold t))
  (evil-search-highlight-persist-highlight-face (:background yellow))
  (evil-quickscope-first-face       (:foreground yellow :underline t))
  (evil-quickscope-second-face      (:foreground peach :underline t))
  (evil-goggles-default-face        (:background peach))
  (evil-goggles-join-face           (:foreground blue))
  (evil-goggles-delete-face         (:background red))
  (evil-goggles-paste-face          (:background green))
  (evil-goggles-indent-face         (:background subtext0))
  (evil-goggles-set-marker-face     (:foreground red :background red))
  (evil-goggles-yank-face           (:foreground blue :background blue))

  (ansi-color-crust (:background crust))

  (term (:background crust :foreground text))
  (term-color-blue (:background blue :foreground blue))
  (term-color-bright-blue (:inherit 'term-color-blue))
  (term-color-red (:background red :foreground red))
  (term-color-bright-red (:background maroon :foreground maroon))
  (term-color-yellow (:background yellow :foreground yellow))
  (term-color-bright-yellow (:background yellow :foreground yellow))

  (term-color-green (:background green :foreground green))
  (term-color-bright-green (:inherit 'term-color-green))

  (term-color-bright-crust (:background mantle :foreground red))
  (term-color-rosewater (:background text :foreground text))
  (term-color-bright-rosewater (:background rosewater :foreground rosewater))
  (term-color-cyan (:background sky :foreground sky))
  (term-color-bright-cyan (:background sky :foreground sky))
  (term-color-magenta (:background mauve :foreground mauve))
  (term-color-bright-magenta (:background mauve :foreground mauve))
  (term-underline (:background mauve :foreground blue))

  (vterm-color-crust (:background mantle :foreground mantle))
  (vterm-color-blue (:background blue :foreground blue))
  (vterm-color-cyan (:background sky :foreground sky))
  (vterm-color-green (:background green :foreground green))
  (vterm-color-magenta (:background maroon :foreground maroon))
  (vterm-color-yellow (:background peach :foreground yellow))
  (vterm-color-red (:background red :foreground red))
  (vterm-color-rosewater (:background text :foreground text))

  (popup-face (:inherit 'tooltip))
  (popup-selection-face (:inherit 'tooltip))
  (popup-tip-face (:inherit 'tooltip))

  (anzu-match-1 (:foreground green :background crust))
  (anzu-match-2 (:foreground yellow :background crust))
  (anzu-match-3 (:foreground teal :background crust))

  (anzu-mode-line		(:foreground crust :background mauve))
  (anzu-mode-no-match	(:foreground text :background red))
  (anzu-replace-to		(:foreground yellow :background surface2))

  (ace-jump-face-background (:foreground overlay2))
  (ace-jump-face-foreground (:foreground red :background crust :bold t))

  (hydra-face-amaranth		(:foreground mauve))
  (hydra-face-blue			(:foreground blue))
  (hydra-face-pink			(:foreground pink))
  (hydra-face-red			(:foreground red))
  (hydra-face-teal			(:foreground teal))

  ;; Bookmarks
  (bm-fringe-face                           (:background red :foreground crust))
  (bm-fringe-persistent-face                (:background red :foreground crust))

  (centaur-tabs-active-bar-face				(:background crust :foreground text))
  (centaur-tabs-selected					(:background crust :foreground text :bold t))
  (centaur-tabs-selected-modified			(:background crust :foreground text))
  (centaur-tabs-modified-marker-selected	(:background crust :foreground text))
  (centaur-tabs-close-selected				(:inherit 'centaur-tabs-selected))

  (centaur-tabs-unselected					(:background mantle :foreground overlay2))
  (centaur-tabs-unselected-modified			(:background mantle :foreground mauve))
  (centaur-tabs-modified-marker-unselected	(:background mantle :foreground overlay2))
  (centaur-tabs-close-unselected			(:background mantle :foreground overlay2))

  (centaur-tabs-close-mouse-face			(:foreground red))
  (centaur-tabs-default						(:background mantle))
  (centaur-tabs-name-mouse-face				(:foreground blue :bold t))

  (git-gutter:added                              (:foreground green))
  (git-gutter:deleted                            (:foreground red))
  (git-gutter:modified                           (:foreground blue))

  (goggles-added (:background green))
  (goggles-changed (:background blue))
  (goggles-removed (:background red))
  
  ;; Tree sitter highlightning
  (tree-sitter-hl-face:function                  (:inherit 'font-lock-function-name-face))
  (tree-sitter-hl-face:function.call             (:inherit 'tree-sitter-hl-face:function))
  (tree-sitter-hl-face:function.builtin          (:foreground sky))
  (tree-sitter-hl-face:function.special          (:foreground text :italic t :bold t))
  (tree-sitter-hl-face:function.macro            (:foreground sapphire))
  (tree-sitter-hl-face:function.label            (:foreground yellow))

  (tree-sitter-hl-face:method                    (:inherit 'tree-sitter-hl-face:function))
  (tree-sitter-hl-face:method.call               (:inherit 'tree-sitter-hl-face:method))

  (tree-sitter-hl-face:type                      (:inherit 'font-lock-type-face))
  (tree-sitter-hl-face:type.parameter            (:foreground pink :italic t))
  (tree-sitter-hl-face:type.argument             (:foreground subtext0))
  (tree-sitter-hl-face:type.builtin              (:inherit 'font-lock-builtin-face))
  (tree-sitter-hl-face:type.super                (:foreground maroon :bold t))
  (tree-sitter-hl-face:constructor               (:foreground teal :weight 'semi-bold))

  (tree-sitter-hl-face:variable                  (:inherit 'font-lock-variable-name-face))
  (tree-sitter-hl-face:variable.parameter        (:inherit 'tree-sitter-hl-face:type.parameter))
  (tree-sitter-hl-face:variable.builtin          (:foreground blue :italic t))
  (tree-sitter-hl-face:variable.special          (:foreground mauve :italic t))
  (tree-sitter-hl-face:variable.synthesized      (:foreground pink :italic t))
  (tree-sitter-hl-face:property                  (:foreground rosewater))
  (tree-sitter-hl-face:property.definition       (:inherit 'tree-sitter-hl-face:property :italic t))

  (tree-sitter-hl-face:comment                   (:inherit 'font-lock-comment-face))
  (tree-sitter-hl-face:doc                       (:inherit 'font-lock-comment-face))
  (tree-sitter-hl-face:string                    (:inherit 'font-lock-string-face))
  (tree-sitter-hl-face:string.special            (:inherit 'font-lock-string-face))
  (tree-sitter-hl-face:escape                    (:inherit 'font-lock-regexp-grouping-backslash))
  (tree-sitter-hl-face:embedded                  (:foreground teal))

  (tree-sitter-hl-face:keyword                   (:inherit 'font-lock-keyword-face))
  (tree-sitter-hl-face:operator                  (:foreground sapphire))
  (tree-sitter-hl-face:label                     (:foreground overlay2))
  (tree-sitter-hl-face:constant                  (:inherit 'font-lock-constant-face))
  (tree-sitter-hl-face:constant.builtin          (:inherit 'font-lock-constant-face :weight 'normal))
  (tree-sitter-hl-face:number                    (:foreground peach))

  (tree-sitter-hl-face:punctuation               (:foreground red :weight 'semi-bold))
  (tree-sitter-hl-face:punctuation.bracket       (:foreground blue :weight 'semi-bold))
  (tree-sitter-hl-face:punctuation.delimiter     (:foreground text :weight 'semi-bold))
  (tree-sitter-hl-face:punctuation.special       (:foreground maroon :weight 'semi-bold))

  (tree-sitter-hl-face:case-pattern              (:foreground peach))
  (tree-sitter-hl-face:keyword.compiler          (:foreground overlay2 :bold t :italic t))

  ;; ;; Custom for pinkus tree-sitter-swift
  (tree-sitter-hl-face:include                   (:foreground subtext0 :italic t :bold t))
  (tree-sitter-hl-face:parameter                 (:foreground subtext0 :italic t))
  (tree-sitter-hl-face:repeat                    (:foreground teal))
  (tree-sitter-hl-face:boolean                   (:inherit 'font-lock-constant-face))
  (tree-sitter-hl-face:keyword.return            (:inherit 'tree-sitter-hl-face:keyword :italic t))
  (tree-sitter-hl-face:keyword.operator          (:foreground sapphire :bold t))
  (tree-sitter-hl-face:keyword.function          (:inherit 'tree-sitter-hl-face:keyword))
  (tree-sitter-hl-face:conditional               (:inherit 'tree-sitter-hl-face:keyword :weight 'semi-bold))

  (swift-mode:preprocessor-keyword-face (:foreground text :italic t))
  (swift-mode:property-access-face (:foreground subtext1))
  (swift-mode:builtin-property-face (:foreground rosewater))
  (swift-mode:builtin-enum-case-face (:foreground teal))
  (swift-mode:builtin-method-trailing-closure-face (:foreground teal))
  (swift-mode:builtin-function-trailing-closure-face (:foreground teal))
  (swift-mode:function-call-face (:foreground pink))
  
  ;; (localizeable-variable-face (:inherit font-lock-string-face))
  ;; (localizeable-value-face (:inherit font-lock-keyword-face))

 ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kman)
;;; kman-theme.el ends here
