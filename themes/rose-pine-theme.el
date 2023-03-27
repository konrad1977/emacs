;;; package --- A beutiful theme
;;; Commentary:
;;; rose-pine based theme


;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


(autothemer-deftheme
	rose-pine "A theme based on rose-pine's amazing color scheme"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (highlight-high   "#524f67" "#d7d7ff")
  (highlight-med    "#403d52" "#d7d7ff")
  (highlight-low    "#21202E" "#d7d7ff")
  (iris             "#c4a7e7" "#d7d7ff")
  (foam             "#9ccfd8" "#00d7ff")
  (foam-low         "#272B37" "#00d7ff")
  (pine             "#31748f" "#afffff")
  (rose             "#ebbcba" "#ffffff")
  (rose-low         "#302935" "#ffffff")
  (gold             "#f6c177" "#ffd7af")
  (gold-low         "#312A2E" "#ffd7af")
  (love             "#eb6f92" "#ff87af")
  (text             "#e0def4" "#ffffff")
  (subtle           "#908caa" "#ffffff")
  (muted            "#6e6a86" "#ffffff")
  (overlay          "#26233a" "#ffffff")
  (surface          "#1F1D2E" "#ffffff")
  (base             "#191724" "#ffffff")
  (ex               "#16141f" "#F00000"))

 ;; Customize faces
 (
  (default                              (:background surface :foreground text))
  (border                               (:foreground muted))
  (bookmark-face                        (:foreground love))
  (button                               (:foreground foam))
  (child-frame                          (:foreground base))
  (child-frame-border                   (:foreground base))
  (cursor                               (:background gold :foreground base))
  (error                                (:foreground love))
  (link                                 (:foreground iris))
  (fringe                               (:foreground overlay))
  (file-name-shadow                     (:foreground muted))
  (glyph-face                           (:background love :foreground muted))
  (glyphless-char                       (:foreground muted))
  (header-line                          (:background base :foreground rose :weight 'semi-bold :italic t))
  (highlight                            (:background gold :foreground base :distant-foreground base))
  (hl-line                              (:background overlay))
  (homoglyph                            (:foreground foam))
  (line-number                          (:foreground highlight-high))
  (line-number-current-line             (:background overlay :foreground iris :bold t))
  (match                                (:background gold :foreground base))
  (menu                                 (:foreground rose))
  (fill-column-indicator                (:foreground overlay))
  (mode-line                            (:background base :foreground muted))
  (mode-line-inactive                   (:background overlay))
  (mode-line-active                     (:foreground muted :background base))
  (mode-line-highlight                  (:foreground rose))
  (mode-line-buffer-id                  (:foreground text :bold t))
  (numbers                              (:background gold))
  (region                               (:background text :foreground highlight-med))
  (tooltip                              (:background muted :foreground base))
  (shadow                               (:foreground muted))
  (success                              (:foreground pine))
  (vertical-border                      (:foreground overlay))
  (warning                              (:foreground gold))
  (window-divider                       (:foreground surface :distant-foreground base))

  (whitespace-newline                   (:foreground muted))
  (whitespace-space                     (:foreground muted))
  (whitespace-trailing                  (:foreground base :background love))

  ;; ;; Font lock
  (font-lock-keyword-face               (:foreground iris :weight 'semi-bold))
  (font-lock-type-face                  (:foreground love :weight 'semi-bold))
  (font-lock-builtin-face               (:foreground iris))
  (font-lock-function-name-face         (:foreground pine))
  (font-lock-variable-name-face         (:foreground text))
  (font-lock-comment-face               (:foreground muted :italic t))
  (font-lock-doc-face                   (:foreground muted))
  (font-lock-string-face                (:foreground gold :italic t))
  (font-lock-warning-face               (:foreground gold))
  (font-lock-constant-face              (:foreground rose))
  (font-lock-regexp-grouping-backslash  (:foreground foam :weight 'semi-bold))
  
  (font-lock-reference-face				(:foreground foam))
  (font-lock-negation-char-face         (:foreground love))
  (font-lock-comment-delimiter-face     (:foreground muted :italic t))
  (font-lock-doc-markup-face            (:foreground muted))
  (font-lock-preprocessor-face	   		(:foreground muted))
  
  (elisp-shorthand-font-lock-face       (:foreground gold))

  (highlight-operators-face             (:foreground love))
  (highlight-quoted-symbol              (:foreground rose))
  (highlight-numbers-face               (:foreground love))
  (highlight-symbol-face                (:background highlight-med :foreground subtle))
  (info-xref                            (:foreground gold))

  (minibuffer-prompt-end                (:foreground love))
  (minibuffer-prompt                    (:foreground love))
  (epa-mark                             (:foreground love))
  (dired-mark                           (:foreground love))
  (dired-ignored                        (:background gold))

  (iedit-occurrence                     (:background foam :foreground base))
  (iedit-read-only-occurrence           (:background pine :foreground base))
  
  (trailing-rosewaterspace              (:background overlay))

  ;; ;; Battery colors
  (doom-modeline-battery-critical       (:inherit 'error))
  (doom-modeline-battery-warning        (:inherit 'warning))
  (doom-modeline-battery-charging       (:foreground muted))
  (doom-modeline-battery-error          (:inherit 'eror))
  (doom-modeline-battery-normal         (:foreground muted))
  (doom-modeline-battery-full           (:foreground muted))

  ;; Doom visual state
  (doom-modeline-evil-motion-state      (:foreground foam))
  (doom-modeline-evil-emacs-state       (:foreground foam))
  (doom-modeline-evil-insert-state      (:foreground rose))
  (doom-modeline-evil-normal-state      (:foreground muted))
  (doom-modeline-evil-visual-state      (:foreground foam))
  (doom-modeline-evil-replace-state     (:foreground love))
  (doom-modeline-evil-operator-state    (:foreground foam))

  (doom-modeline-project-dir            (:foreground muted))
  (doom-modeline-buffer-path            (:foreground muted))
  (doom-modeline-buffer-file            (:foreground text :bold t))
  (doom-modeline-buffer-major-mode      (:foreground rose :bold t))
  (doom-modeline-buffer-modified        (:foreground text :italic t :bold t))
  (doom-modeline-error                  (:background love))
  (doom-modeline-info                   (:foreground subtle :bold t))
  (doom-modeline-time                   (:foreground muted :weight 'semi-bold))
  (doom-modeline-project-dir            (:foreground foam))
  (doom-modeline-bar                    (:background gold))
  (doom-modeline-bar-inactive           (:background overlay))
  (doom-modeline-panel                  (:background foam :foreground base :bold t))
  (doom-modeline                        (:foreground muted))
  (doom-themes-visual-bell              (:background love))


  (telephone-line-accent-active         (:background overlay :foreground subtle))
  (telephone-line-accent-inactive       (:background overlay :foreground muted))
  (telephone-line-evil-normal           (:background muted :foreground base :bold t))
  (telephone-line-evil-visual           (:background gold :foreground base :bold t))
  (telephone-line-evil-motion           (:background foam :foreground base :bold t))
  (telephone-line-evil-insert           (:background pine :foreground base :bold t))

  ;;elfeed
  (elfeed-search-feed-face              (:foreground iris))
  (elfeed-search-tag-face               (:foreground foam))

  ;; message colors
  (message-header-name                  (:foreground muted))
  (message-header-other                 (:foreground gold))
  (message-header-subject               (:foreground gold))
  (message-header-to                    (:foreground rose))
  (message-header-cc                    (:foreground pine))
  (message-header-xheader               (:foreground rose))
  (custom-link                          (:foreground foam :underline t))
  
  ;; org-mode
  (with-eval-after-load "org"
    (org-done                             (:foreground muted))
    (org-code                             (:background base))
    (org-meta-line                        (:background overlay :foreground foam))
    (org-block                            (:background base))
    (org-block-begin-line                 (:background base :foreground muted))
    (org-block-end-line	                (:background base :foreground muted))
    (org-headline-done                    (:foreground muted :strike-through t))
    (org-todo                             (:foreground pine :bold t))
    (org-headline-todo                    (:foreground surface))
    (org-upcoming-deadline                (:foreground love))
    (org-footnote                         (:foreground pine))
    (org-indent                           (:foreground surface))
    (org-hide	                            (:foreground surface))
    (org-date                             (:foreground muted))
    (org-ellipsis                         (:foreground muted :bold t))
    (org-level-1                          (:foreground love :height 1.3 :bold t))
    (org-level-2                          (:foreground iris :height 1.15 :bold t))
    (org-level-3                          (:foreground rose :height 1.05))
    (org-level-4                          (:foreground text))
    (org-level-5                          (:foreground text))
    (org-level-6                          (:foreground gold))
    (org-level-7                          (:foreground gold))
    (org-level-8                          (:foreground rose)))

  ;; which-key
  (which-key-key-face                   (:inherit 'font-lock-variable-name-face))
  (which-func							(:inherit 'font-lock-function-name-face :bold t))
  (which-key-group-description-face     (:foreground love))
  (which-key-command-description-face   (:foreground foam))
  (which-key-local-map-description-face (:foreground gold))
  (which-key-posframe					(:background base))
  (which-key-posframe-border			(:background base))

  ;; swiper
  (swiper-line-face                     (:foreground gold))
  (swiper-background-match-face-1       (:background gold :foreground base))
  (swiper-background-match-face-2       (:background foam :foreground base))
  (swiper-background-match-face-3       (:background rose :foreground base))
  (swiper-background-match-face-4       (:background love :foreground base))
  (swiper-match-face-1					(:inherit 'swiper-background-match-face-1))
  (swiper-match-face-2					(:inherit 'swiper-background-match-face-2))
  (swiper-match-face-3					(:inherit 'swiper-background-match-face-3))
  (swiper-match-face-4					(:inherit 'swiper-background-match-face-4))

  (counsel-outline-default              (:foreground gold))
  (info-header-xref                     (:foreground gold))
  (xref-file-header                     (:foreground gold))
  (xref-match		                    (:foreground gold))

  ;; rainbow delimiter
  (rainbow-delimiters-mismatched-face   (:foreground base :background love))
  (rainbow-delimiters-unmatched-face    (:foreground base :background love))
  (rainbow-delimiters-base-error-face   (:foreground base :background love))

  (rainbow-delimiters-base-face         (:foreground muted))

  (rainbow-delimiters-depth-1-face      (:foreground pine))
  (rainbow-delimiters-depth-2-face      (:foreground rose))
  (rainbow-delimiters-depth-3-face      (:foreground love))
  (rainbow-delimiters-depth-4-face      (:foreground foam))
  (rainbow-delimiters-depth-5-face      (:foreground iris))
  (rainbow-delimiters-depth-6-face      (:foreground subtle))
  (rainbow-delimiters-depth-7-face      (:foreground muted))
  (rainbow-delimiters-depth-8-face      (:foreground gold))
  (rainbow-delimiters-depth-9-face      (:foreground iris))

  ;; show-paren
  (show-paren-match						(:background rose :foreground base :bold t))
  (show-paren-match-expression			(:background rose :foreground base :bold t))
  (show-paren-mismatch					(:background love))

  (company-tooltip                          (:background overlay :foreground text))
  (company-tooltip-common                   (:foreground gold :distant-foreground base :bold t))
  (company-tooltip-search                   (:foreground gold))
  (company-tooltip-selection                (:background overlay :bold t :underline t))
  (company-tooltip-mouse                    (:foreground base :distant-foreground text))
  (company-tooltip-annotation               (:foreground muted :distant-foreground gold))
  (company-tooltip-scrollbar-track          (:background gold))
  (company-tooltip-scrollbar-thumb          (:background rose))
  (company-tooltip-quick-access             (:foreground overlay))
  (company-tooltip-quick-access-selection   (:foreground gold))
  (company-scrollbar-bg                     (:inherit 'tooltip))
  (company-scrollbar-fg                     (:background love))
  (company-preview                          (:foreground love))
  (company-preview-common                   (:background love :foreground base))
  (company-preview-search                   (:inherit 'company-tooltip-search))
  (company-template-field                   (:inherit 'match))

  (markdown-hr-face                     (:foreground overlay))

  ;; Flycheck
  (flycheck-posframe-background-face    (:background base))
  (flycheck-posframe-face               (:background base))
  (flycheck-posframe-info-face          (:foreground foam :background "#1B2431" :height 160))
  (flycheck-posframe-warning-face       (:foreground "#FFF" :background "#2F3E56" :height 160 :weight 'semi-light))
  (flycheck-posframe-error-face         (:foreground "#FFF" :background "#2D1E28" :height 160 :weight 'semi-light))
  (flycheck-fringe-warning              (:inherit 'warning))
  (flycheck-fringe-error                (:inherit 'error))
  (flycheck-fringe-info                 (:inherit 'info ))
  (flycheck-error-list-warning          (:inherit 'warning :bold t))
  (flycheck-error-list-error            (:inheirt 'error :bold t))
  (flycheck-error-list-info             (:foreground foam :bold t))
  (flycheck-inline-error                (:background love :foreground love :height 128))
  (flycheck-inline-info                 (:background foam :foreground foam :height 128))
  (flycheck-inline-warning              (:background gold :foreground gold :height 128))
  
  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground love :background love))
  (highlight-indent-guides-stack-character-face (:foreground love :background love))
  (highlight-indent-guides-top-character-face   (:foreground love :background love))
  (highlight-indent-guides-stack-odd-face       (:foreground love :background love))
  (highlight-indent-guides-stack-even-face      (:foreground love :background love))
  (highlight-indent-guides-even-face            (:foreground love :background love))
  (highlight-indent-guides-odd-face             (:foreground love :background love))
  (highlight-indent-guides-top-odd-face         (:foreground love :background love))
  (highlight-indent-guides-top-even-face        (:foreground love :background love))

   ;;;; ivy
  (ivy-current-match                            (:background foam :foreground base :bold t))
  (ivy-action                                   (:background base :foreground iris))
  (ivy-grep-line-number                         (:background base :foreground gold))
  (ivy-minibuffer-match-face-1                  (:background base :foreground foam :bold t))
  (ivy-minibuffer-match-face-2                  (:background base :foreground pine))
  (ivy-minibuffer-match-highlight               (:foreground foam))
  (ivy-grep-info                                (:foreground foam))
  (ivy-grep-line-number                         (:foreground iris))
  (ivy-confirm-face                             (:foreground pine))

  (vertico-multiline                            (:background base))
  (vertico-group-title                          (:foreground subtle :weight 'semi-bold :height 180))
  (vertico-group-separator                      (:foreground muted :strike-through t))
  (vertico-current                              (:background overlay :distant-foreground text :bold t :foreground surface))

  (vertico-posframe-border                      (:background muted))
  (vertico-posframe                             (:background base :foreground muted))

  (corfu-annotations                            (:foreground muted))
  (corfu-current                                (:inherit 'vertico-current))
  (corfu-border                                 (:background overlay))
  (corfu-bar                                    (:background gold))
  (corfu-default                                (:background base :foreground text))
  (corfu-popupinfo                              (:background overlay :foreground text :weight 'light :italic t))
  
  ;; posframe's
  (ivy-posframe                                 (:background muted))
  (ivy-posframe-border                          (:inherit 'ivy-posframe))

  (orderless-match-face-0                       (:foreground love :background "#211A1F" :weight 'semi-bold))
  (orderless-match-face-1                       (:foreground foam :background "#222d3e" :weight 'semi-bold))
  (orderless-match-face-2                       (:foreground gold :background "#221F18" :weight 'semi-bold))
  (orderless-match-face-3                       (:foreground iris :background "#292233" :weight 'semi-bold))

  (comint-highlight-prompt                      (:background gold :foreground base))

  (completions-annotations                      (:foreground muted :italic t))
  (completions-highlight                        (:foreground foam :italic t))
  (completions-common-part                      (:foreground gold :distant-foreground base :distant-background pine :bold t :italic t))
  (completions-first-difference                 (:foreground love :strike-through t))
  (consult-file                                 (:foreground muted :distant-foreground base))

  (diff-added (:background pine :foreground text))
  (diff-changed (:background gold :foreground base))
  
  (treemacs-directory-collapsed-face			(:foreground muted))
  (treemacs-directory-face						(:foreground subtle))
  (treemacs-file-face							(:foreground subtle))
  (treemacs-fringe-indicator-face               (:foreground love))

  (treemacs-git-added-face						(:foreground gold))
  (treemacs-git-renamed-face				   	(:foreground rose))
  (treemacs-git-ignored-face				   	(:foreground muted))
  (treemacs-git-unmodified-face		   			(:foreground text))
  (treemacs-git-renamed-face		   			(:foreground text))
  (treemacs-git-modified-face		   			(:foreground rose))

  ;; lets support solaire mode
  (solaire-default-face (:background base))
  ;; lsp
  (lsp-headerline-breadcrumb-path-error-face (:underline (:color rose :style 'wave)
                                                         :foreground muted :background base))

  (lsp-headerline-breadcrumb-path-face				(:background muted))
  (lsp-headerline-breadcrumb-path-hint-face	   		(:background base))
  (lsp-headerline-breadcrumb-path-info-face	   		(:background muted))
  (lsp-headerline-breadcrumb-separator-face			(:background muted))
  (lsp-headerline-breadcrumb-symbols-face			(:background foam))
  (lsp-headerline-breadcrumb-project-prefix-face	(:background gold))
  (lsp-headerline-breadcrumb-symbols-error-face     (:foreground love))

  (lsp-ui-doc-background							(:background base :foreground love))
  (lsp-ui-doc-header								(:background base :foreground love))
  (lsp-ui-peek-filename								(:foreground foam))
  (lsp-ui-sideline-code-action			   			(:foreground gold))
  (lsp-ui-sideline-current-symbol					(:foreground foam))
  (lsp-ui-sideline-symbol							(:foreground muted))

  ;; dashboard
  (dashboard-items-face								(:weight 'light :height 150))
  (dashboard-banner-logo-title						(:weight 'thin :height 320))
  (dashboard-heading								(:foreground subtle :weight 'thin :height 170))
  (dashboard-no-items-face							(:foreground muted))

  ;; all-the-icons
  (all-the-icons-dgreen							(:foreground pine))
  (all-the-icons-green							(:foreground pine))
  (all-the-icons-dpurple						(:foreground iris))
  (all-the-icons-purple							(:foreground iris))

  ;; evil
  (evil-ex-lazy-highlight           (:foreground base :background gold :bold t))
  (evil-ex-substitute-matches       (:foreground love :strike-through t))
  (evil-ex-substitute-replacement   (:foreground foam :bold t))
  (evil-search-highlight-persist-highlight-face (:background gold))
  (evil-quickscope-first-face       (:foreground gold :underline t))
  (evil-quickscope-second-face      (:foreground gold :underline t))
  (evil-goggles-default-face        (:background gold))
  (evil-goggles-join-face           (:foreground foam))
  (evil-goggles-delete-face         (:background love))
  (evil-goggles-paste-face          (:background pine))
  (evil-goggles-indent-face         (:background muted))
  (evil-goggles-set-marker-face     (:foreground love :background love))
  (evil-goggles-yank-face           (:foreground foam :background foam))

  (ansi-color-crust (:background base))

  (term (:background base :foreground text))
  (term-color-blue (:background foam :foreground foam))
  (term-color-bright-blue (:inherit 'term-color-blue))
  (term-color-red (:background love :foreground love))
  (term-color-bright-red (:background rose :foreground rose))
  (term-color-yellow (:background gold :foreground gold))
  (term-color-bright-yellow (:background gold :foreground gold))

  (term-color-green (:background pine :foreground pine))
  (term-color-bright-green (:inherit 'term-color-green))

  (term-color-bright-crust (:background base :foreground love))
  (term-color-rosewater (:background text :foreground text))
  (term-color-bright-rosewater (:background rose :foreground rose))
  (term-color-cyan (:background foam :foreground foam))
  (term-color-bright-cyan (:background foam :foreground foam))
  (term-color-magenta (:background iris :foreground iris))
  (term-color-bright-magenta (:background iris :foreground iris))
  (term-underline (:background iris :foreground foam))

  (vterm-color-crust (:background base :foreground base))
  (vterm-color-blue (:background foam :foreground foam))
  (vterm-color-cyan (:background foam :foreground foam))
  (vterm-color-green (:background pine :foreground pine))
  (vterm-color-magenta (:background rose :foreground rose))
  (vterm-color-yellow (:background gold :foreground gold))
  (vterm-color-red (:background love :foreground love))
  (vterm-color-rosewater (:background text :foreground text))

  (popup-face (:inherit 'tooltip))
  (popup-selection-face (:inherit 'tooltip))
  (popup-tip-face (:inherit 'tooltip))

  (anzu-match-1 (:foreground pine :background base))
  (anzu-match-2 (:foreground gold :background base))
  (anzu-match-3 (:foreground foam :background base))

  (anzu-mode-line		(:foreground base :background iris))
  (anzu-mode-no-match	(:foreground text :background love))
  (anzu-replace-to		(:foreground gold :background muted))

  (ace-jump-face-background (:foreground muted))
  (ace-jump-face-foreground (:foreground love :background base :bold t))

  (hydra-face-amaranth		(:foreground iris))
  (hydra-face-blue			(:foreground foam))
  (hydra-face-pink			(:foreground love))
  (hydra-face-red			(:foreground love))
  (hydra-face-teal			(:foreground foam))

  ;; Bookmarks
  (bm-fringe-face                           (:background love :foreground base))
  (bm-fringe-persistent-face                (:background love :foreground base))

  (centaur-tabs-active-bar-face				(:background base :foreground text))
  (centaur-tabs-selected					(:background base :foreground text :bold t))
  (centaur-tabs-selected-modified			(:background base :foreground text))
  (centaur-tabs-modified-marker-selected	(:background base :foreground text))
  (centaur-tabs-close-selected				(:inherit 'centaur-tabs-selected))

  (centaur-tabs-unselected					(:background base :foreground muted))
  (centaur-tabs-unselected-modified			(:background base :foreground iris))
  (centaur-tabs-modified-marker-unselected	(:background base :foreground muted))
  (centaur-tabs-close-unselected			(:background base :foreground muted))

  (centaur-tabs-close-mouse-face			(:foreground love))
  (centaur-tabs-default						(:background base))
  (centaur-tabs-name-mouse-face				(:foreground foam :bold t))

  (git-gutter:added                              (:foreground pine))
  (git-gutter:deleted                            (:foreground love))
  (git-gutter:modified                           (:foreground foam))

  (goggles-added (:background pine))
  (goggles-changed (:background foam))
  (goggles-removed (:background love))
  
  ;; Tree sitter highlightning
  (tree-sitter-hl-face:function                  (:inherit 'font-lock-function-name-face))
  (tree-sitter-hl-face:function.call             (:inherit 'tree-sitter-hl-face:function))
  (tree-sitter-hl-face:function.builtin          (:foreground love))
  (tree-sitter-hl-face:function.special          (:foreground text :italic t :bold t))
  (tree-sitter-hl-face:function.macro            (:foreground pine))
  (tree-sitter-hl-face:function.label            (:foreground gold))

  (tree-sitter-hl-face:method                    (:inherit 'tree-sitter-hl-face:function))
  (tree-sitter-hl-face:method.call               (:inherit 'tree-sitter-hl-face:method))

  (tree-sitter-hl-face:type                      (:inherit 'font-lock-type-face))
  (tree-sitter-hl-face:type.parameter            (:foreground iris :italic t))
  (tree-sitter-hl-face:type.argument             (:foreground muted))
  (tree-sitter-hl-face:type.builtin              (:inherit 'font-lock-builtin-face))
  (tree-sitter-hl-face:type.super                (:foreground rose :bold t))
  (tree-sitter-hl-face:constructor               (:foreground foam :weight 'semi-bold))

  (tree-sitter-hl-face:variable                  (:inherit 'font-lock-variable-name-face))
  (tree-sitter-hl-face:variable.parameter        (:inherit 'tree-sitter-hl-face:type.parameter :weight 'semi-bold))
  (tree-sitter-hl-face:variable.builtin          (:foreground foam :italic t))
  (tree-sitter-hl-face:variable.special          (:foreground iris :italic t))
  (tree-sitter-hl-face:variable.synthesized      (:foreground love :italic t))
  (tree-sitter-hl-face:property                  (:foreground rose))
  (tree-sitter-hl-face:property.definition       (:inherit 'tree-sitter-hl-face:property :italic t))

  (tree-sitter-hl-face:comment                   (:inherit 'font-lock-comment-face))
  (tree-sitter-hl-face:doc                       (:inherit 'font-lock-comment-face))
  (tree-sitter-hl-face:string                    (:inherit 'font-lock-string-face))
  (tree-sitter-hl-face:string.special            (:inherit 'font-lock-string-face))
  (tree-sitter-hl-face:escape                    (:inherit 'font-lock-regexp-grouping-backslash))
  (tree-sitter-hl-face:embedded                  (:foreground foam))

  (tree-sitter-hl-face:keyword                   (:inherit 'font-lock-keyword-face))
  (tree-sitter-hl-face:operator                  (:foreground subtle))
  (tree-sitter-hl-face:label                     (:foreground muted))
  (tree-sitter-hl-face:constant                  (:inherit 'font-lock-constant-face))
  (tree-sitter-hl-face:constant.builtin          (:inherit 'font-lock-constant-face :weight 'normal))
  (tree-sitter-hl-face:number                    (:foreground gold))

  (tree-sitter-hl-face:punctuation               (:foreground subtle :weight 'semi-bold))
  (tree-sitter-hl-face:punctuation.bracket       (:foreground subtle :weight 'semi-bold))
  (tree-sitter-hl-face:punctuation.delimiter     (:foreground text :weight 'semi-bold))
  (tree-sitter-hl-face:punctuation.special       (:foreground subtle :weight 'semi-bold))

  (tree-sitter-hl-face:case-pattern              (:foreground gold))
  (tree-sitter-hl-face:keyword.compiler          (:foreground muted :bold t :italic t))

  ;; ;; Custom for pinkus tree-sitter-swift
  (tree-sitter-hl-face:include                   (:foreground muted :italic t :bold t))
  (tree-sitter-hl-face:parameter                 (:foreground iris :italic t))
  (tree-sitter-hl-face:repeat                    (:foreground foam))
  (tree-sitter-hl-face:boolean                   (:inherit 'font-lock-constant-face))
  (tree-sitter-hl-face:keyword.return            (:inherit 'tree-sitter-hl-face:keyword :italic t))
  (tree-sitter-hl-face:keyword.operator          (:foreground pine :bold t))
  (tree-sitter-hl-face:keyword.function          (:inherit 'tree-sitter-hl-face:keyword))
  (tree-sitter-hl-face:conditional               (:inherit 'tree-sitter-hl-face:keyword :weight 'semi-bold))

  (swift-mode:preprocessor-keyword-face (:foreground text :italic t))
  (swift-mode:property-access-face (:foreground subtle))
  (swift-mode:builtin-property-face (:foreground rose))
  (swift-mode:builtin-enum-case-face (:foreground foam))
  (swift-mode:builtin-method-trailing-closure-face (:foreground foam))
  (swift-mode:builtin-function-trailing-closure-face (:foreground foam))
  (swift-mode:function-call-face (:foreground love))
  
  ;; (localizeable-variable-face (:inherit font-lock-string-face))
  ;; (localizeable-value-face (:inherit font-lock-keyword-face))

 ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'rose-pine-theme)
;;; rose-pine-theme.el ends here
