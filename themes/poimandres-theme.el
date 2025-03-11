;;; poimandres-theme.el --- Poimandres theme -*- lexical-binding: t; -*-

;;; Commentary: A dark theme inspired by the Poimand

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


(autothemer-deftheme poimandres "Poimandres theme"
 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (yellow       "#FFFAC2" "#FFFAC2")
  (teal1        "#5DE4C7" "#5DE4C7")
  (teal2        "#5FB3A1" "#5FB3A1")
  (teal3        "#42675A" "#42675A")
  (blue1        "#89DDFF" "#89DDFF")
  (blue2        "#ADD7FF" "#ADD7FF")
  (blue3        "#91B4D5" "#91B4D5")
  (blue4        "#7390AA" "#7390AA")
  (pink1        "#FAE4FC" "#FAE4FC")
  (pink2        "#FCC5E9" "#FCC5E9")
  (pink3        "#D0679D" "#D0679D")
  (blueGray1    "#A6ACCD" "#A6ACCD")
  (blueGray2    "#767C9D" "#767C9D")
  (blueGray3    "#506477" "#506477")
  (background1  "#303340" "#303340")
  (background2  "#1B1E28" "#1B1E28")
  (background3  "#171922" "#171922")

  (light        "#FFFFFF" "#FFFFFF")  ; White
  (flamingo     "#C2C2C2" "#C2C2C2")
  (pink         "#B8B8B8" "#B8B8B8")
  (maroon       "#9A9A9A" "#9A9A9A")
  (peach        "#909090" "#909090")

  (text         "#E4F0FB" "#E4F0FB")
  (green        "#7C7C7C" "#7C7C7C")
  (sky          "#686868" "#686868")
  (rosewater    "#4A4A4A" "#4A4A4A")
  (subtext1     "#404040" "#404040")
  (subtext0     "#363636" "#363636")
  (overlay2     "#2C2C2C" "#2C2C2C")
  (surface2     "#1A1A1A" "#1A1A1A")
  (surface1     "#161616" "#161616")
)

 ;; Customize faces
 ((default                              (:background background2 :foreground text))
  (border                               (:foreground blueGray2))
  (bookmark-face                        (:foreground pink3))
  (button                               (:foreground blue1))
  (child-frame                          (:foreground background3))
  (child-frame-border                   (:foreground background3))
  (cursor                               (:background blue1 :foreground background3))
  (error                                (:foreground pink3))
  (link                                 (:foreground blue1))
  (fringe                               (:foreground yellow))
  (file-name-shadow                     (:foreground teal1))
  (glyph-face                           (:background teal2 :foreground background1))
  (glyphless-char                       (:foreground yellow))
  (header-line                          (:background background3 :foreground maroon :weight 'semi-bold :italic t))
  (highlight                            (:background background3 :foreground blue1 :distant-foreground background3))
  (hl-line                              (:background background1))
  (line-number                          (:foreground blueGray3))
  (line-number-current-line             (:background background2 :foreground yellow :bold t))
  (match                                (:background yellow :foreground background3))
  (menu                                 (:foreground rosewater))
  (fill-column-indicator                (:foreground background2))

  (mode-line                            (:background background3 :foreground text :box (:line-width 1 :color overlay2)))
  (mode-line-inactive                   (:background background3 :foreground rosewater))
  (mode-line-active                     (:inherit 'mode-line))
  (mode-line-highlight                  (:foreground light))
  (mode-line-emphasis                   (:foreground pink))
  (mode-line-buffer-id                  (:foreground teal2))

  (numbers                              (:background peach))
  (region                               (:background teal3))
  (tooltip                              (:background overlay2 :foreground background3))
  (shadow                               (:foreground green))
  (success                              (:foreground teal2))
  (vertical-border                      (:foreground background2))
  (warning                              (:foreground yellow))
  (window-divider                       (:foreground background3 :distant-foreground background3))

  (whitespace-newline                   (:foreground surface2))
  (whitespace-space                     (:foreground surface2))
  (whitespace-trailing                  (:foreground background3 :background pink3))

  ;; ;; Font lock
  (font-lock-bracket-face               (:foreground pink))
  (font-lock-builtin-face               (:foreground teal1 :weight 'semi-bold))
  (font-lock-comment-delimiter-face     (:foreground blueGray2 :italic t))
  (font-lock-comment-face               (:foreground blueGray1 :italic t))
  (font-lock-constant-face              (:foreground text :bold t))
  (font-lock-delimiter-face             (:foreground yellow))
  (font-lock-doc-face                   (:foreground blueGray1))
  (font-lock-doc-markup-face            (:foreground blueGray2))
  (font-lock-escape-face                (:foreground yellow))
  (font-lock-function-call-face         (:foreground blue3 :weight 'normal))
  (font-lock-function-name-face         (:foreground blue2))
  (font-lock-keyword-face               (:foreground blue3))
  (font-lock-misc-punctuation-face      (:foreground yellow))
  (font-lock-negation-char-face         (:foreground pink3))
  (font-lock-number-face                (:foreground teal1))
  (font-lock-operator-face              (:foreground pink2))
  (font-lock-preprocessor-face	   	(:foreground text))
  (font-lock-property-name-face	        (:foreground blueGray1 :weight 'normal))
  (font-lock-property-use-face	        (:foreground blue1 :weight 'thin))
  (font-lock-punctuation-face           (:foreground teal2 :weight 'normal))
  (font-lock-reference-face		(:foreground teal2))
  (font-lock-regexp-grouping-backslash  (:foreground pink2 :weight 'semi-bold))
  (font-lock-string-face                (:foreground teal1 :italic t))
  (font-lock-type-face                  (:foreground blueGray1))
  (font-lock-variable-name-face         (:foreground text :weight 'light))
  (font-lock-variable-use-face          (:foreground text :weight 'light))
  (font-lock-warning-face               (:inherit 'warning))

  (swift-ts-face-annotation             (:foreground blue1))
  (swift-ts-face-annotation.builtin     (:foreground blue1))
  (swift-ts-face-annotation.type        (:foreground green))

  (swift-ts-face-punctuation.type                (:inherit 'font-lock-punctuation-face))
  (swift-ts-face-compiler                        (:inherit 'font-lock-builtin-face))
  (swift-ts-face-constructor.call                (:inherit 'font-lock-function-call-face))

  (swift-ts-face-face-label                      (:foreground green))
  (swift-ts-face-method.call                     (:inherit 'font-lock-function-call-face))
  (swift-ts-face-method.name                     (:inherit 'font-lock-function-name-face))
  (swift-ts-face-keyword.annotation              (:foreground pink :background green :weight 'bold))
  (swift-ts-face-keyword.type                    (:inherit 'font-lock-type-face))
  (swift-ts-face-variable.synthesized            (:foreground blue1))

  (highlight-operators-face             (:foreground pink3))
  (highlight-quoted-symbol              (:foreground teal2))
  (highlight-numbers-face               (:foreground pink2))
  (highlight-symbol-face                (:foreground pink3 :underline (:line-width -1 :color pink3)))
  (info-xref                            (:foreground teal2))

  (minibuffer-prompt-end                (:foreground yellow))
  (minibuffer-prompt                    (:foreground yellow))
  (epa-mark                             (:foreground teal2))
  (dired-mark                           (:foreground pink))
  (dired-ignored                        (:background peach))

  (iedit-occurrence                     (:background blue1 :foreground background3))
  (iedit-read-only-occurrence           (:background green :foreground background3))

  ;; ;; Battery colors
  (doom-modeline-battery-critical       (:inherit 'error))
  (doom-modeline-battery-warning        (:inherit 'warning))
  (doom-modeline-battery-charging       (:foreground overlay2))
  (doom-modeline-battery-error          (:inherit 'eror))
  (doom-modeline-battery-normal         (:foreground overlay2))
  (doom-modeline-battery-full           (:foreground blueGray2))

  ;; Doom visual state
  (doom-modeline-evil-insert-state      (:foreground background2))
  (doom-modeline-evil-normal-state      (:foreground subtext0))
  (doom-modeline-evil-visual-state      (:foreground sky))
  (doom-modeline-evil-replace-state     (:foreground pink3))
  (doom-modeline-evil-operator-state    (:foreground blue1))

  (doom-modeline-project-dir            (:foreground overlay2))
  (doom-modeline-buffer-path            (:foreground overlay2))
  (doom-modeline-buffer-file            (:foreground text :bold t))
  (doom-modeline-buffer-major-mode      (:foreground maroon :bold t))
  (doom-modeline-buffer-modified        (:foreground text :italic t :bold t))
  (doom-modeline-error                  (:background pink3))
  (doom-modeline-info                   (:foreground subtext1 :bold t))
  (doom-modeline-time                   (:foreground overlay2 :weight 'semi-bold))
  (doom-modeline-project-dir            (:foreground blue1))
  (doom-modeline-bar                    (:background yellow))
  (doom-modeline-bar-inactive           (:inherit 'mode-line-inactive))
  (doom-modeline-panel                  (:background blue1 :foreground background3 :bold t))
  (doom-modeline                        (:foreground blueGray2))
  (doom-themes-visual-bell              (:background pink3))


  (telephone-line-accent-active         (:background background2 :foreground subtext1))
  (telephone-line-accent-inactive       (:background background2 :foreground subtext0))
  (telephone-line-evi-normal           (:background blueGray2 :foreground background3 :bold t))
  (telephone-line-evil-visual           (:background yellow :foreground background3 :bold t))
  (telephone-line-evil-motion           (:background blue1 :foreground background3 :bold t))
  (telephone-line-evil-insert           (:background green :foreground background3 :bold t))

  ;;elfeed
  (elfeed-search-feed-face              (:foreground pink2))
  (elfeed-search-tag-face               (:foreground green))

  ;; message colors
  (message-header-name                  (:foreground overlay2))
  (message-header-other                 (:foreground peach))
  (message-header-subject               (:foreground yellow))
  (message-header-to                    (:foreground rosewater))
  (message-header-cc                    (:foreground green))
  (message-header-xheader               (:foreground rosewater))
  (custom-link                          (:foreground blue1 :underline t))

  (avy-lead-face                        (:background pink3 :foreground background3))
  (avy-lead-face-0                      (:background blue1 :foreground background3))
  (avy-lead-face-1                      (:background green :foreground background3))
  (avy-lead-face-2                      (:background rosewater :foreground background3))
  
  ;; org-mode
  (org-done                             (:foreground overlay2))
  (org-code                             (:background background3))
  (org-meta-line                        (:background surface1 :foreground blue1))
  (org-block                            (:background background3))
  (org-block-begin-line                 (:background background3 :foreground overlay2))
  (org-block-end-line	                (:background background3 :foreground overlay2))
  (org-headline-done                    (:foreground overlay2 :strike-through t))
  (org-todo                             (:foreground green :bold t))
  (org-headline-todo                    (:foreground background3))
  (org-upcoming-deadline                (:foreground pink3))
  (org-footnote                         (:foreground green))
  (org-indent                           (:foreground background3))
  (org-hide                             (:foreground background3))
  (org-date                             (:foreground overlay2))
  (org-ellipsis                         (:foreground overlay2 :bold t))
  (org-level-1                          (:foreground yellow :height 1.3 :bold t))
  (org-level-2                          (:foreground teal2 :height 1.15 :bold t))
  (org-level-3                          (:foreground blue1 :height 1.05))
  (org-level-4                          (:foreground pink2))
  (org-level-5                          (:foreground pink3))
  (org-level-6                          (:foreground yellow))
  (org-level-7                          (:foreground peach))
  (org-level-8                          (:foreground maroon))

  ;; which-key
  (which-key-key-face                   (:foreground teal2))
  (which-func				(:inherit 'font-lock-function-name-face :bold t))
  (which-key-group-description-face     (:foreground pink))
  (which-key-command-description-face   (:foreground blue1))
  (which-key-local-map-description-face (:foreground yellow))

  (info-header-xref                     (:foreground yellow))
  (xref-file-header                     (:foreground yellow))
  (xref-match		                    (:foreground peach))

  ;; rainbow delimiter
  (rainbow-delimiters-mismatched-face   (:foreground background3 :background pink2))
  (rainbow-delimiters-unmatched-face    (:foreground background3 :background pink2))
  (rainbow-delimiters-base-error-face   (:foreground background3 :background pink2))

  (rainbow-delimiters-base-face         (:foreground overlay2))

  (rainbow-delimiters-depth-1-face      (:foreground blue1))
  (rainbow-delimiters-depth-2-face      (:foreground blue4))
  (rainbow-delimiters-depth-3-face      (:foreground blue3))
  (rainbow-delimiters-depth-4-face      (:foreground sky))
  (rainbow-delimiters-depth-5-face      (:foreground text))
  (rainbow-delimiters-depth-6-face      (:foreground blue1))
  (rainbow-delimiters-depth-7-face      (:foreground green))
  (rainbow-delimiters-depth-8-face      (:foreground yellow))
  (rainbow-delimiters-depth-9-face      (:foreground sky))

  ;; show-paren
  (show-paren-match			(:background maroon :foreground background3 :bold t))
  (show-paren-match-expression		(:background maroon :foreground background3 :bold t))
  (show-paren-mismatch			(:background pink3))

  (markdown-hr-face                     (:foreground background2))

  ;; Flycheck
  (flycheck-posframe-background-face    (:background background3))
  (flycheck-posframe-face               (:background background3))
  (flycheck-fringe-warning              (:inherit 'warning))
  (flycheck-fringe-error                (:inherit 'error))
  (flycheck-fringe-info                 (:inherit 'info ))
  (flycheck-error-list-warning          (:inherit 'warning :bold t))
  (flycheck-error-list-error            (:inheirt 'error :bold t))
  (flycheck-error-list-info             (:background blue1 :bold t))
  (flycheck-inline-error                (:background pink3 :foreground pink3 :height 128))
  (flycheck-inline-info                 (:background blue1 :foreground blue1 :height 128))
  (flycheck-inline-warning              (:background yellow :foreground yellow :height 128))
  
  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground pink3 :background pink3))
  (highlight-indent-guides-stack-character-face (:foreground pink3 :background pink3))
  (highlight-indent-guides-top-character-face   (:foreground pink3 :background pink3))
  (highlight-indent-guides-stack-odd-face       (:foreground pink3 :background pink3))
  (highlight-indent-guides-stack-even-face      (:foreground pink3 :background pink3))
  (highlight-indent-guides-even-face            (:foreground pink3 :background pink3))
  (highlight-indent-guides-odd-face             (:foreground pink3 :background pink3))
  (highlight-indent-guides-top-odd-face         (:foreground pink3 :background pink3))
  (highlight-indent-guides-top-even-face        (:foreground pink3 :background pink3))

   ;;;; ivy
  (ivy-current-match                            (:background blue1 :foreground background3 :bold t))
  (ivy-action                                   (:background background3 :foreground pink2))
  (ivy-grep-line-number                         (:background background3 :foreground peach))
  (ivy-minibuffer-match-face-1                  (:background background3 :foreground blue1 :bold t))
  (ivy-minibuffer-match-face-2                  (:background background3 :foreground green))
  (ivy-minibuffer-match-highlight               (:foreground blue1))
  (ivy-grep-info                                (:foreground blue1))
  (ivy-grep-line-number                         (:foreground text))
  (ivy-confirm-face                             (:foreground green))

  (vertico-multiline                            (:background background3))
  (vertico-group-title                          (:foreground teal2 :weight 'normal))
  (vertico-group-separator                      (:foreground teal2 :strike-through t))
  (vertico-current                              (:background blue2 :distant-foreground background3 :bold t :foreground light))

  (vertico-posframe-border                      (:background background3))
  (vertico-posframe                             (:background background3 :foreground text))

  (corfu-annotations                            (:foreground blueGray3 :distant-foreground background3 :italic t))
  (corfu-current                                (:inherit 'vertico-current))

  (corfu-border                                 (:background surface1))
  (corfu-bar                                    (:background yellow))
  (corfu--cbar                                  (:background teal2))
  (corfu-default                                (:background background3 :foreground text))
  (corfu-popupinfo                              (:background surface1 :foreground text :weight 'light :italic t))
  
  ;; posframe's
  (ivy-posframe                                 (:background surface2))
  (ivy-posframe-border                          (:inherit 'ivy-posframe))

  (orderless-match-face-0                       (:foreground teal2 :distant-foreground background3 :weight 'bold))
  (orderless-match-face-1                       (:foreground blue1 :weight 'bold))
  (orderless-match-face-2                       (:foreground pink2 :weight 'bold))
  (orderless-match-face-3                       (:foreground pink3 :weight 'bold))

  (comint-highlight-prompt                      (:foreground teal2))

  ;; (completions-annotations                      (:foreground yellow :distant-foreground background3 :italic t))
  ;; (completions-highlight                        (:foreground text :italic t))
  ;; (completions-common-part                      (:foreground yellow :distant-foreground background3 :distant-background green :bold t :italic t))
  ;; (completions-first-difference                 (:foreground pink3))

  (consult-file                                 (:foreground text :distant-foreground background3))

  (diff-added                                   (:background green :foreground text))
  (diff-changed                                 (:background yellow :foreground background3))
  
  (treemacs-window-background-face              (:background background3))
  (treemacs-directory-face                      (:foreground text))
  (treemacs-file-face                           (:foreground text))
  (treemacs-directory-collapsed-face		(:foreground subtext0))
  (treemacs-directory-face			(:foreground subtext1))
  (treemacs-file-face				(:foreground subtext0))
  (treemacs-nerd-icons-file-face                (:foreground pink2))
  (treemacs-fringe-indicator-face               (:foreground pink3))

  (treemacs-git-added-face			(:foreground peach))
  (treemacs-git-renamed-face			(:foreground rosewater))
  (treemacs-git-ignored-face			(:foreground pink2))
  (treemacs-git-unmodified-face		   	(:foreground text))
  (treemacs-git-renamed-face		   	(:foreground teal2))
  (treemacs-git-modified-face		   	(:foreground teal2))

  ;; all-the-icons
  (all-the-icons-dgreen							(:foreground green))
  (all-the-icons-green							(:foreground green))
  (all-the-icons-dpurple						(:foreground text))
  (all-the-icons-purple							(:foreground text))

  ;; evil
  (evil-ex-lazy-highlight           (:background background3 :foreground yellow :italic t :box (:line-width -1 :color yellow)))
  (evil-ex-search                   (:background background3 :foreground yellow :italic t :box (:line-width -1 :color yellow)))
  (evil-ex-substitute-matches       (:background pink2 :foreground background3 :strike-through t))
  (evil-ex-substitute-replacement   (:background teal1 :foreground background3 :bold t))
  (evil-search-highlight-persist-highlight-face (:background blue1))

  (evil-quickscope-first-face       (:foreground yellow :underline t))
  (evil-quickscope-second-face      (:foreground peach :underline t))
  (evil-goggles-default-face        (:background peach))
  (evil-goggles-join-face           (:foreground blue1))
  (evil-goggles-delete-face         (:background pink3))
  (evil-goggles-paste-face          (:background green))
  (evil-goggles-indent-face         (:background subtext0))
  (evil-goggles-set-marker-face     (:foreground pink3 :background pink3))
  (evil-goggles-yank-face           (:foreground blue1 :background blue1))

  (ansi-color-crust (:background background3))

  (term (:background background3 :foreground text))
  (term-color-blue (:background blue1 :foreground blue1))
  (term-color-bright-blue (:inherit 'term-color-blue))
  (term-color-red (:background pink3 :foreground pink3))
  (term-color-bright-red (:background maroon :foreground maroon))
  (term-color-yellow (:background yellow :foreground yellow))
  (term-color-bright-yellow (:background yellow :foreground yellow))

  (term-color-green (:background green :foreground green))
  (term-color-bright-green (:inherit 'term-color-green))

  (term-color-bright-crust (:background background3 :foreground pink3))
  (term-color-rosewater (:background text :foreground text))
  (term-color-bright-rosewater (:background rosewater :foreground rosewater))
  (term-color-cyan (:background sky :foreground sky))
  (term-color-bright-cyan (:background sky :foreground sky))
  (term-color-red (:background text :foreground text))
  (term-color-bright-red (:background text :foreground text))
  (term-underline (:background text :foreground blue1))

  (vterm-color-crust (:background background3 :foreground background3))
  (vterm-color-blue (:background blue1 :foreground blue1))
  (vterm-color-cyan (:background sky :foreground sky))
  (vterm-color-green (:background green :foreground green))
  (vterm-color-red (:background maroon :foreground maroon))
  (vterm-color-yellow (:background peach :foreground yellow))
  (vterm-color-red (:background pink3 :foreground pink3))
  (vterm-color-rosewater (:background text :foreground text))

  (popup-face (:inherit 'tooltip))
  (popup-selection-face (:inherit 'tooltip))
  (popup-tip-face (:inherit 'tooltip))
  (eldoc-box-body (:foreground maroon :background background3))
  (eldoc-box-border (:background background3))

  (anzu-match-1 (:foreground green :background background3))
  (anzu-match-2 (:foreground yellow :background background3))
  (anzu-mode-line		(:foreground background3 :background text))
  (anzu-mode-no-match	(:foreground text :background pink3))
  (anzu-replace-to		(:foreground yellow :background surface2))

  (ace-jump-face-background (:foreground overlay2))
  (ace-jump-face-foreground (:foreground pink3 :background background3 :bold t))

  (hydra-face-amaranth		(:foreground text))
  (hydra-face-blue		(:foreground blue1))
  (hydra-face-pink		(:foreground pink))
  (hydra-face-red		(:foreground pink3))

  (bm-fringe-face                           (:background pink3 :foreground background3))
  (bm-fringe-persistent-face                (:background pink3 :foreground background3))

  (centaur-tabs-active-bar-face				(:background background3 :foreground text))
  (centaur-tabs-selected					(:background background3 :foreground text :bold t))
  (centaur-tabs-selected-modified			(:background background3 :foreground text))
  (centaur-tabs-modified-marker-selected	(:background background3 :foreground text))
  (centaur-tabs-close-selected				(:inherit 'centaur-tabs-selected))

  (centaur-tabs-unselected					(:background background3 :foreground overlay2))
  (centaur-tabs-unselected-modified			(:background background3 :foreground text))
  (centaur-tabs-modified-marker-unselected	(:background background3 :foreground overlay2))
  (centaur-tabs-close-unselected			(:background background3 :foreground overlay2))

  (centaur-tabs-close-mouse-face			(:foreground pink3))
  (centaur-tabs-default					(:background background3))
  (centaur-tabs-name-mouse-face				(:foreground blue1 :bold t))

  (git-gutter:added                              (:foreground teal1))
  (git-gutter:deleted                            (:foreground pink2))
  (git-gutter:modified                           (:foreground teal2))

  (goggles-added (:background green))
  (goggles-changed (:background blue1))
  (goggles-removed (:background pink3))

  (punch-line-evil-normal-face  (:foreground background3 :background text :weight 'bold))
  (punch-line-evil-visual-face  (:foreground background3 :background pink3 :weight 'bold))
  (punch-line-evil-replace-face (:foreground background3 :background pink2 :weight 'bold))
  (punch-line-evil-insert-face  (:foreground background3 :background yellow :weight 'bold))
  (punch-line-project-face      (:foreground blue1 :weight 'bold))
  (punch-line-buffer-name-face  (:foreground sky :weight 'bold))
  (punch-line-time-face         (:foreground background3))
  (punch-line-major-mode-face   (:foreground light))
  (punch-line-separator-face    (:foreground subtext0 :weight 'thin))
  
  (flycheck-overlay-error (:background background3 :foreground pink2 :height 0.9))
  (flycheck-overlay-warning (:background background3 :foreground yellow :height 0.9))
  (flycheck-overlay-info (:background background3 :foreground teal2 :height 0.9))
  (flycheck-overlay-marker (:foreground pink1 :weight 'semi-bold :italic t :height 0.9))

  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'poimandres)
;;; poimandres-theme.el ends here
