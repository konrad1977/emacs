;;; package: --- A beautiful theme inspired by neon lights -*- lexical-binding: t -*-

;;; Commentary:
;; A neon theme

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(autothemer-deftheme
        nordic "A port of the Neovim theme"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (white3    "#ECEFF4" "#eee8d5")
  (black0   "#191D24" "#120e28")
  (black1   "#1E222A" "#1d1741")
  (black2   "#222630" "#271e56")
  (gray0    "#242933" "#201947")
  (gray1    "#2E3440" "#2e2466")
  (gray2    "#3B4252" "#352975")
  (gray3    "#434C5E" "#3e318a")
  (gray4    "#4C566A" "#322880")
  (gray5    "#69728A" "#423494")
  (magent_d "#A97EA1" "#7E74CC")
  (magenta  "#B48EAD" "#7E74CC")
  (magent_b "#BE9dB8" "#7E74CC")
  (red_d    "#B74E58" "#ff1d8a")
  (red      "#BF616A" "#ff047d")
  (red_b    "#C5727A" "#e61d7e")
  (green    "#A3BE8C" "#859900")
  (green_b  "#B1c89D" "#b2c62d")
  (blue0    "#5E81AC" "#268bd2")
  (blue1    "#81A1c1" "#197ec5")
  (blue2    "#88C0D0" "#197ec5")
  (yellow_d "#E7C173" "#b58900")
  (yellow   "#EBCB8B" "#b58900")
  (yellow_b "#EDD49F" "#c4980f")
  (orange_d "#CB775D" "#c85106")
  (orange   "#D08770" "#c85106")
  (orange_b "#D79784" "#c85106")
  (cyan_d   "#80B3B2" "#74c5aa")
  (cyan     "#8FBCBB" "#37dcf6")
  )

 ;; Customize faces
 (
  (default                      (:background gray0 :foreground white3))
  (file-name-shadow             (:foreground gray5))
  (fringe                       (:background gray0))
  (hl-line                      (:background black1))
  (header-line                  (:background black1))
  (line-number                  (:foreground gray2))
  (line-number-current-line     (:background black1 :foreground white3 :weight 'semi-bold))
  (region                       (:background black0))

  (mode-line                    (:background black2 :foreground magenta :box (:line-width 1 :color black0)))
  (mode-line-inactive           (:background black2 :foreground magenta))
  (mode-line-active             (:inherit 'mode-line))
  (mode-line-highlight          (:foreground magent_d))
  (mode-line-buffer-id          (:foreground blue1))

  (vertical-border              (:foreground black0))
  (link                         (:foreground blue0))
  (custom-link                  (:foreground blue0))
  (warning                      (:foreground yellow_b))
  (error                        (:foreground red_b))
  (success                      (:foreground cyan_d))

  (vertico-posframe-border      (:background black0))
  (vertico-posframe             (:background black0 :foreground gray5))
  (vertico-current              (:background gray3 :foreground white3))

  (corfu-current                (:inherit 'vertico-current))
  (corfu-annotations            (:background black0))
  (corfu-default                (:background black0 :foreground white3))
  (corfu-border                 (:background black0))
  (corfu-popupinfo              (:background black0 :foreground blue0 :box (:line-width 1 :color gray0)))
  
  (eldoc-box-body               (:inherit 'vertico-posframe))
  (eldoc-box-border             (:background black2))

  (orderless-match-face-0                        (:foreground red_b :weight 'bold))
  (orderless-match-face-1                        (:foreground blue1 :weight 'bold))
  (orderless-match-face-2                        (:foreground yellow_b :weight 'bold))
  (orderless-match-face-3                        (:foreground cyan :weight 'bold))

  (minibuffer-prompt-end                         (:foreground magenta :background black0))
  (minibuffer-prompt                             (:foreground red_d :background black0 :bold t))

  (window-stool-face (:background gray2))

  ;;treemacs
  (treemacs-window-background-face               (:background black1))
  (treemacs-directory-face                       (:foreground blue1))
  (treemacs-file-face                            (:foreground blue1))
  ;; (treemacs-nerd-icons-file-face                 (:inherit 'treemacs-file-face))
  ;; (treemacs-nerd-icons-root-face                 (:inherit 'treemacs-directory-face))
  (treemacs-git-added-face                       (:foreground cyan_d))
  (treemacs-git-renamed-face                     (:foreground red_b))
  (treemacs-git-ignored-face                     (:foreground gray2 :italic t))
  (treemacs-git-unmodified-face                  (:foreground white3))
  (treemacs-git-untracked-face                   (:foreground green_b))
  (treemacs-git-modified-face                    (:foreground magent_d :italic t))

  (elisp-shorthand-font-lock-face                (:foreground red_b))

  (font-lock-bracket-face                        (:foreground blue0))
  (font-lock-builtin-face                        (:foreground yellow))
  (font-lock-comment-delimiter-face              (:inherit 'font-lock-comment-face))
  (font-lock-comment-face                        (:foreground gray5 :italic t :weight 'thin))
  (font-lock-constant-face                       (:foreground magenta :weight 'semi-bold))
  (font-lock-delimiter-face                      (:foreground red_d))
  (font-lock-doc-face                            (:inherit 'font-lock-comment-face))
  (font-lock-doc-markup-face                     (:inherit 'font-lock-doc-face))
  (font-lock-function-call-face                  (:foreground blue1))
  (font-lock-function-name-face                  (:foreground blue2 :italic t))
  (font-lock-keyword-face                        (:foreground orange_d :weight 'bold))
  (font-lock-misc-punctuation-face               (:foreground yellow_b))
  (font-lock-negation-char-face                  (:foreground red_b))
  (font-lock-number-face                         (:foreground magenta))
  (font-lock-operator-face                       (:foreground red))
  (font-lock-preprocessor-face	                 (:foreground orange))
  (font-lock-property-name-face                  (:foreground white3))
  (font-lock-property-use-face                   (:foreground magent_b))
  (font-lock-punctuation-face                    (:foreground yellow_b :weight 'normal))
  (font-lock-reference-face                      (:foreground yellow))
  (font-lock-regexp-regex-face                   (:foreground red_b))
  (font-lock-regexp-grouping-backslash           (:foreground red_b))
  (font-lock-regexp-grouping-construct           (:foreground red_b))
  (font-lock-string-face                         (:foreground green_b :italic t :weight 'light))
  (font-lock-type-face                           (:foreground yellow_d))
  (font-lock-variable-name-face                  (:foreground cyan))
  (font-lock-variable-use-face                   (:foreground cyan_d))
  (font-lock-warning-face                        (:foreground yellow_b))

  (swift-ts-face-annotation                      (:foreground red_d))
  (swift-ts-face-annotation.builtin              (:foreground cyan_d))
  (swift-ts-face-annotation.type                 (:foreground green))

  (swift-ts-face-punctuation.type                (:inherit 'font-lock-punctuation-face))
  (swift-ts-face-compiler                        (:inherit 'font-lock-builtin-face))
  (swift-ts-face-constructor.call                (:inherit 'font-lock-function-call-face))

  (swift-ts-face-face-label                      (:foreground green))
  (swift-ts-face-method.call                     (:inherit 'font-lock-function-call-face))
  (swift-ts-face-method.name                     (:inherit 'font-lock-function-name-face))
  (swift-ts-face-keyword.annotation              (:foreground red_b :background green :weight 'bold))
  (swift-ts-face-keyword.type                    (:inherit 'font-lock-type-face))
  (swift-ts-face-variable.synthesized            (:foreground cyan))
  (swift-ts-face-property.declaration            (:foreground cyan_d))
  

  (eglot-inlay-hint-face (:foreground magenta :background black1 :height 0.8 :weight 'light))
  (eglot-parameter-hint-face (:inherit 'eglot-inlay-hint-face))
  (eglot-type-hint-face (:inherit 'eglot-inlay-hint-face))

  ;;; Rainbow-delimiters
  (rainbow-delimiters-mismatched-face            (:foreground red))
  (rainbow-delimiters-unmatched-face             (:foreground orange))
  (rainbow-delimiters-base-error-face            (:foreground red))
  (rainbow-delimiters-base-face                  (:foreground green_b))

  (rainbow-delimiters-depth-1-face               (:foreground red))
  (rainbow-delimiters-depth-2-face               (:foreground magent_d))
  (rainbow-delimiters-depth-3-face               (:foreground blue0))
  (rainbow-delimiters-depth-4-face               (:foreground cyan_d))
  (rainbow-delimiters-depth-5-face               (:foreground gray5))
  (rainbow-delimiters-depth-6-face               (:foreground blue1))
  (rainbow-delimiters-depth-7-face               (:foreground red_d))
  (rainbow-delimiters-depth-8-face               (:foreground orange))
  (rainbow-delimiters-depth-9-face               (:foreground blue0))

  (punch-line-evil-normal-face  (:foreground black0 :background orange_b :weight 'semi-bold))
  (punch-line-evil-visual-face  (:foreground black0 :background red :weight 'semi-bold))
  (punch-line-evil-replace-face (:foreground black0 :background magenta :weight 'semi-bold))
  (punch-line-evil-insert-face  (:foreground black0 :background green_b :weight 'semi-bold))
  (punch-line-project-face      (:foreground magenta :weight 'bold))
  (punch-line-buffer-name-face  (:foreground white3 :weight 'bold))
  (punch-line-time-face         (:foreground black0))
  (punch-line-major-mode-face   (:foreground gray5))
  (punch-line-separator-face    (:foreground gray0 :weight 'thin))

  (term                     (:background black0 :foreground magenta))
  (term-color-black         (:background black0 :foreground black0))
  (term-color-yellow        (:background yellow :foreground yellow))
  (term-color-blue          (:background blue1 :foreground blue1))
  (term-color-green         (:background green :foreground green))
  (term-color-red           (:background red :foreground red))

  (ansi-color-bright-cyan       (:background cyan :foreground cyan))
  (ansi-color-blue              (:background blue0 :foreground blue0))
  (ansi-color-bright-magenta    (:background red :foreground red))

  (evil-mc-region-face          (:background cyan_d :foreground black0))
  (evil-mc-cursor-default-face  (:background blue0 :foreground black0))
  (evil-mc-cursor-bar-face      (:background black0 :foreground cyan_d))
  (evil-mc-cursor-hbar-face     (:background cyan_d :foreground cyan_d))

  (evil-ex-lazy-highlight        (:background red_d :foreground black0 :italic t))

  (highlight-symbol-face        (:foreground yellow_d :weight 'bold :underline t))

  (smerge-base		        (:background magenta))
  (smerge-markers		(:background black1 :foreground magenta))

  (smerge-upper			(:background black2))
  (smerge-lower			(:background black2))
  (smerge-refined-change        (:background blue1))
  (smerge-refined-removed	(:background orange :strike-through t))
  (smerge-refined-added 	(:background green :foreground black0))

  (markdown-header-delimiter-face (:foreground magenta))
  (markdown-header-face-1 (:height 1.15 :foreground red :weight 'bold))
  (markdown-header-face-2 (:height 1.12 :foreground red_b :weight 'semi-bold))
  (markdown-header-face-3 (:height 1.1 :foreground cyan_d :weight 'semi-bold))
  (markdown-header-face-4 (:height 1.08 :foreground orange :weight 'normal))
  (markdown-list-face (:foreground yellow_b :bold t))
  (markdown-markup-face (:foreground red_d))
  (markdown-inline-code-face (:foreground magenta :background black2 :weight 'normal :italic t))
  (markdown-code-face (:foreground cyan :weight 'normal))
  (markdown-pre-face (:foreground blue0))

  (elfeed-search-tag-face (:foreground blue1))

  (git-gutter:added (:foreground green_b))
  (git-gutter:deleted (:foreground red_b))
  (git-gutter:modified (:foreground magenta))

  (flycheck-overlay-error (:background black0 :foreground red_b :height 0.9))
  (flycheck-overlay-warning (:background black0 :foreground yellow_d :height 0.9))
  (flycheck-overlay-info (:background black0 :foreground blue0 :height 0.9))
  (flycheck-overlay-marker (:foreground red_b :italic t :height 0.9))

  ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nordic)
;;; nordic-theme.el ends here
