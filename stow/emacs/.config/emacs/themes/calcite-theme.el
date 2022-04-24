(deftheme calcite
  "Distractionless light theme with minimal syntax highlighting")

;; use `M-x list-faces-display` to check which faces to customize


(let ((fg "#424242")
      (bg "#f1f1f1")

      (fg-light "#ddddd8") ;; remove
      
      (blue "#008ec4")
      (green "#10a778")
      (cyan "#007a7a") ;;TODO
      (yellow "#7c730e") ;;TODO
      (red "#c30771")
      (purple "#523c79")
      (purple-light "#e8e3f0")
      )

  (custom-theme-set-faces
   'calcite

   ;; general faces
   `(default ((t (:background ,bg :foreground ,fg))))
   `(button ((t (:foreground ,fg :underline t))))
   `(cursor ((t (:background ,fg :foreground "white smoke"))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t))))
   `(font-latex-bold-face ((t (:foreground ,fg))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground "#a9a9a9"))))
   `(font-lock-builtin-face ((t (:background ,bg :foreground ,fg))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,blue))))
   `(font-lock-comment-face ((t (:foreground ,blue :background ,bg))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-doc-face ((t (:foreground ,fg :weight semi-bold))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :underline nil))))
   `(font-lock-warning-face ((t (:foreground ,red :weight bold))))
   `(fringe ((t (:background ,bg ))))
   `(isearch ((t (:background "#eeeee8" :foreground ,fg))))
   `(line-number ((t (:background ,bg :foreground: ,fg ))))
   `(line-number-current-line ((t (:background ,bg :foreground ,purple ))))
   `(link ((t (:foreground ,fg))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
   ;; `(mode-line ((t (:background ,bg :foreground ,fg :height 0.8))))
   ;; `(mode-line-buffer ((t (:foreground ,fg :weight bold))))
   ;; `(mode-line-inactive ((t (:background ,bg :foreground ,bg :height 0.8))))
   ;; `(mode-line-minor-mode ((t (:weight ultra-light))))
   ;; `(modeline ((t (:background ,bg :foreground ,fg :height 0.8))))
   `(region ((t (:background "#eeeee8" :foreground ,fg))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(whitespace-line ((t (:background ,purple :foreground ,fg))))

   ;; orgmode
   `(org-agenda-date ((t (:foreground ,fg :height 1.2))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold :height 1.4))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:foreground ,fg :background ,purple-light))))
   `(org-block-begin-line ((t (:foreground ,fg :background ,purple-light))))
   `(org-block-end-line ((t (:foreground ,fg :background ,purple-light))))
   `(org-block-begin-line ((t (:foreground ,fg-light))))
   `(org-block-end-line ((t (:foreground ,fg-light))))
   `(org-date ((t (:foreground ,fg) :underline)))
   `(org-document-info-keyword ((t (:foreground ,fg))))
   `(org-document-title ((t (:foreground ,purple :height 1.0 :underline nil))))
   `(org-done ((t (:foreground ,green))))
   `(org-ellipsis ((t (:foreground ,fg))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-level-1 ((t (:foreground ,green :weight semi-bold :height 1.1))))
   `(org-level-2 ((t (:foreground ,green :weight semi-bold :height 1.1))))
   `(org-level-3 ((t (:foreground ,green :weight semi-bold :height 1.1))))
   `(org-level-4 ((t (:foreground ,green :weight semi-bold :height 1.1))))
   `(org-level-5 ((t (:foreground ,green :weight semi-bold :height 1.1))))
   `(org-level-6 ((t (:foreground ,green :weight semi-bold :height 1.1))))
   `(org-level-7 ((t (:foreground ,green :weight semi-bold :height 1.1))))
   `(org-level-8 ((t (:foreground ,green :weight semi-bold :height 1.1))))
   `(org-link ((t (:foreground ,fg :underline t))))
   `(org-meta-line ((t (:foreground ,fg)))) 
   `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-sexp-date ((t (:foreground ,fg))))
   `(org-special-keyword ((t (:foreground ,fg))))
   `(org-todo ((t (:foreground ,purple :background, purple-light))))
   `(org-verse ((t (:inherit org-block :slant italic))))

   ;; vc
   `(diff-hl-change ((t (:background ,blue :foreground ,blue ))))
   `(diff-hl-delete ((t (:background ,red :foreground ,red))))
   `(diff-hl-insert ((t (:background ,green :foreground, green ))))
   ;; TODO add magit
   ))

(provide-theme 'calcite)

(provide 'calcite-theme)
