;; use dnl prefixed file, 
(setq custom-file "~/.config/emacs/dnl-custom-vars.el")

(cond ((file-exists-p custom-file)
        ;; if custom file exists load it and its values
        (load custom-file))
       (t
        ;; if not use standard values
        ;; cons: this means these need to be tracked in two places
        (defvar dnl/default-font-size 130)
        (defvar dnl/org-agenda-path "~/Sync/org/")
        ))
 

 (defvar dnl/emacs-conf-path "~/.config/emacs/")
 (defvar dnl/emacs-conf-org-path "~/.config/emacs/emacs.org")
 (defvar dnl/emacs-conf-init-path "~/.config/.emacs/init.el")
 (defvar dnl/default-font "Iosevka SS08")
 (defvar dnl/indent-width 2)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			                   ("org" . "https://orgmode.org/elpa/")
			                   ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; always download packages if they don't already exist
(setq use-package-always-ensure t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Jump to emacs config
(global-set-key (kbd "C-c e") '(lambda ()
			                           (interactive)
			                           (find-file dnl/emacs-conf-org-path)))

;; Reload Emacs Config
(defun dnl/reload-init-file ()
  (interactive)
  ;; export config to init.el
  (org-babel-tangle-file dnl/emacs-conf-org-path dnl/emacs-conf-init-path)
  ;; reload init file
  (load-file user-init-file)
  ;; restart org mode, else there are some hook issues. Disabled as also annoying if not changing stuff for org, run manually if needed
  ;;(org-mode-restart)
  )

(global-set-key (kbd "C-c r") 'dnl/reload-init-file) 

;; Show buffer menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Kill line backward
;; alternative: https://github.com/purcell/emacs.d/blob/485a3af948db4671baf73f14bced123bae3112f3/init-editing-utils.el#L147
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ;; Disable visible scrollbar
(tool-bar-mode -1)          ;; Disable the toolbar
;; (menu-bar-mode -1)          ; Disable the menu bar
(tooltip-mode -1)           ;; Disable tooltips
(set-fringe-mode 10)        ;; Give some breathing room
(setq ring-bell-function 'ignore) ;; no audio bell
(blink-cursor-mode 0)       ;; no blinking cursor

;; show line and column numbers globaly
(column-number-mode)
(global-display-line-numbers-mode t)

;; but don't show line numbers in certain modes
(dolist (mode '(org-mode-hook
	              term-mode-hook
	              eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; transform yes-or-no questions into y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; use olivetti mode for writing
(use-package olivetti )

;; (Use-package spacemacs-theme
;;  :defer t
;;  :disabled
;;  :init
;;  (load-theme 'spacemacs-light t))

(use-package mindre-theme
  :custom
  (mindre-use-more-bold nil)
  :config
  (load-theme 'mindre t))

;;(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
;;(load-theme 'calcite t)

(set-face-attribute 'default nil :font dnl/default-font :height dnl/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font dnl/default-font :height  dnl/default-font-size)

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq backup-directory-alist '(("." . "~/.config/emacs/backups/")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

(use-package ivy
  :disabled
  :diminish
  :bind (("C-s" . swiper)
	       :map ivy-minibuffer-map
	       ("TAB" . ivy-alt-done)	
	       ("C-l" . ivy-alt-done)
	       ("C-j" . ivy-next-line)
	       ("C-k" . ivy-previous-line)
	       :map ivy-switch-buffer-map
	       ("C-k" . ivy-previous-line)
	       ("C-l" . ivy-done)
	       ("C-d" . ivy-switch-buffer-kill)
	       :map ivy-reverse-i-search-map
	       ("C-k" . ivy-previous-line)
	       ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :disabled
  :bind (
	       ("C-x C-f" . counsel-find-file)
	       ("C-x b" . counsel-ibuffer)
	       ;; ("M-y" . counsel-yank-pop)
	       ("M-x " . counsel-M-x))
  :config
  (counsel-mode 1))

;; Completion systems based on the standard Emacs completing-read API
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ;; ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode))


;; completion style that divides the pattern into space-separated components, and matches candidates that match all of the components in any order
;; e.g.: searching "region indent" will match with "indent-region"
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Vertical completion UI based on the default completion system
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Sort of right-click contextual menu for Emacs, accessed through the embark-act command (which you should bind to a convenient key), offering you relevant actions to use on a target determined by the context
(use-package embark
  :bind
  (("C-." . embark-act)         ;; C-. by default but does not work on gnome, run ~gsettings set org.freedesktop.ibus.panel.emoji hotkey "[]"~ to get it back
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(windmove-default-keybindings)

(defun split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package dashboard
  ;; only show dasboard if opening emacs without file
  :if (< (length command-line-args) 2)
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "YOUR ADD HERE")
  (setq dashboard-set-footer nil)
  (setq dashboard-startup-banner "~/.config/emacs/dashboard-logos/orb.png")
  ;; (setq dashboard-startup-banner "~/.config/emacs/dashboard-logos/rms-pepper.png")  
  (setq dashboard-items '((bookmarks . 10)
                          (agenda . 5)                            
                          ;;(projects . 5)
                          (recents . 0)
                          (registers . 5)))
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

(use-package vterm
  ;;:when (bound-and-true-p module-file-suffix)
  :commands vterm-mode
  :init
  ;;(add-hook 'vterm-mode-hook
  ;;          (lambda ()
  ;;            (setq confirm-kill-processes nil
  ;;                  hscroll-margin 0)))
  :bind
  (("C-c t t" . vterm))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-shell "/usr/bin/fish"))

(use-package dired
  :ensure nil
  :config (setq dired-listing-switches "-agho --group-directories-first"))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Autoclose brackets
;; (electric-pair-mode 1)

;; Highlight brackets
(setq show-paren-delay 0)
(show-paren-mode 1)

(defun dnl/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun dnl/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; meta shift to not collide with orgmode
(global-set-key [(meta shift up)]  'dnl/move-line-up)
(global-set-key [(meta shift down)]  'dnl/move-line-down)

(defun dnl/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . dnl/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  ;; set custom todo states
   (setq org-todo-keywords 
      '((sequence "TODO(t)" "NEXT(n)" "BLOCKED(b)" "SOMEDAY(s)" "PROJ(p)"  "|" "DONE(d)" "CANCELLED(c)")))

  ;; shortcut for agenda
  (global-set-key (kbd "C-c a") 'org-agenda)
  ;; don't show done items in agenda
  (setq org-agenda-skip-scheduled-if-done t)

  ;; set to same size as visual-fill-mode TODO this could be a variable
  ;; (setq org-tags-column 100)

  (setq org-image-actual-width nil)

  ;; no indentation, all text starts at same position
  ;; (setq org-adapt-indentation nil)

  ;; set source for agenda
  ;; find out why to use this ~`(,var)~ syntax
  (setq org-agenda-files `(,dnl/org-agenda-path))

  ;; theme source blocks like in native mode
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0)

  ;;(dnl/org-font-setup)

  ;; capture keybind and templates
  (define-key global-map "\C-cc" 'org-capture)

  ;; collection of examples: https://www.reddit.com/r/emacs/comments/7zqc7b/share_your_org_capture_templates/
  ;; work tracking: https://yannesposito.com/posts/0015-how-i-use-org-mode/index.html https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
  (setq org-capture-templates
      '(
        ("i" "Inbox" entry (file+headline "~/Sync/org/notes.org" "Inbox")
         "** %?")
        ("t" "Todo" entry (file+headline "~/Sync/org/notes.org" "GTD")
         "** TODO %?")
        ("r" "Recipe" entry (file "~/Sync/org/rezepte.org")
         "* %? %^G \n:PROPERTIES:\n:Quelle:\n:Menge:\n:Dauer:\n:Kalorien:\n:END:\n** Zutaten\n** Zubereitung\n"
         :jump-to-captured t)
        ("b" "Bookmark (Clipboard)" entry (file+headline "~/Sync/org/bookmarks.org" "Captured")
         "** %(dnl/org-web-tools-insert-link-for-clipboard-url)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t)))
)

;; use custom bullets instead of stars for headings
;;(use-package org-bullets
;;  :after org
;;  :hook (org-mode . org-bullets-mode)
;;  :custom
;;  (org-bullets-bullet-list '("➀" "➁" "➂" "➃" "➄" "➅" "➆" "➇" "➈" "➉")))
  ;;(org-bullets-bullet-list '("●" "○" "◉" "○" "●" "○" "●")))
  ;;(org-bullets-bullet-list '("➊" "➋" "➌" "➍" "➎" "➏" "➐" "➑" "➒" "➓")))


;; use visual-fill-column to center org mode content
(defun dnl/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;;(use-package visual-fill-column
;;  :hook (org-mode . dnl/org-mode-visual-fill))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(use-package org-web-tools
  :ensure t)

(defun dnl/org-web-tools-insert-link-for-clipboard-url ()
  "Extend =org-web-tools-inster-link-for-url= to take URL from clipboard or kill-ring"
  (interactive)
  (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))

(use-package org-roam
  :after org
  :custom (org-roam-directory "~/Sync/org/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

(use-package org-roam-ui
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package magit)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package web-mode
  :config
  :mode (("\\.html\\'" . web-mode)
         ("\\.xml\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         )
  :config
  ;; should this rather live under :custom, see: https://erickgnavar.github.io/emacs-config/#orgdb7ae90
           (setq web-mode-markup-indent-offset dnl/indent-width)
           (setq web-mode-code-indent-offset dnl/indent-width)
           (setq web-mode-css-indent-offset dnl/indent-width)
           (setq web-mode-script-padding dnl/indent-width)
           ;; highlight columns
           ;;(setq web-mode-enable-current-column-highlight t)
           ;;(setq web-mode-enable-current-element-highlight t)
           )

(use-package emmet-mode
  :hook ((web-mode . emmet-mode))
  :init
  ;; toggle autocompletion on inline css
  (add-hook 'web-mode-before-auto-complete-hooks
    '(lambda ()
     (let ((web-mode-cur-language
            (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "css")
           (setq emmet-use-css-transform t)
           (setq emmet-use-css-transform nil))))))

;; js-mode is fine instead of js2-mode as we use treesitter for syntax, so should?! be fine
;; (use-package js-mode
;;   :ensure nil
;;   :custom
;;   (js-indent-level 2)
;;   (js-switch-indent-offset 2))

(use-package typescript-mode
  ;;:mode "\\.ts\\'"
  :custom (typescript-indent-level 2))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))


;; fedora defaults to older guile 2 version, need to explicitly set the used guile version
;; ~sudo dnf install guile30~
(use-package geiser
  :config
  (setq geiser-guile-binary "guile3.0")
  )

(use-package geiser-guile
  :after geiser)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package tree-sitter
  :hook
   (js-mode . tree-sitter-hl-mode)
  (typescript-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package eglot
  :hook ((js-mode typescript-mode) . eglot-ensure))

(use-package elfeed
  :ensure t)


(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Sync/org/bookmarks.org")))
