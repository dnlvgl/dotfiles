(defvar dnl/emacs-conf-path "~/.config/emacs/")
(defvar dnl/emacs-conf-org-path "~/.config/emacs/emacs.org")
(defvar dnl/emacs-conf-init-path "~/.config/.emacs/init.el")
(defvar dnl/default-font "Iosevka")
;;(defvar dnl/default-variable-font "Cantarell")
(defvar dnl/default-font-size 130)
;;(defvar dnl/default-variable-font-size 130)
;;(defvar dnl/org-path '(""))

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
	              eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; transform yes-or-no questions into y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package spacemacs-theme
 :defer t
 :disabled
 :init
 (load-theme 'spacemacs-light t))

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(load-theme 'calcite t)

(set-face-attribute 'default nil :font dnl/default-font :height dnl/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font dnl/default-font :height  dnl/default-font-size)

;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font dnl/default-variable-font :height dnl/default-font-size :weight 'regular)

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq backup-directory-alist '(("." . "~/.config/emcas/backups/")))
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
  (("M-." . embark-act)         ;; pick some comfortable binding. C-. by default but does not work on gnome
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

(windmove-default-keybindings)

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

(defun dnl/org-mode-setup ()
  (org-indent-mode)
  ;; use variable width font, overwrite some of it via dnl/org-font-setup which is run later
  ;;(variable-pitch-mode 1)
  ;; make sure we have linebreaks in org mode
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

  ;; set source for agenda
  ;;(setq org-agenda-files '("~/Documents/work.org"))
  (setq org-agenda-files '("~/Sync/org/"))

  ;; theme source blocks like in native mode
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0)

  ;;(dnl/org-font-setup)
  )

;; use custom bullets instead of stars for headings
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○" "◉" "○" "●" "○" "●")))


;; use visual-fill-column to center org mode content
(defun dnl/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dnl/org-mode-visual-fill))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(use-package magit)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

;; TODO does not find guile implementation yet, try run repl on scheme file C-x C-e
;;(use-package geiser-guile)

(use-package geiser
  :config
  (setq scheme-program-name "guile")
  ;;(setq geiser-default-implementation '(guile2))
  ;;(setq geiser-active-implementations '(guile2))
  (setq geiser-guile-binary "guile2.2")
  )
