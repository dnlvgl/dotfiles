;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-

;; This file uses outline-minor-mode for section folding.
;; Use M-g o (consult-outline) to jump between sections.

;;; Custom Variables

;; Setup misc variables to use later.
;; 'Per System' variables are loaded via an dnl-custom-vars.el, if this does not
;; exist use fallback values

;; use dnl prefixed file,
(setq custom-file (expand-file-name "dnl-custom-vars.el" user-emacs-directory))

(cond ((file-exists-p custom-file)
        ;; if custom file exists load it and its values
        (load custom-file))
       (t
        ;; if not use standard values
        ;; cons: this means these need to be tracked in two places
        (defvar dnl/default-font-size 130)
        (defvar dnl/org-agenda-path "~/Sync/org/")
        ))

(defvar dnl/default-font "Fira Code") ;; sudo dnf install fira-code-fonts
(defvar dnl/indent-width 2)

;;; Package Management

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			                   ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
;; always download packages if they don't already exist
(setq use-package-always-ensure t)

;;; Keyboard Bindings

;; General rebindings

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Jump to emacs config
(defun dnl/jump-to-init-file ()
  "Open the Emacs init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c e") #'dnl/jump-to-init-file)

;; Show buffer menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Kill line backward
;; alternative: https://github.com/purcell/emacs.d/blob/485a3af948db4671baf73f14bced123bae3112f3/init-editing-utils.el#L147
(defun dnl/kill-line-backward ()
  "Kill from point to the beginning of the line, then re-indent."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))
(global-set-key (kbd "C-<backspace>") #'dnl/kill-line-backward)

;; jump to file under cursor, problems without file ending, use xref-find-definition or projectile?
(global-set-key (kbd "C-c f") 'find-file-at-point)

;;;; which-key

;; Show possible keybindings after short delay

(use-package which-key
  :ensure nil
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;;; General Config

;; At work emacs does not see all shell path by default under macos. This fixes
;; stuff like lsp not being found

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x pgtk))
    (setq exec-path-from-shell-shell-name "fish")
    (setq exec-path-from-shell-arguments '("-l" "-i"))
    (exec-path-from-shell-initialize)))

;;;; UI

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ;; Disable visible scrollbar
(tool-bar-mode -1)          ;; Disable the toolbar
;; (menu-bar-mode -1)          ; Disable the menu bar
(tooltip-mode -1)           ;; Disable tooltips
(set-fringe-mode 10)        ;; Give some breathing room
(setq ring-bell-function 'ignore) ;; no audio bell
(blink-cursor-mode 0)       ;; no blinking cursor

;; enable right click context menu
(context-menu-mode t)

;; show line and column numbers globaly
(column-number-mode)
(global-display-line-numbers-mode t)

;; but don't show line numbers in certain modes
(defun dnl/disable-line-numbers ()
  "Disable line numbers in the current buffer."
  (display-line-numbers-mode 0))
(dolist (mode '(org-mode-hook
	              term-mode-hook
	              eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode #'dnl/disable-line-numbers))

;; transform yes-or-no questions into y-or-n
(setq use-short-answers t)

;; use olivetti mode for writing
(use-package olivetti
  :init
  (setq olivetti-body-width 80)
  (setq olivetti-style 'fancy))

;; Pick up the changes from disk automatically
(global-auto-revert-mode 1)

;;;; Theme

;; Load custom theme from the themes folder

(add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))
(load-theme 'calcite-light t)


(defun dnl/reload-theme ()
  "Reload the current theme without confirmation."
  (interactive)
  (let ((current-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (mapc (lambda (theme) (load-theme theme t)) current-themes)))

;; Switch theme according to system theme. Linux: current used Emacs 30.2 uses
;; GTK3 Theme and needs e.g. Legacy GTK3 Switcher Extension +
;; ~dnf install adw-gtk3-theme~ and set it in gnome tweaks

(use-package auto-dark
  :custom
  ;; list 1. dark theme 2. light theme
  (auto-dark-themes '((calcite-dark) (calcite-light)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil)
  :init (auto-dark-mode))

;; Icons similiar to all-the-icons, e.g used by doom-modeline

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;;;; Font

(set-face-attribute 'default nil :font dnl/default-font :height dnl/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font dnl/default-font :height  dnl/default-font-size)

;;;; Modeline

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;;; Saving and Backup

;; All backup files get sent to ~/.config/emacs/backups/ to not clutter the
;; file system

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;;;; Project Management
(use-package project
  :ensure nil
  :bind-keymap ("C-x p" . project-prefix-map))

;;;; Completion

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
         ;; ("C-x p b" . consult-project-buffer)   ;; conflicts with project :bind-keymap on C-x p
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


;; completion style that divides the pattern into space-separated components,
;; and matches candidates that match all of the components in any order
;; e.g.: searching "region indent" will match with "indent-region"

(use-package orderless
  :init
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
  :ensure nil
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

;; Sort of right-click contextual menu for Emacs, accessed through the
;; embark-act command (which you should bind to a convenient key), offering you
;; relevant actions to use on a target determined by the context

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

;; use corfu for completion at point

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)) ; shows documentation after `corfu-popupinfo-delay'

;;;; Movement

;; Enable windmove to use ~SHIFT+arrow~ to move between windows

(windmove-default-keybindings)

;; Switches cursor automatically to new splitted window

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

;; Move around with avy. Lots of feature, lets start simple with jumping to
;; chars. Would be cool to have a

(use-package avy
      :bind
    (("C-z" . avy-goto-char)))

;;;; Dashboard

(use-package dashboard
  ;; only show dasboard if opening emacs without file
  :if (< (length command-line-args) 2)
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "YOUR ADD HERE")
  (setq dashboard-set-footer nil)
  (setq dashboard-startup-banner (expand-file-name "dashboard-logos/orb.png" user-emacs-directory))
  (setq dashboard-items '((bookmarks . 10)
                          (agenda . 5)
                          ;;(projects . 5)
                          (recents . 0)
                          (registers . 5)))
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

;;;; Terminal

;; Use Vterm, faster than term. Needed to install ~sudo dnf install cmake
;; libtool~. Needs some more shell-side configuration

(use-package vterm
  :commands vterm-mode
  :bind
  (("C-c t t" . vterm))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-shell "/usr/bin/fish"))

;;;; File Management

;; Using dired for file management, some basic tweaks

(use-package dired
  :ensure nil
  :config (setq dired-listing-switches "-agho --group-directories-first"))

(when (eq system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

;;; File Editing

;;;; Indentation

;; Use 2 spaces by default

(setq-default tab-width dnl/indent-width)
(setq-default indent-tabs-mode nil)
(setq-default js-indent-level dnl/indent-width)
(setq-default css-indent-offset dnl/indent-width)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;;; Commenting Lines

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;;; Brackets

;; Autoclose brackets
(electric-pair-mode 1)

;; Highlight brackets
(setq show-paren-delay 0)
(show-paren-mode 1)

;;;; Move current line or region up and down

(use-package move-text
;; meta shift to not collide with orgmode
  :bind (("M-S-<up>" . move-text-up)
        ("M-S-<down>" . move-text-down)))

;;; Org Mode

(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
        ("C-c c" . org-capture))
  :custom
  ;; Visual settings
  (org-ellipsis " ▾")
  ;; Set custom TODO states
  (org-todo-keywords '((sequence "TODO(t)" "DOING(n)" "BLOCKED(b)" "|" "DONE(d)" "DELEGATED(g)" "CANCELLED(c)")))
  ;; Start agenda + calendar on Monday
  (org-agenda-start-on-weekday 1)
  (calendar-week-start-day 1)
  ;; Don't show done items in agenda
  (org-agenda-skip-scheduled-if-done t)
  ;; Add timestamp once task is done
  (org-log-done t)
  ;; Set source for agenda
  ;; Comma unquote, rather use: (org-agenda-files (list dnl/org-agenda-path))
  (org-agenda-files `(,dnl/org-agenda-path))
  ;; Theme source blocks like in native mode
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  :config
  ;; Default capture templates for personal use. On my work machine,
  ;; dnl-custom-vars.el sets org-capture-templates before this runs,
  ;; so the unless check skips these defaults.
  (unless org-capture-templates
    (setq org-capture-templates
          '(("i" "Inbox" entry (file+headline "~/Sync/org/notes.org" "Inbox")
             "** %?")
            ("t" "Todo" entry (file+headline "~/Sync/org/notes.org" "GTD")
             "** TODO %?")
            ("r" "Recipe" entry (file "~/Sync/org/rezepte.org")
             "* %? %^G \n:PROPERTIES:\n:Quelle:\n:Menge:\n:Dauer:\n:Kalorien:\n:END:\n** Zutaten\n** Zubereitung\n"
             :jump-to-captured t)
            ("b" "Bookmark (Clipboard)" entry (file+headline "~/Sync/org/bookmarks.org" "Captured")
             "** %(dnl/org-web-tools-insert-link-for-clipboard-url)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t))))
  )

(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-star nil))

;;;; Block Templates

;; These templates enable you to type things like =<el= and then hit =Tab= to
;; expand the template. More documentation can be found at the Org Mode Easy
;; Templates documentation page.

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

;;;; Capture Web Bookmarks to orgmode

(use-package org-web-tools)

(defun dnl/org-web-tools-insert-link-for-clipboard-url ()
  "Extend =org-web-tools-inster-link-for-url= to take URL from clipboard or kill-ring"
  (interactive)
  (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))

;;; Git

;; Does it make sense to auto-refresh Magit status buffers?
;; `(setq magit-auto-revert-mode t)`

(use-package magit)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-revert-hook 'diff-hl-update)

  ;; diff-hl-flydiff-mode only diffs buffer content against the last
  ;; save — it does not re-run `git diff` against HEAD, so external
  ;; git state changes (commits, stashes, index updates) won't show
  ;; up.  We hook into focus and window-selection changes to call
  ;; diff-hl-update, which does shell out to git.

  ;; fires once when Emacs gains focus — this updates the current buffer immediately when you alt-tab back.                    
  (add-function :after after-focus-change-function
                (lambda ()
                  (when (frame-focus-state)
                    (when diff-hl-mode (diff-hl-update)))))

  ;; fires when you switch between windows (splits) — this updates buffers you navigate to after returning
  (add-hook 'window-selection-change-functions
            (lambda (_)
              (when diff-hl-mode
                (diff-hl-update)))))

;;; Language Support

;;;; Web

;;;;; HTML

;; Minimal setup for HTML and mixed modes

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.xml\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset dnl/indent-width)
  (setq web-mode-code-indent-offset dnl/indent-width)
  (setq web-mode-css-indent-offset dnl/indent-width)
  (setq web-mode-script-padding dnl/indent-width))

;;;;; Emmet

;; Use emmet to complete HTML abbreviations via ~C-j~. So ~div~ becomes
;; ~<div></div>~

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-ts-mode . emmet-mode))
  :init
  ;; toggle autocompletion on inline css
  (add-hook 'web-mode-before-auto-complete-hooks
    (lambda ()
     (let ((web-mode-cur-language
            (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "css")
           (setq emmet-use-css-transform t)
           (setq emmet-use-css-transform nil))))))

;;;;; JS/TS

;; Syntax Highlighting for Javascript + Typescript

(use-package typescript-ts-mode
  :ensure nil
  :after eglot
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset dnl/indent-width))

(use-package js-ts-mode
  :ensure nil ; derived mode
  :after eglot
  :mode (("\\.js\\'" . js-ts-mode)))

;;;;; CSS

(use-package css-ts-mode
  :ensure nil ;derived mode
  :after eglot
  :mode "\\.css\\'"
  :config
  (setq css-ts-mode-indent-offset dnl/indent-width))

;;;;; JSON

(use-package json-ts-mode
  :ensure nil
  :after eglot
  :mode "\\.json\\'"
  :config
  (setq json-ts-mode-indent-offset dnl/indent-width))

;;;; Lisp Stuff

;; fedora defaults to older guile 2 version, need to explicitly set the used
;; guile version ~sudo dnf install guile30~

(use-package geiser
  :config
  (setq geiser-guile-binary "guile3.0")
  )

(use-package geiser-guile
  :after geiser)

;;;; Markdown

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;;; Go

(use-package go-ts-mode
  :ensure nil
  :after eglot
  :mode "\\.go\\'"
  :config
  (setq go-ts-mode-indent-offset tab-width))

;;;; LSP + Parser

;;;;; treesitter

;; treesit-auto to install treesitter major modes automatically

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;; eglot

(use-package eglot
  :ensure nil
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure))
  :config
  (defun dnl/eglot-format-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer nil t))
  (dolist (hook '(go-ts-mode-hook
                 typescript-ts-mode-hook
                 tsx-ts-mode-hook
                 js-ts-mode-hook
                 css-ts-mode-hook))
    (add-hook hook #'dnl/eglot-format-on-save)))

;;; RSS

(use-package elfeed)


(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Sync/org/bookmarks.org")))

;;; Misc

;;; init.el ends here
