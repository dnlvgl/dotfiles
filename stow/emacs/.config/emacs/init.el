(defvar dnl/emacs-conf-org-path "~/.config/emacs/emacs.org")
(defvar dnl/emacs-conf-init-path "~/.config/.emacs/init.el")
(defvar dnl/default-font "Iosevka")
(defvar dnl/default-variable-font "Cantarell")
(defvar dnl/default-font-size 130)
(defvar dnl/default-variable-font-size 130)
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
  ;; restart org mode, else there are some hook issues
  (org-mode-restart))

(global-set-key (kbd "C-c r") 'dnl/reload-init-file) 

;; Show buffer menu
(global-set-key (kbd "C-x C-b") 'buffer-menu)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
;; (menu-bar-mode -1)          ; Disable the menu bar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(setq ring-bell-function 'ignore) ;; no audio bell

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
  :init
  (load-theme 'spacemacs-light t))

;;  (use-package doom-themes
;;    :config
;;    (load-theme 'doom-one t))

(set-face-attribute 'default nil :font dnl/default-font :height dnl/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font dnl/default-font :height  dnl/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font dnl/default-variable-font :height dnl/default-font-size :weight 'regular)

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(use-package ivy
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
    :bind (
	   ("C-x C-f" . counsel-find-file)
	   ("C-x b" . counsel-ibuffer)
	   ;; ("M-y" . counsel-yank-pop)
	   ("M-x " . counsel-M-x))
    :config
    (counsel-mode 1))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Autoclose brackets
(electric-pair-mode 1)

;; Highlight brackets
(setq show-paren-delay 0)
(show-paren-mode 1)

(defun dnl/org-font-setup ()
  ;; Set faces for heading levels, also use the variable font
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font dnl/default-variable-font :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun dnl/org-mode-setup ()
  (org-indent-mode)
  ;; use variable width font, overwrite some of it via dnl/org-font-setup which is run later
  (variable-pitch-mode 1)
  ;; make sure we have linebreaks in org mode
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . dnl/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
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

  (dnl/org-font-setup))

;; use custom bullets instead of stars for headings
(use-package org-bullets
:after org
:hook (org-mode . org-bullets-mode)
:custom
(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(visual-fill-column org-bullets evil-nerd-commenter counsel ivy doom-modeline emojify spacemacs-theme which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
