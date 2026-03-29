;;; init.el --- thin loader -*- lexical-binding: t -*-
(require 'org)
(org-babel-load-file
  (expand-file-name "emacs.org" user-emacs-directory))
