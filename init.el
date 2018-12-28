(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq custom-file "~/.emacs.d/custom.el")

;; General Visual

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(global-hl-line-mode 1)
(setq initial-frame-alist '((fullscreen . maximized)))
(setq default-frame-alist '((fullscreen . maximized)))

;; Line numbers
(setq-default display-line-numbers-type 'visual
	      display-line-numbers-current-absolute t
	      display-line-numbers-width 4
	      display-line-numbers-widen t)

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Behaviour
(setq sentence-end-double-space nil)

;; Update the buffer if the file changes outside of emacs
(global-auto-revert-mode t)

;; Do not generate extra files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Some editor help
(winner-mode)
(electric-indent-mode +1)
(show-paren-mode 1)

;; Packages

(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package ido
  :config
  (ido-mode t))

(use-package dired
  :bind ("C-x C-j" . dired-jump))

(use-package imenu
  :bind ("M-i" . imenu))
