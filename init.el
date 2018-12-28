;;; Init --- Initial file for my emacs configuration

;;; Code:

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

;; Do not use tabs
(setq-default indent-tabs-mode nil)

;; Packages

(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package ido
  :config
  (ido-mode t))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package dired
  :bind ("C-x C-j" . dired-jump))

(use-package imenu
  :bind ("M-i" . imenu))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package whitespace
  :ensure t
  :config
  (add-hook 'prog-mode 'whitespace-mode)
  (setq whitespace-style '(face tabs tab-mark trailing empty)))

;; Source control
(use-package git-timemachine
  :ensure t
  :defer t)

(use-package magit
  :demand t
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; Searching

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil))

;; Completion

(use-package company
  :ensure t
  :config
  (global-company-mode t))

;; Analysis

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flyspell
  :ensure t
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(provide nil)


;; Programming Modes

(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)

;;; init.el ends here
