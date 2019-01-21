; Init --- Initial file for my emacs configuration

;;; Code:

(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq custom-file "~/.emacs.d/custom.el"
      inhibit-startup-screen t
      visible-bell t
      sentence-end-double-space nil
      make-backup-files nil
      default-frame-alist '((fullscreen . maximized)))

(setq-default apropos-sort-by-scores t
              frame-title-format '("%f [%m]")
              display-line-numbers-type 'visual
	      display-line-numbers-current-absolute t
	      display-line-numbers-width 4
	      display-line-numbers-widen t
              indent-tabs-mode nil)

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t) ;; Ensure Dropbox files exists for this (org mode agenda)
(winner-mode)
(electric-indent-mode +1)
(show-paren-mode 1)

;; GPG
(setq epg-gpg-program "gpg2")

;; Use hippie expand rather
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x C-r") 'find-file-read-only)

;; Packages

(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package all-the-icons :ensure t)

;; Ensure there is a way to only access open buffers
(defun ido-switch-buffer-with-virtual-buffers ()
  (interactive)
  (let ((ido-use-virtual-buffers t))
    (ido-switch-buffer)))

(defun ido-switch-buffer-without-virtual-buffers ()
  (interactive)
  (let ((ido-use-virtual-buffers nil))
    (ido-switch-buffer)))

(use-package idomenu
  :ensure t
  :bind ("M-i" . idomenu))

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 15)
  (recentf-mode 1))

(use-package ido
  :demand t ;; demand for ido everywhere, since there is a bind
  :bind (("C-x C-b" . ido-switch-buffer-with-virtual-buffers)
         ("C-x b" . list-buffers))
  :config
  (setq ido-use-virtual-buffers t
        ido-enable-flex-matching t
        ido-everywhere t
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always
        ido-ignore-extensions t
        ido-enable-tramp-completion t)
  (ido-mode 1))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command--last-typed)))

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
        projectile-completion-system 'ido
        projectile-file-exists-remote-cache-expire nil)
  (projectile-mode +1))

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

;; erc
(use-package erc
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
  :config
  (setq erc-echo-notices-in-minibuffer-flag t)
  (setq erc-auto-reconnect nil)
  (setq erc-lurker-hide-list '("JOIN" "QUIT"))
  (setq erc-lurker-threshold-time 3600)
  (setq erc-input-line-position -2))

(defun erc-connect ()
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 6697 :nick "peirama"))

(use-package buffer-move :ensure t)
;; TODO: Find a better keybinding for this
;; (use-package hydra :ensure t)
;; (defhydra hydra-windmove (global-map "C-x C-o")
;;   "movement"
;;   ("o" other-window "other")
;;   ("B" windmove-left "left")
;;   ("B" buf-move-left "buffer-left")
;;   ("f" windmove-right "right")
;;   ("F" (lambda () (interactive) (split-window-right)) "split-right")
;;   ("p" windmove-up "up")
;;   ("P" buf-move-up "buffer-up")
;;   ("n" windmove-down "down")
;;   ("N" (lambda () (interactive) (split-window-below)) "split-down")
;;   ("x" delete-window "delete"))

;; Auth stuff
(setq epg-gpg-program "gpg2")
(setq auth-sources
      '((:source "~/Dropbox/secrets/.authinfo.gpg")))

(use-package org-pomodoro :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-global-mode))
(use-package yasnippet-snippets :ensure t)

(use-package define-word
  :ensure t)

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package restclient
  :ensure t)

(use-package spotify
  :ensure t
  :bind (("C-c s n" . spotify-next)
         ("C-c s p" . spotify-pause)
         ("C-c s c" . spotify-current))
  :config
  (spotify-enable-song-notifications))

(use-package dired-filter
  :ensure t)

(load "~/.emacs.d/programming.el")
(load "~/.emacs.d/functions.el")

;;; init.el ends here
