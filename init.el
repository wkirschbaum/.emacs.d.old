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

(setq default-frame-alist '((fullscreen . maximized)))

(setq-default frame-title-format '("%f [%m]"))

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

;; Ensure there is a way to only access open buffers
(defun ido-switch-buffer-with-virtual-buffers ()
  (interactive)
  (let ((ido-use-virtual-buffers t))
    (ido-switch-buffer)))

(defun ido-switch-buffer-without-virtual-buffers ()
  (interactive)
  (let ((ido-use-virtual-buffers nil))
    (ido-switch-buffer)))

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 15)
  (recentf-mode 1))

(use-package ido
  :bind (("C-x C-b" . ido-switch-buffer-with-virtual-buffers)
         ("C-x b" . list-buffers))
  :config
  (setq ido-use-virtual-buffers t
        ido-enable-flex-matching t
        ido-everywhere t)
  (ido-mode t))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command--last-typed)))

;; Make space turn into dash like normal M-x
(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command 
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))

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
  (erc :server "irc.freenode.net" :port 6667 :nick "peirama"))

(use-package buffer-move :ensure t)
(use-package hydra :ensure t)
(defhydra hydra-windmove (global-map "C-x C-o")
  "movement"
  ("o" other-window "other")
  ("b" windmove-left "left")
  ("B" buf-move-left "buffer-left")
  ("f" windmove-right "right")
  ("F" (lambda () (interactive) (split-window-right)) "split-right")
  ("p" windmove-up "up")
  ("P" buf-move-up "buffer-up")
  ("n" windmove-down "down")
  ("N" (lambda () (interactive) (split-window-below)) "split-down")
  ("x" delete-window "delete"))
         
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/programming.el")

;;; init.el ends here
