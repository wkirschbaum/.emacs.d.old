; Init --- Initial file for my emacs configuration
;; Version 27.0.50


;;; Code:

(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(setq inhibit-startup-screen t
      make-backup-files nil
      default-frame-alist '((fullscreen . maximized)))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

(defun flash-mode-line ()
  "Flashes the mode-line."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq-default apropos-sort-by-scores t
              frame-title-format '("%f [%m]")
              display-line-numbers-type 'visual
	      display-line-numbers-current-absolute t
	      display-line-numbers-width 4
	      display-line-numbers-widen t
              indent-tabs-mode nil)

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t) ;; Ensure Dropbox files exists for this (org mode agenda)
(winner-mode)
(electric-indent-mode +1)
(show-paren-mode 1)

;; Use hippie expand rather
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x C-r") 'find-file-read-only)

;; Packages

(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package all-the-icons
  :ensure t)

;; --- START Searching and navigation

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

(use-package ido
  :demand t ;; demand for ido everywhere, since there is a bind
  :config
  (setq ido-use-virtual-buffers t
        ido-everywhere t
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always
        ido-ignore-extensions t
        ido-enable-trap-completion t)
  (ido-mode 1))

(use-package flx-ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (flx-ido-mode))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))


;; --- END Searching and navigation

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :config
  (setq recentf-max-menu-items 15)
  (recentf-mode 1))

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
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package forge
  :ensure t)

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
  :bind-keymap ("C-x p" . projectile-command-map)
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

(setq auth-sources '((:source "~/Dropbox/secrets/.authinfo.gpg")))

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

(use-package browse-at-remote
  :ensure t)

;; (use-package zenburn-theme
;;   :ensure t)

;; (set-face-background hl-line-face "gray22")
;; (set-frame-font "DejaVu Sans Mono 12" nil t)

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))


(with-system darwin
  (setq-default ns-alternate-modifier 'super
                ns-command-modifier 'meta
                ns-option-modifier 'meta)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(load "~/.emacs.d/programming.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/org.el")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here
