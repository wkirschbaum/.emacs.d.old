; Init --- Initial file for my emacs configuration
;; Version 27.0.50

;;; Commentary:
;; This is my personal configuration.

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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

(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (menu-bar-mode -1)))

(global-hl-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(winner-mode)
(electric-indent-mode +1)
(show-paren-mode 1)
(column-number-mode 1)

;; Do not jump when scrolling up or down
;; (setq scroll-step 1)
;; (setq scroll-conservatively 10000)
;; (setq auto-window-vscroll nil)

(setq-default auto-revert-verbose nil)
(global-auto-revert-mode t) ;; Ensure Dropbox files exists for this (org mode agenda)
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; Tell dired to update on change
(setq-default wdired-allow-to-change-permissions t) ;; Allow permission editing

;; Use hippie expand rather
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x C-r") 'find-file-read-only)

;; Packages

(straight-use-package 'use-package)

(use-package all-the-icons
  :straight t)

(use-package smex
  :straight t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; --- END Searching and navigation

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-dwim-target t))

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(use-package whitespace
  :straight t
  :config
  (add-hook 'prog-mode 'whitespace-mode)
  (setq whitespace-style '(face tabs tab-mark trailing empty)))

;; Source control
(use-package git-timemachine
  :straight t
  :defer t)

(use-package magit
  :straight t
  :demand t
  :bind ("C-x g" . magit-status))

(use-package forge
  :straight t)

(use-package diff-hl
  :straight t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; Searching

(use-package ag
  :straight t
  :commands (ag ag-regexp ag-project))

(use-package ivy
  :straight t
  :config
  (ivy-mode 1)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq magit-completing-read-function 'ivy-completing-read)
  (global-set-key (kbd "C-c r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "M-i") 'counsel-imenu)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package projectile
  :straight t
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-file-exists-remote-cache-expire nil
        projectile-sort-order 'recently-active)
  (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))

;; Completion

(use-package company
  :straight t
  :config
  (global-company-mode t))

;; Analysis
(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package flyspell
  :straight t
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;; erc
(use-package erc
  :straight t
  :config
  (setq-default erc-echo-notices-in-minibuffer-flag t)
  (setq erc-rename-buffers t)
  (setq erc-lurker-hide-list '("JOIN" "QUIT"))
  (setq erc-lurker-threshold-time 3600)
  (setq erc-input-line-position -2))

(setq auth-sources '((:source "~/Dropbox/secrets/.authinfo.gpg")))

(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-global-mode))

(use-package yasnippet-snippets :straight t)

(use-package define-word :straight t)

(use-package nov
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package restclient :straight t)

(use-package spotify
  :straight t
  :bind (("C-c s n" . spotify-next)
         ("C-c s p" . spotify-pause)
         ("C-c s c" . spotify-current))
  :config
  (spotify-enable-song-notifications))

(use-package dired-filter :straight t)

(use-package browse-at-remote :straight t)

(with-system darwin
  (setq-default ns-alternate-modifier 'super
                ns-command-modifier 'meta
                ns-option-modifier 'meta)
  (use-package exec-path-from-shell
    :straight t
    :config
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(use-package rainbow-mode :straight t)

(load "~/.emacs.d/programming.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/themes.el")
(load "~/.emacs.d/startup.el")

(load "~/.emacs.d/external/confluence-ox.el")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;; init.el ends here
