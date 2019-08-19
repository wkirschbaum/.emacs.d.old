; Init --- Initial file for my emacs configuration

;;; Commentary:
;; This is my personal configuration.

;;; Code:

;; Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(with-system darwin
  (setq-default ns-alternate-modifier 'super
                ns-command-modifier 'meta
                ns-option-modifier 'meta))

(setq inhibit-startup-screen t)
(setq default-frame-alist '((fullscreen . maximized)))
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq-default display-time-default-load-average nil)
(display-time-mode t)

(defun flash-mode-line ()
  "Flashes the mode-line."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq visible-bell nil)
(setq ring-bell-function 'flash-mode-line)

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

(recentf-mode 1)
(setq-default recentf-max-menu-items 25)
(setq-default recentf-max-saved-items 25)

(setq-default auto-revert-verbose nil)
(global-auto-revert-mode t) ;; Ensure Dropbox files exists for this (org mode agenda)
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; Tell dired to update on change
(setq-default wdired-allow-to-change-permissions t) ;; Allow permission editing

;; Use hippie expand rather
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x C-r") 'find-file-read-only)

;; eshell
(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-filter-apply)

;; Stop and debug on error
;; (setq debug-on-error t)
;; (setq debug-on-quit t)

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

(straight-use-package 'use-package)

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

(load "~/.emacs.d/packages.el")
;; (load "~/.emacs.d/exwm.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/gnus.el")
(load "~/.emacs.d/external/confluence-ox.el")
(load "~/.emacs.d/themes.el")

;;; init.el ends here
