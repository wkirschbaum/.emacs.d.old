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

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/themes.el")
(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/org.el")

(load "~/.emacs.d/external/confluence-ox.el")

(setq custom-file "~/.emacs.d/custom.el")

(if (file-exists-p custom-file)
    (load custom-file))

(load-theme 'spacemacs-dark)

;;; init.el ends here
