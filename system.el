;;; System -- My system settings

;;; Commentary:
;; This is my personal configuration.

;;; Code:

;; Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(if (display-graphic-p)
    (progn
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (setq use-dialog-box nil)
      (setq inhibit-startup-screen t)
      (setq default-frame-alist '((fullscreen . maximized)))
      (global-unset-key (kbd "C-z"))
      (global-unset-key (kbd "C-x C-z"))))

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

(global-hl-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(winner-mode)
(electric-indent-mode +1)
(show-paren-mode 1)
(column-number-mode 1)


(recentf-mode 1)
(setq-default recentf-max-menu-items 50)
(setq-default recentf-max-saved-items 50)

(save-place-mode 1)
(setq save-place-file "~/.emacs.d/saveplace")

(set-default 'cursor-in-non-selected-windows 'hollow)

(setq-default auto-revert-verbose nil)
(setq-default indicate-empty-lines t)
(global-auto-revert-mode t) ;; Ensure Dropbox files exists for this (org mode agenda)
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq-default wdired-allow-to-change-permissions t)
(setq-default wdired-create-parent-directories t)

;; Use hippie expand rather
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-x C-r") 'find-file-read-only)

;; eshell
(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-filter-apply)

(setq auth-sources '((:source "~/Dropbox/secrets/.authinfo.gpg")))

;;; system.el ends here
