; Core --- Core file for my emacs configuration

;;; Commentary:
;; This is my personal configuration.

;;; Code:


;; Keep custom file out of the init.el file
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

(setq auth-sources '((:source "~/Dropbox/secrets/.authinfo.gpg")))

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

(defun flash-mode-line ()
  "Flashes the mode-line."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil)
(setq ring-bell-function 'flash-mode-line)

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

(global-hl-line-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(winner-mode)
(electric-indent-mode +1)
(show-paren-mode 1)
(column-number-mode 1)

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

(setq-default frame-title-format '("%f [%m]"))
(setq-default display-line-numbers-width 4)
(setq-default display-line-numbers-widen t)
(setq-default indent-tabs-mode nil)

(save-place-mode 1)

;; Do not ask when following symbolic link
(setq vc-follow-symlinks nil)

;; Toggle relative line numbers
(whk/line-rel)

(add-hook 'emacs-startup-hook (lambda () (message (concat "Emacs started in" " " (emacs-init-time)))))

;;; core.el ends here
