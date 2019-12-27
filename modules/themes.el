; Themes --- All Theme related configuration

;;; Commentary:
;; This is my personal configuration.

;; (use-package zenburn-theme
;;   :ensure t)
;; (load-theme 'zenburn)

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(set-frame-font "Hack 11" nil t)

;;; Code:
;; (progn
;;   (straight-use-package 'zenburn-theme)
;;   (straight-use-package 'monokai-theme)
;;   (straight-use-package 'solarized-theme)
;;   (straight-use-package 'dracula-theme)
;;   (straight-use-package 'spacemacs-theme))

;; (use-package nord-theme
;;   :ensure t
;;   :config
;;   (setq nord-comment-brightness 2)
;;   (setq nord-region-highlight "snowstorm")
;;   (load-theme 'nord t)
;;   (set-frame-font "DejaVu Sans Mono 12" nil t))
;; (load-theme 'spacemacs-dark)

;; (set-frame-font "DejaVu Sans Mono 13" nil t)

;;; themes.el ends here
