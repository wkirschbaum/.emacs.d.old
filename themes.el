; Themes --- All Theme related configuration

;;; Commentary:
;; This is my personal configuration.

;;; Code:
(progn
  (straight-use-package 'zenburn-theme)
  (straight-use-package 'monokai-theme)
  (straight-use-package 'solarized-theme)
  (straight-use-package 'dracula-theme)
  (straight-use-package 'spacemacs-theme))

;; (use-package nord-theme
;;   :straight t
;;   :config
;;   (setq nord-comment-brightness 2)
;;   (setq nord-region-highlight "snowstorm")
;;   (load-theme 'nord t)
;;   (set-frame-font "DejaVu Sans Mono 12" nil t))

;;; themes.el ends here
