;; Things I can really do without, but makes emacs nicer
;; (use-package nyan-mode
;;   :ensure t
;;   :config
;;   (setq nyan-wavy-trail t
;;         nyan-bar-length 24)
;;   (nyan-mode))

;; (use-package keycast
;;   :ensure t)

(use-package focus
  :ensure t)

(use-package define-word
  :ensure t)

(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "{*}"
        minions-direct '(projectile-mode flycheck-mode))
  (minions-mode 1))

(use-package fireplace
  :ensure t)
