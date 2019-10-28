;; Things I can really do without, but makes emacs nicer
;; (use-package nyan-mode
;;   :straight t
;;   :config
;;   (setq nyan-wavy-trail t
;;         nyan-bar-length 24)
;;   (nyan-mode))

;; (use-package keycast
;;   :straight t)

(use-package focus
  :straight t)

(use-package define-word
  :straight t)

(use-package minions
  :straight t
  :config
  (setq minions-mode-line-lighter "{*}"
        minions-direct '(projectile-mode))
  (minions-mode 1))
