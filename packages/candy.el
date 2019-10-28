;; Things I can really do without, but makes emacs nicer
;; (use-package nyan-mode
;;   :straight t
;;   :config
;;   (setq nyan-wavy-trail t
;;         nyan-bar-length 24)
;;   (nyan-mode))

;; (use-package helpful
;;   :straight t
;;   :bind(("C-h f" . helpful-callable)
;;         ("C-h v" . helpful-variable)
;;         ("C-h k" . helpful-key)
;;         ("C-c C-d" . helpful-at-point)
;;         ("C-h C" . helpful-command)
;;         ("C-h F" . helpful-function))
;;   :config
;;   (setq counsel-describe-function-function #'helpful-callable
;;         counsel-describe-variable-function #'helpful-variable))

;; (use-package keycast
;;   :straight t)

;; (use-package focus
;;   :straight t)

;; (use-package define-word
;;   :straight t)

(use-package minions
  :straight t
  :config
  (setq minions-mode-line-lighter "{*}"
        minions-direct '(projectile-mode))
  (minions-mode 1))

(use-package dired-subtree
  :straight t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))
  )

(use-package all-the-icons-dired
  :straight t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
