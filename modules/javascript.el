(use-package jest
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq js-indent-level 2))

(use-package indium
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'indium-interaction-mode))
