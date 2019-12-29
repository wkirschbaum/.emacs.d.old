(use-package jest
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode "\\.js\\|.ts\\'"
  :interpreter "node"
  :config
  (setq js-indent-level 2)
  (setq js2-include-node-externs t)
  (setq js2-global-externs '("describe" "test" "expect")))

(use-package indium
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'indium-interaction-mode))
