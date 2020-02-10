(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq js-indent-level 2)
  (setq js2-include-node-externs t)
  (setq js2-global-externs '("describe" "test" "expect")))

(use-package indium
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'indium-interaction-mode))

(use-package jest
  :ensure t)

(use-package typescript-mode
  :ensure t
  :config
  (setq-default typescript-indent-level 2))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; Your face ends here
